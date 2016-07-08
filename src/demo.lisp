(in-package #:mazes.demo)

;;;; Config
(setf *bypass-cache* t)
(defparameter *ui-font* nil)


(defparameter *width* 800)
(defparameter *height* 800)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))
(defparameter *maze-size* 700)
(defvar *generator* 'sidewinder-generator)
(defvar *instant* nil)
(defvar *show-longest* nil)
(defvar *show-stats* nil)
(defvar *show-colors* nil)
(defvar *cell-size* nil)


;;;; Globals
(defvar *shift* nil)
(defvar *control* nil)
(defvar *command* nil)
(defvar *option* nil)


;;;; Utils
(defmacro with-centered-coords (&body body)
  `(in-context
     (translate *center-x* *center-y*)
     ,@body))

(defmacro with-setup (&body body)
  `(with-fps
    (background (gray 0.1))
    (with-font *ui-font*
      (with-centered-coords
        ,@body))))

(defun cell-size (grid)
  (truncate (/ *maze-size*
               (max (grid-cols grid)
                    (grid-rows grid)))))


;;;; Sketch
(defparameter *wall-pen*
  (make-pen :weight 3 :stroke (rgb 0.625 0.423 0.399)))

(defparameter *path-pen*
  (make-pen :fill (rgb 0.831 0.537 0.416)))

(defparameter *longest-pen*
  (make-pen :fill (rgb 0.314 0.235 0.325)))

(defparameter *end-pen*
  (make-pen :fill (rgb 1.000 0.733 0.424)))

(defparameter *active-pen*
  (make-pen :fill (rgb 1.000 0.273 0.476)))

(defparameter *active-group-pen*
  (make-pen :fill (rgb 1.000 0.512 0.580)))


(defun cell-x (cell &optional (offset 0))
  (* *cell-size* (+ offset (cell-col cell))))

(defun cell-y (cell &optional (offset 0))
  (* *cell-size* (+ offset (cell-row cell))))

(defun draw-cell (cell)
  (rect (cell-x cell) (cell-y cell) *cell-size* *cell-size*))


(defun draw-colors (instance)
  (when *show-colors*
    (let* ((grid (slot-value instance 'grid))
           (distances (cell-distance-map
                        (or (slot-value instance 'start)
                            (grid-ref grid 0 0))))
           (max (dm-distance distances (dm-max distances))))
      (when (plusp max)
        (iterate
          (for cell :in-grid grid)
          (when-let (distance (dm-distance distances cell))
            (with-pen
                (make-pen :fill (lerp-color
                                  (rgb 0.149 0.141 0.212)
                                  (rgb 0.570 0.429 0.591)
                                  (/ distance max)))
              (draw-cell cell))))))))

(defun draw-longest (instance)
  (when *show-longest*
    (with-pen *longest-pen*
      (map nil #'draw-cell (slot-value instance 'longest-path)))))

(defun draw-path (instance)
  (with-slots (start end path) instance
    (with-pen *path-pen*
      (map nil #'draw-cell path))
    (with-pen *end-pen*
      (when start (draw-cell start))
      (when end (draw-cell end)))))

(defun draw-active (instance)
  (iterate (for cell :in-grid (slot-value instance 'grid))
           (when (cell-active-group cell)
             (with-pen *active-group-pen* (draw-cell cell)))
           (when (cell-active cell)
             (with-pen *active-pen* (draw-cell cell)))))

(defun draw-stats (instance)
  (when *show-stats*
    (with-slots (grid) instance
      (text (format nil "dead-ends: ~D"
                    (grid-dead-end-count grid))
            150 0))))


(defun draw-maze (instance)
  (with-slots (finished-generating grid) instance
    (when finished-generating
      (in-context
        (translate (- *center-x*) (- *center-y*))
        (draw-stats instance)))
    (in-context
      (translate (/ (* (grid-cols grid) *cell-size*) -2)
                 (/ (* (grid-rows grid) *cell-size*) -2))
      (when finished-generating
        (draw-colors instance)
        (draw-longest instance)
        (draw-path instance))
      (draw-active instance)
      (with-pen *wall-pen*
        (iterate
          (for cell :in-grid grid)
          (let ((x1 (cell-x cell))
                (y1 (cell-y cell))
                (x2 (cell-x cell 1))
                (y2 (cell-y cell 1)))
            (when (not (cell-north cell))
              (line x1 y1 x2 y1))
            (when (not (cell-west cell))
              (line x1 y1 x1 y2))
            (when (not (cell-linked-east-p cell))
              (line x2 y1 x2 y2))
            (when (not (cell-linked-south-p cell))
              (line x1 y2 x2 y2))))))))


(defun find-longest-path (grid)
  (let ((distances (-> (grid-ref grid 0 0)
                     cell-distance-map
                     dm-max
                     cell-distance-map)))
    (dijkstra distances (dm-max distances))))

(defsketch demo
    ((width *width*) (height *height*) (y-axis :down) (title "Mazes")
     (mouse (cons 0 0))
     (frame 0)
     (log " ")
     ;; Variables
     (grid (make-grid 20 20))
     (gen (funcall *generator* grid))
     (finished-generating nil)
     (distances nil)
     (path nil)
     (longest-path nil)
     (start nil)
     (end nil)
     )
  ;; Setup
  (setf *cell-size* (cell-size grid))
  (when (null *ui-font*)
    (setf *ui-font* (make-font :color (gray 0.8) :size 14)))
  (with-setup
    ;; Maze
    (when (and (not finished-generating)
               (dividesp frame 3))
      (when *instant*
        (iterate (while (not (funcall gen)))))
      (when (funcall gen)
        (setf finished-generating t
              longest-path (find-longest-path grid))))
    (draw-maze sketch::instance)
    ;; UI
    (with-font *ui-font*
      (text "algorithm: [a]ldous-broder [b]inary tree [h]unt and kill [r]ecursive backtracker [s]idewinder [w]ilson"
            (+ (- *center-x*) 5) (- *center-y* 44))
      (text "display: [C]olor distances [I]nstant generation [L]ongest path [S]tats"
            (+ (- *center-x*) 5) (- *center-y* 24)))
    ;;
    (incf frame)
    ))


;;;; Mouse
(defun cell-clicked (instance x y)
  ;; assume a square grid for now...
  (with-slots (log grid) instance
    (let* ((offset (/ (- *width* *maze-size*) 2))
           (x (- x offset))
           (y (- y offset)))
      (if (and (< -1 x *maze-size*)
               (< -1 y *maze-size*))
        (values (truncate (/ y *cell-size*))
                (truncate (/ x *cell-size*)))
        (values nil nil)))))


(defun mousemove (instance x y)
  (with-slots (mouse) instance
    (setf (car mouse) x)
    (setf (cdr mouse) y)
    ;;
    ;;
    )
  )

(defun mousedown-left (instance x y)
  (declare (ignorable instance x y))
  (with-slots (end grid distances path finished-generating) instance
    (when finished-generating
      (multiple-value-bind (row col) (cell-clicked instance x y)
        (when (and row col distances)
          (setf end
                (grid-ref grid row col)
                path
                (dijkstra distances end)))))))

(defun mousedown-right (instance x y)
  (declare (ignorable instance x y))
  (with-slots (start distances grid end path finished-generating) instance
    (when finished-generating
      (multiple-value-bind (row col) (cell-clicked instance x y)
        (when row
          (setf distances
                (cell-distance-map (grid-ref grid row col))
                start
                (grid-ref grid row col)
                end nil
                path nil
                ))))))

(defun mouseup-left (instance x y)
  (declare (ignorable instance x y))
  )

(defun mouseup-right (instance x y)
  (declare (ignorable instance x y))
  )


(defmethod kit.sdl2:mousemotion-event ((window demo) ts b x y xrel yrel)
  (declare (ignore ts b xrel yrel))
  (mousemove window x y))

(defmethod kit.sdl2:mousebutton-event ((window demo) state ts button x y)
  (declare (ignore ts))
  (funcall (case state
             (:mousebuttondown
              (case button
                (1 #'mousedown-left)
                (3 #'mousedown-right)))
             (:mousebuttonup
              (case button
                (1 #'mouseup-left)
                (3 #'mouseup-right))))
           window x y))


;;;; Keyboard
(defmacro scancode-case (scancode-form &rest pairs)
  (with-gensyms (scancode)
    `(let ((,scancode ,scancode-form))
      (cond
        ,@(mapcar (lambda (pair)
                    (destructuring-bind (key-scancode &rest body) pair
                      `((sdl2:scancode= ,scancode ,key-scancode)
                        ,@body)))
           pairs)))))

(defmacro toggle (place)
  `(zap% ,place #'not %))


(defun keydown (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space (sketch::prepare instance))
    (:scancode-lshift (setf *shift* t))
    (:scancode-rshift (setf *shift* t))
    (:scancode-lctrl (setf *control* t))
    (:scancode-rctrl (setf *control* t))
    (:scancode-lgui (setf *command* t))
    (:scancode-rgui (setf *command* t))
    (:scancode-lalt (setf *option* t))
    (:scancode-ralt (setf *option* t))
    ;;
    (:scancode-s (if *shift*
                   (toggle *show-stats*)
                   (setf *generator* 'sidewinder-generator)))
    (:scancode-b (setf *generator* 'binary-tree-generator))
    (:scancode-a (setf *generator* 'aldous-broder-generator))
    (:scancode-w (setf *generator* 'wilson-generator))
    (:scancode-h (setf *generator* 'hunt-and-kill-generator))
    (:scancode-r (setf *generator* 'recursive-backtracker-generator))
    (:scancode-l (if *shift*
                   (toggle *show-longest*)
                   nil))
    (:scancode-c (if *shift*
                   (toggle *show-colors*)
                   nil))
    (:scancode-i (if *shift*
                   (toggle *instant*)
                   nil))
    ;;
    ))

(defun keyup (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-lshift (setf *shift* nil))
    (:scancode-rshift (setf *shift* nil))
    (:scancode-lctrl (setf *control* nil))
    (:scancode-rctrl (setf *control* nil))
    (:scancode-lgui (setf *command* nil))
    (:scancode-rgui (setf *command* nil))
    (:scancode-lalt (setf *option* nil))
    (:scancode-ralt (setf *option* nil))
    (:scancode-space nil)))


(defmethod kit.sdl2:keyboard-event
    ((instance demo) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Statistics
(defun run-stats (&key (iterations 100) (size 15))
  (iterate
    (for algorithm :in '(binary-tree sidewinder aldous-broder wilson
                         hunt-and-kill recursive-backtracker))
    (iterate
      (repeat iterations)
      (for grid = (make-grid size size))
      (funcall algorithm grid)
      (timing run-time
              :since-start-into time-total
              :per-iteration-into time-single)
      (averaging (grid-dead-end-count grid) :into dead-ends)
      (averaging time-single :into time-average)
      (finally
        (format t "~A (~D by ~:*~D)~%" algorithm size)
        (format t "Dead Ends:        ~10,2F (~4,1F%)~%"
                dead-ends
                (* 100 (/ dead-ends (* size size))))
        (format t "Average Run Time: ~10,2Fms~%"
                (/ time-average internal-time-units-per-second 1/1000))
        (format t "Total Run Time:   ~10,2Fms~%"
                (/ time-total internal-time-units-per-second 1/1000))
        (format t "~%")
        (finish-output)))))


;;;; Run
; (run-stats)
; (defparameter *demo* (make-instance 'demo))
