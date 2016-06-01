(in-package #:mazes.demo)

;;;; Config
(setf *bypass-cache* t)

(defparameter *width* 800)
(defparameter *height* 800)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))
(defparameter *maze-size* 700)


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
    (with-centered-coords
      ,@body)))

(defun cell-size (grid)
  (truncate (/ *maze-size*
               (max (grid-cols grid)
                    (grid-rows grid)))))


;;;; Sketch
(defparameter *wall-pen*
  (make-pen :weight 3 :stroke (rgb 0.625 0.423 0.399)))

(defun draw-maze (grid distances)
  (let ((cell-size (cell-size grid)))
    (in-context
      (translate (/ (* (grid-cols grid) cell-size) -2)
                 (/ (* (grid-rows grid) cell-size) -2))
      (with-pen *wall-pen*
        (with-font (make-font :color (rgb 0.314 0.235 0.325)
                              :size 20)
          (grid-loop-cells cell grid
            (let ((x1 (* cell-size (cell-col cell)))
                  (y1 (* cell-size (cell-row cell)))
                  (x2 (* cell-size (1+ (cell-col cell))))
                  (y2 (* cell-size (1+ (cell-row cell)))))
              (when distances
                (text (princ-to-string (dm-distance distances cell))
                      (+ 5 x1) (+ 0 y1)))
              (when (not (cell-north cell))
                (line x1 y1 x2 y1))
              (when (not (cell-west cell))
                (line x1 y1 x1 y2))
              (when (not (cell-linked-east-p cell))
                (line x2 y1 x2 y2))
              (when (not (cell-linked-south-p cell))
                (line x1 y2 x2 y2)))))))))

(defsketch demo
    ((width *width*) (height *height*) (y-axis :down) (title "Mazes")
     (mouse (cons 0 0))
     (frame 0)
     (log " ")
     ;; Variables
     (grid (make-grid 10 10))
     (gen (sidewinder-generator grid))
     (distances nil)
     ;; Pens
     (log-font (make-font :color (gray 0.8)))
     )
  (with-setup
    ;;
    (draw-maze grid distances)
    (if (dividesp frame 1)
      (funcall gen))
    ;;
    (with-font log-font
      (text log
            (- *center-x*)
            (- *center-y* 22)))
    (incf frame)
    ))


;;;; Mouse
(defun cell-clicked (instance x y)
  ;; assume a square grid for now...
  (with-slots (log grid) instance
    (let* ((cell-size (cell-size grid))
           (offset (/ (- *width* *maze-size*) 2))
           (x (- x offset))
           (y (- y offset)))
      (if (and (< -1 x *maze-size*)
               (< -1 y *maze-size*))
        (values (truncate (/ y cell-size))
                (truncate (/ x cell-size)))
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
  (multiple-value-bind (row col) (cell-clicked instance x y)
    (when row
      (with-slots (distances grid) instance
        (setf distances
              (cell-distance-map (grid-ref grid row col)))))))

(defun mousedown-right (instance x y)
  (declare (ignorable instance x y))
  )

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


(defun keydown (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space (sketch::prepare instance))
    (:scancode-lshift (setf *shift* t))
    (:scancode-lctrl (setf *control* t))
    (:scancode-lgui (setf *command* t))
    (:scancode-lalt (setf *option* t))
    ;;
    ;;
    ))

(defun keyup (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-lshift (setf *shift* nil))
    (:scancode-lctrl (setf *control* nil))
    (:scancode-lgui (setf *command* nil))
    (:scancode-lalt (setf *option* nil))
    (:scancode-space nil)))


(defmethod kit.sdl2:keyboard-event
    ((instance demo) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Run
; (defparameter *demo* (make-instance 'demo))
