(in-package #:mazes.demo)


;;;; Config
(setf *bypass-cache* t)

(defparameter *width* 800)
(defparameter *height* 800)

(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))


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


;;;; Sketch
(defsketch demo
    ((width *width*) (height *height*) (y-axis :up) (title "Mazes")
     (mouse (cons 0 0))
     ;; Variables
     ;; Pens
     (simple-pen (make-pen :fill (gray 0.1)))
     (line-pen (make-pen :stroke (gray 0.1) :weight 1))
     )
  (with-setup
    ;;
    ;;
    ))


;;;; Mouse
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
  )

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
(defparameter g (make-grid 7 5))
(gen-binary-tree g)

; (defparameter *demo* (make-instance 'demo))
