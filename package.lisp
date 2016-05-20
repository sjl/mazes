(defpackage #:mazes.utils
  (:use
    #:cl
    #:sketch
    #:mazes.quickutils)
  (:export
    #:dividesp
    #:in-context
    #:zap%
    #:%))

(defpackage #:mazes.fps
  (:use
    #:cl
    #:sketch
    #:mazes.quickutils
    #:mazes.utils)
  (:export
    #:with-fps
    #:draw-fps))

(defpackage #:mazes.grid
  (:use
    #:cl
    #:mazes.quickutils
    #:mazes.utils)
  (:export))

(defpackage #:mazes.demo
  (:use
    #:cl
    #:sketch
    #:mazes.grid
    #:mazes.quickutils
    #:mazes.utils
    #:mazes.fps))

