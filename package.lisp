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
  (:export
    #:cell
    #:make-cell
    #:cell-link
    #:cell-unlink
    #:cell-linked-p
    #:cell-linked-north-p
    #:cell-linked-south-p
    #:cell-linked-east-p
    #:cell-linked-west-p
    #:cell-neighbors
    #:grid
    #:grid-ref
    #:make-grid
    #:grid-size
    #:grid-map-cells
    #:grid-map-rows
    #:grid-size
    #:grid-random-cell))

(defpackage #:mazes.generation
  (:use
    #:cl
    #:mazes.quickutils
    #:mazes.utils
    #:mazes.grid))

(defpackage #:mazes.demo
  (:use
    #:cl
    #:sketch
    #:mazes.grid
    #:mazes.quickutils
    #:mazes.utils
    #:mazes.fps))

