(defpackage #:mazes.utils
  (:use
    #:cl
    #:sketch
    #:mazes.quickutils)
  (:export
    #:dividesp
    #:in-context
    #:random-elt
    #:zap%
    #:full-list
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
    #:cell-north
    #:cell-south
    #:cell-east
    #:cell-west
    #:grid
    #:grid-ref
    #:make-grid
    #:grid-size
    #:grid-map-cells
    #:grid-map-rows
    #:grid-loop-cells
    #:grid-loop-rows
    #:grid-size
    #:grid-random-cell))

(defpackage #:mazes.generation
  (:use
    #:cl
    #:mazes.quickutils
    #:mazes.utils
    #:mazes.grid)
  (:export
    #:gen-binary-tree))

(defpackage #:mazes.demo
  (:use
    #:cl
    #:sketch
    #:mazes.grid
    #:mazes.generation
    #:mazes.quickutils
    #:mazes.utils
    #:mazes.fps))

