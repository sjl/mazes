(defpackage #:mazes.utils
  (:use
    #:cl
    #:sketch
    #:mazes.quickutils)
  (:export
    #:dividesp
    #:in-context
    #:random-elt
    #:randomp
    #:zap%
    #:full-list
    #:smallest
    #:largest
    #:when-let
    #:recursively
    #:recur
    #:hash-keys
    #:make-set
    #:set-contains-p
    #:set-add
    #:set-remove
    #:set-add-all
    #:set-remove-all
    #:set-random
    #:set-pop
    #:set-empty-p
    #:hash-set
    #:set-clear
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
    #:cell-random-neighbor
    #:cell-north
    #:cell-south
    #:cell-east
    #:cell-west
    #:cell-col
    #:cell-row
    #:cell-active
    #:cell-active-group
    #:cell-links
    #:grid
    #:grid-cols
    #:grid-rows
    #:grid-cells
    #:grid-ref
    #:grid-clear-active
    #:make-grid
    #:grid-size
    #:grid-map-cells
    #:grid-map-rows
    #:grid-loop-cells
    #:grid-loop-rows
    #:grid-size
    #:grid-random-cell
    #:distance-map
    #:make-dm
    #:dm-distance
    #:dm-cells
    #:dm-max
    #:cell-distance-map
    #:dijkstra))

(defpackage #:mazes.generation
  (:use
    #:cl
    #:mazes.quickutils
    #:mazes.utils
    #:mazes.grid)
  (:export
    #:binary-tree
    #:binary-tree-generator
    #:sidewinder
    #:sidewinder-generator
    #:aldous-broder
    #:aldous-broder-generator
    #:wilson
    #:wilson-generator)
  (:import-from #:snakes
    #:defgenerator
    #:do-generator
    #:yield))

(defpackage #:mazes.demo
  (:use
    #:cl
    #:sketch
    #:cl-arrows
    #:mazes.grid
    #:mazes.generation
    #:mazes.quickutils
    #:mazes.utils
    #:mazes.fps)
  (:import-from #:snakes
    #:do-generator))

