(in-package #:mazes.generation)

(defun gen-binary-tree (grid)
  (grid-loop-cells cell grid
    (let ((other (random-elt (full-list (cell-north cell)
                                        (cell-east cell)))))
      (when other
        (cell-link cell other)))))
