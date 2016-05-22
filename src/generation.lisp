(in-package #:mazes.generation)

(defgenerator binary-tree-generator (grid)
  (grid-loop-cells cell grid
    (let ((other (random-elt (full-list (cell-north cell)
                                        (cell-east cell)))))
      (when other
        (cell-link cell other)))
    (yield)))

(defun binary-tree (grid)
  (do-generator (_ (binary-tree-generator grid))))
