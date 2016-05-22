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

(defgenerator sidewinder-generator (grid)
  (grid-loop-rows row grid
    (loop :with run = nil
          :for cell :across row
          :for at-east-bound = (null (cell-east cell))
          :for at-north-bound = (null (cell-north cell))
          :for should-close = (or at-east-bound
                                  (and (not at-north-bound)
                                       (randomp)))
          :do (progn
                (push cell run)
                (if should-close
                  (let* ((member (random-elt run))
                         (member-north (cell-north member)))
                    (when member-north
                      (cell-link member member-north))
                    (setf run nil))
                  (cell-link cell (cell-east cell)))
                (yield)))))

(defun sidewinder (grid)
  (do-generator (_ (sidewinder-generator grid))))
