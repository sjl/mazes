(in-package #:mazes.generation)

(defgenerator binary-tree-generator (grid)
  (grid-loop-cells cell grid
    (setf (cell-active cell) t)
    (let ((other (random-elt (full-list (cell-north cell)
                                        (cell-east cell)))))
      (when other
        (cell-link cell other)))
    (yield)
    (setf (cell-active cell) nil)))

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
          :do
          (progn
            (setf (cell-active-group cell) t
                  (cell-active cell) t)
            (push cell run)
            (if should-close
              (let* ((member (random-elt run))
                     (member-north (cell-north member)))
                (when member-north
                  (setf (cell-active member) t)
                  (cell-link member member-north))
                (yield)
                (setf (cell-active member) nil)
                (loop :for c :in run :do (setf (cell-active-group c) nil))
                (setf run nil))
              (progn
                (cell-link cell (cell-east cell))
                (yield)))
            (setf (cell-active cell) nil)))))

(defun sidewinder (grid)
  (do-generator (_ (sidewinder-generator grid))))
