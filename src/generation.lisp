(in-package #:mazes.generation)

(defmacro with-cell-active (cell-place &body body)
  `(prog2
     (setf (cell-active ,cell-place) t)
     (progn ,@body)
     (setf (cell-active ,cell-place) nil)))


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
  (do-generator (_ (binary-tree-generator grid)))
  grid)


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
  (do-generator (_ (sidewinder-generator grid)))
  grid)


(defgenerator aldous-broder-generator (grid)
  (let ((cell (grid-random-cell grid))
        (unvisited (1- (grid-size grid))))
    (while (plusp unvisited)
      (setf (cell-active-group cell) t)
      (let ((neighbor (random-elt (cell-neighbors cell))))
        (with-cell-active cell
          (when (null (cell-links neighbor))
            (cell-link cell neighbor)
            (decf unvisited))
          (yield))
        (setf cell neighbor))))
  (grid-clear-active grid))

(defun aldous-broder (grid)
  (do-generator (_ (aldous-broder-generator grid)))
  grid)
