(in-package #:mazes.generation)

(defmacro with-cell-active (cell &body body)
  (once-only (cell)
    `(prog2
      (setf (cell-active ,cell) t)
      (progn ,@body)
      (setf (cell-active ,cell) nil))))

(defun clear-active-group (cells)
  (loop :for c :in cells :do (setf (cell-active-group c) nil)))

(defun set-active-group (cells)
  (loop :for c :in cells :do (setf (cell-active-group c) t)))

(defun reset-active-group (old-cells new-cells)
  (clear-active-group old-cells)
  (set-active-group new-cells))


;;;; Binary Tree
;;; The Binary Tree generation algorithm works by looping through each cell,
;;; choosing either the north or east neighbor at random, and carving out the
;;; wall to it.
;;;
;;; For the north and east edges of the map the only viable neighbor is always
;;; picked, which results in signature long corridors on those edges.

(defgenerator binary-tree-generator (grid)
  (grid-loop-cells cell grid
    (with-cell-active cell
      (let ((other (random-elt (full-list (cell-north cell)
                                          (cell-east cell)))))
        (when other
          (cell-link cell other)))
      (yield))))

(defun binary-tree (grid)
  (do-generator (_ (binary-tree-generator grid)))
  grid)


;;;; Sidewinder
;;; The Sidewinder algorithm works by looping over each row.
;;;
;;; For each row it loops over the cells to form "runs" of consecutive
;;; horizontal neighbors, randomly deciding when to end a particular run.  Once
;;; a run is ended ("closed") it picks a random cell in the run and carves out
;;; a passage north.
;;;
;;; For the top row of the map we never want to carve out to the north wall, so
;;; we never allow the top row to close -- it'll always be a single run.  This
;;; results in a signature long corridor along the top of the maze.

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
          (with-cell-active cell
            (setf (cell-active-group cell) t)
            (push cell run)
            (if should-close
              (let* ((member (random-elt run))
                     (member-north (cell-north member)))
                (when member-north
                  (setf (cell-active member) t)
                  (cell-link member member-north))
                (yield)
                (setf (cell-active member) nil)
                (clear-active-group run)
                (setf run nil))
              (progn
                (cell-link cell (cell-east cell))
                (yield)))))))

(defun sidewinder (grid)
  (do-generator (_ (sidewinder-generator grid)))
  grid)


;;;; Aldous-Broder
;;; The Aldous-Broder algorithm picks a random cell and walks to a random
;;; neighbor at each step.  If that neighbor has not been visited yet it carves
;;; out the wall back to the previous cell.
;;;
;;; This produces really nice, unbiased mazes but is *really* slow.

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


;;;; Wilson
;;;

(defgenerator wilson-generator (grid)
  (let ((unvisited (make-set :initial-data (grid-map-cells #'identity grid))))
    (setf (cell-active-group (set-pop unvisited)) t) ; random initial target
    (loop :with path = nil
          :with cell = (set-random unvisited)
          :while cell :do
          (with-cell-active cell
            (let ((path-loop (member cell path)))
              (setf path (cons cell path))
              (cond
                ;; If we've made a loop, trim it off.
                (path-loop
                 (reset-active-group path path-loop)
                 (setf path path-loop
                       cell (cell-random-neighbor cell)))

                ;; If we've hit a visited cell, carve out the path.
                ((not (set-contains-p unvisited cell))
                 (mapc (curry #'apply #'cell-link)
                       (n-grams 2 path))
                 (set-remove-all unvisited path)
                 (setf path nil
                       cell (set-random unvisited)))

                ;; Otherwise keep going
                (t
                 (setf (cell-active-group cell) t)
                 (setf cell (cell-random-neighbor cell)))))
            (yield))))
  (grid-clear-active grid))

(defun wilson (grid)
  (do-generator (_ (wilson-generator grid)))
  grid)
