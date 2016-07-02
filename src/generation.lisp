(in-package #:mazes.generation)

(defmacro with-cell-active ((cell &rest options) &body body)
  (once-only (cell)
    `(prog2
      (progn (setf (cell-active ,cell) t)
             ,(when (member :mark-group options)
                `(setf (cell-active-group ,cell) t)))
      (progn ,@body)
      (setf (cell-active ,cell) nil))))

(defun clear-active-group (cells)
  (iterate (for c :in cells)
           (setf (cell-active-group c) nil)))

(defun set-active-group (cells)
  (iterate (for c :in cells)
           (setf (cell-active-group c) t)))

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
  (iterate
    (for cell :in-grid grid)
    (for other = (random-elt (full-list (cell-north cell)
                                        (cell-east cell))))
    (with-cell-active (cell)
      (when other
        (cell-link cell other))
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
  (iterate
    (for row :row-of-grid grid)
    (iterate
      (with run = nil)
      (for cell :in-vector row)
      (for at-east-bound = (null (cell-east cell)))
      (for at-north-bound = (null (cell-north cell)))
      (for should-close = (or at-east-bound
                              (and (not at-north-bound)
                                   (randomp))))
      (with-cell-active (cell :mark-group)
        (push cell run)
        (if should-close
          (let* ((member (random-elt run))
                 (member-north (cell-north member)))
            (when member-north
              (with-cell-active (member-north)
                (cell-link member member-north)
                (yield)))
            (yield)
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
    (iterate (while (plusp unvisited))
             (setf (cell-active-group cell) t)
             (let ((neighbor (random-elt (cell-neighbors cell))))
               (with-cell-active (cell)
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
;;; Wilson's algorithm works by initializing one random cell to be visited, then
;;; starting at some *other* random cell and walking randomly.  Once the path
;;; hits the visited cell, it is linked together and another random unvisited
;;; cell is chosed.
;;;
;;; If a loop in the path is formed before the path manages to hit a visited
;;; cell, the loop is "erased" and the walk restarts from the looping point.

(defgenerator wilson-generator (grid)
  (let ((unvisited (make-set :initial-data (iterate (for cell :in-grid grid)
                                                    (collect cell)))))
    (setf (cell-active-group (set-pop unvisited)) t)
    (iterate
      (with path = nil)
      (with cell = (set-random unvisited))
      (while cell)
      (with-cell-active (cell :mark-group)
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
             (setf cell (cell-random-neighbor cell)))))
        (yield))))
  (grid-clear-active grid))

(defun wilson (grid)
  (do-generator (_ (wilson-generator grid)))
  grid)


;;;; Hunt and Kill
;;;

(defgenerator hunt-and-kill-generator (grid)
  (labels ((visited-p (cell)
             (not (null (cell-links cell))))
           (random-visited-neighbor (cell)
             (random-elt (remove-if-not #'visited-p (cell-neighbors cell))))
           (random-unvisited-neighbor (cell)
             (random-elt (remove-if #'visited-p (cell-neighbors cell))))
           (hunt ()
             (iterate
               (for cell :in-grid grid)
               (finding cell :such-that
                        (and (not (visited-p cell))
                             (some #'visited-p (cell-neighbors cell)))))))
    (iterate
      (with cell = (grid-ref grid 0 0))
      (initially (setf (cell-active-group cell) t))
      (for next = (random-unvisited-neighbor cell))
      (if next
        (with-cell-active (next :mark-group)
          (cell-link cell next)
          (setf cell next)
          (yield))
        (let ((new-cell (hunt)))
          (if (null new-cell)
            (finish)
            (with-cell-active (new-cell :mark-group)
              (cell-link new-cell (random-visited-neighbor new-cell))
              (setf cell new-cell)
              (yield)))))))
  (grid-clear-active grid))

(defun hunt-and-kill (grid)
  (do-generator (_ (hunt-and-kill-generator grid)))
  grid)
