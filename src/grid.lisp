(in-package #:mazes.grid)

;;;; Cells
(defclass cell ()
  ((row :initarg :row :reader cell-row)
   (col :initarg :col :reader cell-col)
   (north :accessor cell-north :initform nil)
   (south :accessor cell-south :initform nil)
   (east :accessor cell-east :initform nil)
   (west :accessor cell-west :initform nil)
   (active :accessor cell-active :initform nil)
   (active-group :accessor cell-active-group :initform nil)
   (links :accessor cell-links :initform nil)))


(defun make-cell (row col)
  (make-instance 'cell :row row :col col))


(defun cell-link% (cell other)
  (pushnew other (cell-links cell)))

(defun cell-link (cell1 cell2)
  (cell-link% cell1 cell2)
  (cell-link% cell2 cell1))

(defun cell-unlink% (cell other)
  (zap% (cell-links cell)
        #'remove other %))

(defun cell-unlink (cell1 cell2)
  (cell-unlink% cell1 cell2)
  (cell-unlink% cell2 cell1))


(defun cell-linked-p (cell other)
  (member other (cell-links cell)))

(defun cell-linked-north-p (cell)
  (cell-linked-p cell (cell-north cell)))

(defun cell-linked-south-p (cell)
  (cell-linked-p cell (cell-south cell)))

(defun cell-linked-east-p (cell)
  (cell-linked-p cell (cell-east cell)))

(defun cell-linked-west-p (cell)
  (cell-linked-p cell (cell-west cell)))


(defun cell-neighbors (cell)
  (with-slots (north south east west) cell
    (full-list north south east west)))

(defun cell-random-neighbor (cell)
  (random-elt (cell-neighbors cell)))

(defun cell-random-unlinked-neighbor (cell)
  (random-elt (set-difference (cell-neighbors cell)
                              (cell-links cell))))

(defun cell-random-linked-neighbor (cell)
  (random-elt (cell-links cell)))


(defmethod print-object ((cell cell) stream)
  (print-unreadable-object (cell stream :type t :identity nil)
    (format stream "(~d, ~d)"
            (cell-row cell)
            (cell-col cell))))


;;;; Grid
(defclass grid ()
  ((rows :initarg :rows :reader grid-rows)
   (cols :initarg :cols :reader grid-cols)
   (cells :accessor grid-cells)))


(defgeneric grid-prepare (grid))
(defgeneric grid-configure-cells (grid))
(defgeneric grid-ref (grid row col))


(defun make-grid (rows cols)
  (let ((grid (make-instance 'grid :rows rows :cols cols)))
    (grid-prepare grid)
    (grid-configure-cells grid)
    grid))


(defun grid-row (grid row)
  (let ((cells (grid-cells grid)))
    (make-array (grid-cols grid)
      :element-type 'cell
      :displaced-to cells
      :displaced-index-offset (array-row-major-index cells row 0))))


(defclause-sequence IN-GRID nil
  :access-fn (lambda (grid index)
               (row-major-aref (grid-cells grid) index))
  :size-fn (lambda (grid)
             (array-total-size (grid-cells grid)))
  :sequence-type 'grid
  :element-type 'cell
  :element-doc-string "All cells in a grid")

(defclause-sequence ROW-OF-GRID nil
  :access-fn #'grid-row
  :size-fn (lambda (grid)
             (array-dimension (grid-cells grid) 0))
  :sequence-type 'grid
  :element-type '(vector cell)
  :element-doc-string "All rows in a grid")


(defun grid-size (grid)
  (* (grid-rows grid) (grid-cols grid)))

(defun grid-random-cell (grid)
  (aref (grid-cells grid)
        (random (grid-rows grid))
        (random (grid-cols grid))))


(defmethod grid-ref ((grid grid) row col)
  (with-slots (rows cols cells) grid
    (if (and (<= 0 row (1- rows))
             (<= 0 col (1- cols)))
      (aref cells row col)
      nil)))

(defmethod grid-prepare ((grid grid))
  (with-slots (rows cols cells) grid
    (setf cells
          (make-array (list rows cols)
            :element-type 'cell
            :initial-contents
            (loop :for r :from 0 :below rows :collect
                  (loop :for c :from 0 :below cols :collect
                        (make-cell r c)))))))

(defmethod grid-configure-cells ((grid grid))
  (iterate
    (for cell :in-grid grid)
    (with-slots (row col north south east west) cell
      (setf north (grid-ref grid (1- row) col)
            south (grid-ref grid (1+ row) col)
            west (grid-ref grid row (1- col))
            east (grid-ref grid row (1+ col))))))


(defun grid-clear-active (grid)
  (iterate (for cell :in-grid grid)
           (setf (cell-active cell) nil
                 (cell-active-group cell) nil)))


(defmethod print-object ((grid grid) stream)
  (print-unreadable-object
      (grid stream :type t :identity nil)
    (format stream "~%+~A~%"
            (cl-strings:repeat "---+" (grid-cols grid)))
    (iterate
      (for row :row-of-grid grid)
      (let ((top "|")
            (bottom "+"))
        (loop :for contents :across row
              :for cell = (or contents (make-cell -1 -1))
              :for cell-top = (if (cell-linked-east-p cell) "    " "   |")
              :for cell-bot = (if (cell-linked-south-p cell) "   +" "---+")
              :do (setf top (cl-strings:insert cell-top top)
                        bottom (cl-strings:insert cell-bot bottom)))
        (format stream "~A~%~A~%" top bottom)))))


;;;; Distance Map
(defclass distance-map ()
  ((root :initarg :root :accessor dm-root)
   (distances :initarg :distances :accessor dm-distances)))


(defun make-dm (root)
  (let ((dm (make-instance 'distance-map
                           :root root
                           :distances (make-hash-table))))
    (setf (gethash root (dm-distances dm)) 0)
    dm))


(defun dm-distance (dm cell)
  (gethash cell (dm-distances dm)))

(defun (setf dm-distance) (new-value dm cell)
  (setf (gethash cell (dm-distances dm)) new-value))

(defun dm-cells (dm)
  (hash-keys (dm-distances dm)))

(defun dm-max (dm)
  (largest (dm-cells dm) :key (curry #'dm-distance dm)))


(defun cell-distance-map (cell)
  (loop :with dm = (make-dm cell)
        :for frontier = (list cell)
        :then (loop :for cell :in frontier
                    :for dist = (dm-distance dm cell)
                    :append
                    (loop :for linked :in (cell-links cell)
                          :when (not (dm-distance dm linked))
                          :collect
                          (progn
                            (setf (dm-distance dm linked) (1+ dist))
                            linked)))
        :while frontier
        :finally (return dm)))


;;;; Path Finding
(defun dijkstra (distances target)
  (let ((root (dm-root distances))
        (dist (curry #'dm-distance distances)))
    (recursively ((cell target) path)
      (cond
        ((not cell) nil) ; maze is fucked
        ((eql cell root) (cons root path)) ; done
        (t (recur (smallest (cell-links cell) :key dist) ; loop
                  (cons cell path)))))))



