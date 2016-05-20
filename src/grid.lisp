(in-package #:mazes.grid)

;;;; Cells
(defclass cell ()
  ((row :initarg :row :reader cell-row)
   (col :initarg :col :reader cell-col)
   (north :accessor cell-north :initform nil)
   (south :accessor cell-south :initform nil)
   (east :accessor cell-east :initform nil)
   (west :accessor cell-west :initform nil)
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

(defun cell-neighbors (cell)
  (with-slots (north south east west) cell
    (remove-if #'null (list north south east west))))


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


(defun grid-map-cells (fn grid)
  (with-slots (cells) grid
    (loop :for i :from 0 :below (array-total-size cells)
          :do (funcall fn (row-major-aref cells i)))))

(defun grid-map-rows (fn grid)
  (with-slots (rows cols cells) grid
    (loop :for row :from 0 :below rows
          :do (funcall fn (make-array cols
                            :element-type 'cell
                            :displaced-to cells
                            :displaced-index-offset
                            (array-row-major-index cells row 0))))))


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
  (grid-map-cells
    (lambda (cell)
      (with-slots (row col north south east west) cell
        (setf north (grid-ref grid (1- row) col)
              south (grid-ref grid (1+ row) col)
              west (grid-ref grid row (1- col))
              east (grid-ref grid row (1+ col)))))
    (grid-cells grid)))
