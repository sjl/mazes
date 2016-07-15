(in-package #:mazes.utils)

(defmacro zap% (place function &rest arguments &environment env)
  "Update `place` by applying `function` to its current value and `arguments`.

  `arguments` should contain the symbol `%`, which is treated as a placeholder
  where the current value of the place will be substituted into the function
  call.

  For example:

  (zap% foo #'- % 10) => (setf foo (- foo 10)
  (zap% foo #'- 10 %) => (setf foo (- 10 foo)

  "
  ;; original idea/name from http://malisper.me/2015/09/29/zap/
  (assert (find '% arguments) ()
    "Placeholder % not included in zap macro form.")
  (multiple-value-bind (temps exprs stores store-expr access-expr)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores)
             (funcall ,function
                      ,@(substitute access-expr '% arguments))))
      ,store-expr)))


(defmacro in-context (&body body)
  `(prog1
    (push-matrix)
    (progn ,@body)
    (pop-matrix)))


(defun dividesp (n divisor)
  "Return whether `n` is evenly divisible by `divisor`."
  (zerop (mod n divisor)))


(defun random-elt (seq)
  (let ((length (length seq)))
    (if (zerop length)
      (values nil nil)
      (values (elt seq (random length)) t))))

(defmacro when-let ((symbol value) &body body)
  `(let ((,symbol ,value))
     (when ,symbol ,@body)))


(defun randomp ()
  (zerop (random 2)))


(defun full-list (&rest args)
  (remove-if #'null args))


(defun largest (list &key (key #'identity))
  (loop :for item :in list
        :when item :maximize (funcall key item)))


(defmacro recursively (bindings &body body)
  "Execute body recursively, like Clojure's `loop`/`recur`.

  `bindings` should contain a list of symbols and (optional) default values.

  In `body`, `recur` will be bound to the function for recurring.

  Example:

      (defun length (some-list)
        (recursively ((list some-list) (n 0))
          (if (null list)
            n
            (recur (cdr list) (1+ n)))))

  "
  (flet ((extract-var (binding)
           (if (atom binding) binding (first binding)))
         (extract-val (binding)
           (if (atom binding) nil (second binding))))
    `(labels ((recur ,(mapcar #'extract-var bindings)
                ,@body))
      (recur ,@(mapcar #'extract-val bindings)))))


(defun best (list predicate &key (key #'identity))
  (when list
    (flet ((reduce-keys (a b)
             (if (funcall predicate
                          (funcall key a)
                          (funcall key b))
               a
               b)))
      (reduce #'reduce-keys list))))


(defun smallest (list &key (key #'identity))
  (best list (lambda (a b)
               (when a
                 (or (null b)
                     (< a b))))
        :key key))

(defun largest (list &key (key #'identity))
  (best list
        (lambda (a b)
          (when a
            (or (null b)
                (> a b))))
        :key key))


(defun hash-keys (hash-table)
  (loop :for k :being :the hash-keys :of hash-table :collect k))


;;;; Sets
(defclass hash-set ()
  ((data :initarg :data)))


(defun make-set (&key (test #'eql) (initial-data nil))
  (let ((set (make-instance 'hash-set
                            :data (make-hash-table :test test))))
    (mapcar (curry #'set-add set) initial-data)
    set))


(defun set-contains-p (set value)
  (nth-value 1 (gethash value (slot-value set 'data))))

(defun set-empty-p (set)
  (zerop (hash-table-count (slot-value set 'data))))

(defun set-add (set value)
  (setf (gethash value (slot-value set 'data)) t)
  value)

(defun set-add-all (set seq)
  (map nil (curry #'set-add set) seq))

(defun set-remove (set value)
  (remhash value (slot-value set 'data))
  value)

(defun set-remove-all (set seq)
  (map nil (curry #'set-remove set) seq))

(defun set-clear (set)
  (clrhash (slot-value set 'data))
  set)

(defun set-random (set)
  (if (set-empty-p set)
    (values nil nil)
    (loop :with data = (slot-value set 'data)
          :with target = (random (hash-table-count data))
          :for i :from 0
          :for k :being :the :hash-keys :of data
          :when (= i target)
          :do (return (values k t)))))

(defun set-pop (set)
  (multiple-value-bind (val found) (set-random set)
    (if found
      (progn
        (set-remove set val)
        (values val t))
      (values nil nil))))


(defmethod print-object ((set hash-set) stream)
  (print-unreadable-object (set stream :type t)
    (format stream "~{~S~^ ~}"
            (hash-keys (slot-value set 'data)))))


;;;; Iterate
(defmacro-clause (AVERAGING expr &optional INTO var)
  (with-gensyms (count)
    (let ((average (or var (gensym "average"))))
      `(progn
        (for ,average
             :first ,expr
             ;; continuously recompute the running average instead of keeping
             ;; a running total to avoid bignums when possible
             :then (/ (+ (* ,average ,count)
                         ,expr)
                      (1+ ,count)))
        (for ,count :from 1)
        ,(when (null var)
           ;; todo handle this better
           `(finally (return ,average)))))))

(defmacro-clause (TIMING time-type &optional SINCE-START-INTO var PER-ITERATION-INTO per)
  (let ((timing-function (ecase time-type
                           ((real-time) #'get-internal-real-time)
                           ((run-time) #'get-internal-run-time)))
        (since (or var (gensym))))
    (with-gensyms (start-time current-time previous-time)
      `(progn
        (with ,start-time = (funcall ,timing-function))
        (for ,current-time = (funcall ,timing-function))
        (for ,previous-time :previous ,current-time :initially ,start-time)
        (for ,since = (- ,current-time ,start-time))
        ,(when per
           `(for ,per = (- ,current-time ,previous-time)))
        ,(when (and (null var) (null per))
           `(finally (return ,since)))))))


(defmacro-driver (FOR var IN-LISTS lists)
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (list)
      `(progn
        (generate ,list :in (remove nil (list ,@lists)))
        (,kwd ,var next (progn (when (null ,list)
                                 (next ,list))
                               (pop ,list)))))))


(defun seq-done-p (seq len idx)
  (if idx
    (= idx len)
    (null seq)))

(defmacro-driver (FOR var IN-SEQUENCES seqs)
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (seq len idx)
      `(progn
        (with ,len = nil)
        (with ,idx = nil)
        (generate ,seq :in (remove-if #'emptyp (list ,@seqs)))
        (,kwd ,var next
         (progn
           (when (seq-done-p ,seq ,len ,idx)
             (etypecase (next ,seq)
               (cons (setf ,len nil ,idx nil))
               (sequence (setf ,len (length ,seq)
                               ,idx 0))))
           (if ,idx
             (prog1 (elt ,seq ,idx)
               (incf ,idx))
             (pop ,seq))))))))

