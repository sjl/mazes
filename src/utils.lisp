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
