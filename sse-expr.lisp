
(defparameter *constant-index* 0)

(defstruct vector-constant value (index (incf *constant-index*) :type fixnum))

(defparameter *sse-binops*
  '((+ . "_mm_add_ps")
    (- . "_mm_sub_ps")
    (* . "_mm_mul_ps")
    (/ . "_mm_div_ps")
    (sqrt . "_mm_sqrt_ps")
    (and . "_mm_and_ps")
    (or . "_mm_or_ps")
    (min . "_mm_min_ps")
    (max . "_mm_max_ps")))

(defun sse-expr (expr*)
  (let ((constants nil))
    (labels ((iter (expr)
	       (cond
		 ((symbolp expr) expr)
		 ((numberp expr) (car (pushnew (make-vector-constant :value expr)
					  constants
					  :key #'vector-constant-value)))
		 ((listp expr) (transform-expr (car expr) (cdr expr)))
		 ((vector-constant-p expr) expr)
		 (t (error (format nil "unsupported datum: ~a" expr)))))
	     (transform-expr (symbol values)
	       (case (length values)
		 (1 (error "only binary ops supported"))
		 (2 (transform-binary symbol (iter (car values)) (iter (cadr values))))
		 (t (transform-binary symbol
				      (iter (car values))
				      (transform-expr symbol (cdr values))))))
	     (transform-binary (symbol l r)
	       (list (cdr (assoc symbol *sse-binops*)) l r))
	     (emit (tree)
	       (cond
		 ((symbolp tree) (princ tree))
		 ((stringp tree) (princ tree))
		 ((listp tree)
		  (format t "~a(" (car tree))
		  (loop for v on (cdr tree)
		       do (progn
			    (emit (car v))
			    (when (cdr v) (princ ", "))))
		  (format t ")"))
		 ((vector-constant-p tree) (format t "vconst~d" (vector-constant-index tree)))
		 (t (error (format nil "bad datum: ~a" tree))))))
      (let ((result-tree (iter expr*)))
	(loop for c in constants do
	     (let ((idx (vector-constant-index c))
		   (val (vector-constant-value c)))
	       (format t "const __m128 vconst~d = { ~f~:*, ~f~:*, ~f~:*, ~f };~%" idx val)))
	(princ "return ")
	(emit result-tree)
	(princ ";"))))
  (values))
