(defun register-fun (name fun)
  (setf (gethash name *env*) fun))

;; Math functions
(defmacro make-math (op)
  "Avoid typing by creating a simple wrapper for the math functions."
  `(register-fun ',op #'(lambda (&rest rest) (apply #',op (mapcar #'mal-value rest)))))

(make-math +)
(make-math -)
(make-math *)
(make-math /)
(make-math <)
(make-math >)
(make-math >=)
(make-math <=)

(defmacro defmal (name args &body body )
  ;; TODO: use (gensym) instead of "mal-"?
  (let ((mal-name (intern (concatenate 'string "mal-" (symbol-name name))))
	(mal-norm (intern (string-downcase name))))
    ;`(register-fun ',mal-norm (defun ,mal-name ,args ,@body))))
    `(progn
       (defun ,mal-name ,args ,@body)
       (register-fun ',mal-norm #',mal-name))))

;; Math functions (which are different from CL)
(defmal = (a b)
  (let ((av (mal-value a))
	(bv (mal-value b))
	(at (mal-type a))
	(bt (mal-type b)))
    (and (eq at bt)
	 (equal av bv))))

;; List functions
(defmal list (&rest args)
  (make-mal :type 'list
	    :value args))

(defmal list? (arg)
  (eq 'list (mal-type arg)))

(defmal empty? (arg)
  (and (malp arg)
       (eq (mal-type arg) 'list)
       (null (mal-value arg))))

(defmal count (lst)
  (length (mal-value lst)))

;; Special form functions
;; ALL FUNCTIONS need to take the env as first argument.
(defmal if (env &rest args)
  (let ((pred (-eval (first args)))
	(snd (second args)))
    (if (and (eq (mal-type pred) 'boolean)
	     (not (mal-value pred)))
	(-eval (third args) env)
	(if snd
	    (-eval snd env)
	    (to-mal nil)))))
  
(setf (gethash "def!" *env*)
      #'(lambda (a b &optional (env *env*))
	  (let ((e (-eval b)))
	    (setf (gethash a env) e)
	    e)))
(setf (gethash 'quote *env*) #'identity)
