(defun register-fun (name fun)
  (setf (gethash name *env*) fun))

(defun special-p (name)
  "List over special forms."
  (member name
	  '("def!" "let*" "if" "fn*" "do")
	  :test #'equal))

;; FUNCTIONS

;; Math functions
(defmacro make-math (fun)
  `(,fun #'(lambda (env &rest args)
	    (declare (ignore env)) (apply #',fun args))))


`(register-fun @,(make-math '+))


(register-fun '+ #'(lambda (env &rest args)
		     (declare (ignore env)) (apply #'+ args)))
;;(setf (gethash '+ *env*) #'+)
(setf (gethash '- *env*) #'(lambda (env a b) (- a b)))
(setf (gethash '* *env*) #'(lambda (env a b) (* a b)))
(setf (gethash '/ *env*) #'(lambda (env a b) (/ a b)))

;; List functions
(defun mal-list (env &rest args)
  (make-mal :type 'list
	    :value (mapcar #'(lambda (e) (-eval e env)) args)))
(register-fun '|list| #'mal-list)


	      
(setf (gethash "list" *env*)
      #'(lambda (&rest rest)
	  ;;(if 
	  (list rest)))

;; Special form functions
(setf (gethash "def!" *env*)
      #'(lambda (a b &optional (env *env*))
	  (let ((e (-eval b)))
	    (setf (gethash a env) e)
	    e)))
(setf (gethash 'quote *env*) #'identity)
