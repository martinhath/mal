
(defun -read ()
  (let ((input (read-line *standard-input* nil nil)))
    (typify
     (lispify ;; list of string to lisp structure
      (quotify ;; handle quotes etc.
       (tokenize ;; lexing
	(string-trim '(#\Space #\Tab) input)))))))

(defun scar (s) (subseq s 0 1))

(defun is-whitespace (c)
  (member c (coerce '(#\space #\tab #\,) 'list)))

(defun is-whitespaces (s)
  (is-whitespace (char s 0)))

(defvar *parens* '("(" ")"))
(defvar *quotes* '("'" "`" "~" "~@"))
(defvar *tokens* (append *parens* *quotes*))

(defparameter *env* (make-hash-table :test #'equal))

	      
(defun group-n (list n)
  (if list
      (if (> n (length list))
	   `(,list)
	   (cons (subseq list 0 n) (group-n (subseq list n) n)))))

(defun make-dotted (lst)
  (mapcar
   #'(lambda (e) `(,(first e) . ,(second e)))
   lst))

(defun copy-env (env)
  (let ((new-env (make-hash-table
		  :test (hash-table-test env)
		  :size (hash-table-size env))))
    (maphash #'(lambda (key value)
		 (setf (gethash key new-env) value))
	     env)
    new-env))

(defun -let* (vals body)
  (let ((args (make-dotted (group-n vals 2)))
	(func (gethash "def!" *env*))
	(env (copy-env *env*)))
    (mapcar #'(lambda (kv)
		(let ((k (car kv))
		      (v (cdr kv)))
		  (funcall func k (-eval v env) env)))
	    args)
    (-eval body env)))

(defun is-paren (str)
  (member str *parens* :test #'equal))

(defun lisp-format (tokens)
  (if (listp tokens)
    (let ((head (car tokens))
	  (rest (cdr tokens))
	  (peek (cadr tokens)))
	(if rest
	    ;; Check if we should _skip_ printing space
	    (if (or (equal "(" head)
		    (and (equal ")" head)
			 (equal ")" peek))
		    (equal ")" peek)
		    (equal "'" head))
		(concatenate 'string head (lisp-format rest))
		(concatenate 'string head " " (lisp-format rest)))
	    head))
    tokens))

(defun string-take (str n)
  (if (>= n (length str))
      str
      (subseq str 0 n)))

(defun string-drop (str n)
  (if (>= n (length str))
      "" 
      (subseq str n)))

(defun read-inner-string (input)
  (let ((head (char input 0)))
    (if (eq head #\")
	(let ((pos (position #\" (subseq input 1))))
	  (subseq input 1 (+ 1 pos)))
	(princ "read-inner-string was passed something weird."))))

(defun tokenize (input)
  (labels ((inner (input acc)
	     (if (not (equal "" input))
		 (let ((f (scar input))
		       (rest (subseq input 1)))
		   (if (equal f "~")
		       (if (and rest (equal (subseq rest 0 1) "@"))
			   (block nil
			     (setq f "~@")
			     (setq rest (subseq rest 1)))))
		   (cond
		     ((member f *tokens* :test #'equal)
		      (append (list acc f) (inner rest nil)))
		     ((equal f "\"")
		      (let* ((str (read-inner-string input))
			     (rst (subseq input (+ 2 (length str)))))
			(cons str (inner rst nil))))
		     ((is-whitespaces f)
		      (if acc 
			  (cons acc (inner rest nil))
			  (inner rest nil)))
		     (t
		      (inner rest (concatenate 'string acc f)))))
		 (list acc))))
    (remove-if #'null (inner input nil))))

(defun next-stmt (tokens)
  (if (is-paren (first tokens))
      (let ((level 1)
	    (len 0)
	    (tks (rest tokens)))
	(loop
	  (let ((h (first tks)))
	    (cond
		((equal "(" h)
		 (setq level (1+ level)))
		((equal ")" h)
		 (setq level (1- level))))
	    (setq len (1+ len))
	    (when (or (eq level 0)
		      (not tks))
	      (return))
	    (setq tks (rest tks))))
	(subseq tokens 0 (+ 1 len)))
      (first tokens)))
	  

(defun quotep (quote)
  (member quote *quotes* :test #'equal))

(defun quote-label (quote)
  (cond ((equal quote "'")
	 "quote")
	((equal quote "`")
	 "quasiquote")
	((equal quote "~")
	 "unquote")
	((equal quote "~@")
	 "splice-unquote")
	(t (error "wtf"))))

(defun quotify (tokens)
  (if tokens
    (let ((h (first tokens))
	  (tail (rest tokens)))
      (if (quotep h)
	  (let ((quoted (next-stmt tail))
		(q-label (quote-label h)))
	    (if (listp quoted)
		(append (list "(" q-label)
			quoted
			'(")")
			(quotify (subseq tokens (+ 1 (length quoted)))))
		(append (list "("  q-label quoted ")")
			(quotify (if tail
				     (rest tail)
				     nil)))))
	  (block nil
	      (cons h (quotify tail)))))))

(defun cutoff (l)
  (butlast (cdr l)))

(defun lispify (tokens)
  (if tokens
      (if (listp tokens)
	  (let ((head (first tokens))
		(tail (rest tokens)))
	    (if (equal "(" head)
	   	(let* ((inside (cutoff (next-stmt tokens)))
	   	       (in-len (length inside)))
	   	  (cons (lispify inside)
			(lispify (subseq tail (1+ in-len)))))
	   	(cons head (lispify tail))))
	  tokens)))

(defun typify (input)
  (labels ((is-number (n)
	     (parse-integer n :junk-allowed t))
	   (is-string (s)
	     (and (> (length s) 2)
		  (equal "\"" (subseq s 2))
		  (cutoff (cutoff s))))
	   )
    (if (listp input)
	(mapcar #'typify input)
	(cond ((is-number input)
	       (is-number input))
	      ((is-string input)
	       (is-string input))
	      (t input)))))


(defun -eval (input &optional (env *env*))
  (cond ((listp input)
	 (let* ((head (-eval (first input))))
	   (if (functionp head)
	       (apply head
		      (if (special-p (first input))
			  (rest input)
			  (mapcar #'(lambda (e) (-eval e env)) (rest input))))
	       input)))
	((stringp input)
	 (let ((lookup (gethash input env)))
	   (if lookup
	       lookup
	       input)))
	(t input)))

(defun newline () (format t "~%"))


(defun show (elem)
  (cond ((listp elem)
	 (mapcar #'show elem))
	((numberp elem)
	 (write-to-string elem))
	((stringp elem)
	 (concatenate 'string "\"" elem "\""))
	(t
	 elem)))
      

(defun -print (input &optional nl)
  (princ (cond ((listp input)
		(mapcar #'show input))
	       (t
		(show input))))
  (when nl
    (newline))
  (force-output))

(defun rep ()
  (let ((prompt "user> ")
	(in 1))
    (do ()
	((null in))
      (princ prompt)
      (force-output)
      (setq in (-read))
      (unless in
	  (return))
      (mapcar #'(lambda (e)
		  (-print (-eval e) t)) in))))


(defun special-p (name)
  (member name
	  '("def!" "let*")
	  :test #'equal))

;; FUNCTIONS
(setf (gethash "+" *env*) #'(lambda (a b) (+ a b)))
(setf (gethash "-" *env*) #'(lambda (a b) (- a b)))
(setf (gethash "*" *env*) #'(lambda (a b) (* a b)))
(setf (gethash "/" *env*) #'(lambda (a b) (/ a b)))
(setf (gethash "def!" *env*)
      #'(lambda (a b &optional (env *env*))
	  (let ((e (-eval b)))
	    (setf (gethash a env) e)
	    e)))
(setf (gethash "let*" *env*) #'-let*)

(declaim (optimize (debug 3)))
(rep)
