(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cl-ppcre))

;; Notes on the data:
;; The Mal expression
;;    (+ 1 2)
;; is, under the hood
;;    (('function '+) ('number 1) ('number 2))
;; where the (a, b) tuples are `(make-mal :type a :value b)`
;; The expresion
;;    (+ (* 2 3) 4)
;; is
;;    (('function '+) ('list (('function '*) ('number 2) ('number 3))) ('number 2)) 

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
(defvar *keywords* '("nil" "true" "false"))

(defparameter *env* (make-hash-table :test #'equal))

;; Mal type structure, with misc functions
(defstruct mal type value)
(defun malp (e) (eq (type-of e) 'mal))


(defvar scanner
  (cl-ppcre:create-scanner
   "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)"))

(defun tokenize (str)
  "Take a mal expression as a string, and return a list of 
   mal tokens"
  (let ((tokens (cl-ppcre:all-matches-as-strings scanner str)))
    (remove-if #'(lambda (s) (equal "" s))
	       (mapcar #'(lambda (s) (string-trim " " s)) tokens))))
      
	      
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
  (cond ((equal quote "'") "quote")
	((equal quote "`") "quasiquote")
	((equal quote "~") "unquote")
	((equal quote "~@") "splice-unquote")
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


(defun malify (tokens)
  "Transforms a list of tokens into the fomat specified
   above."
  (labels ((split-string (toks acc &optional (paren-level 0))
	     "Split a list of tokens, and return the first
              list as first element, and the rest of the tokens
              as the second element"
	     (if toks
		 (let ((head (first toks))
		       (tail (rest toks)))
		   (cond ((equal head "(")
			  (setq paren-level (1+ paren-level)))
			 ((equal head ")")
			  (setq paren-level (1- paren-level))))
		   (if (= 0 paren-level)
		       (values (cons head acc) tail)
		       (split-string tail (cons head acc) paren-level)))))
	   (is-string (tok)
	     (let ((a (subseq tok 0 1))
		   (z (subseq (reverse tok) 0 1)))
	       (and (equal a #\")
		    (equal z #\"))))
	   (is-number (tok) ;; TODO: add support for double/floats
	     (parse-integer tok :junk-allowed t))
	   (do-inner (elems func)
	     (print elems)
	     (labels ((inner (es f)
		      	(if (null (cdr es))
			    es
			    (cons (funcall f (first es))
				  (inner (rest es) f)))))
	       (cons (first elems) (inner (rest elems) func))))
	   )
    (if tokens
	(if (listp tokens)
	    (let ((head (first tokens))
		  (tail (rest tokens)))
	      (cond ((equal head "(")
		     (multiple-value-bind (lst rst) (split-string tokens nil)
		       (let ((req (malify (rest (butlast (reverse lst))))))
			 (setq head (make-mal
				     :type 'list
				     :value req)))
		       (setq tail rst)))
		    ((is-string head)
		     (setq head (make-mal
				 :type 'string
				 :value head)))
		    ((is-number head)
		     (setq head (make-mal
				 :type 'number
				 :value (is-number head))))
		    (t
		     (setq head (make-mal
				 :type 'symbol
				 :value (intern head))))
		    )
	      (cons head (malify tail)))
	    ;; somewhat hacky ?
	    (first (malify (list tokens)))))))


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
	       (let ((str (is-string input)))
		 (if (member str *keywords* :test #'equal)
		     (intern str)
		     str)))
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
  "Take a mal value type, and return its string representation.
   Contrary to 'lisp style' printing, this does not need to 
   be reversable read into the same representation"
  (let ((type (mal-type elem))
	(val (mal-value elem)))
    (case type
	(('list)   (mapcar #'show val))
	(('number) (write-to-string val))
	(('string) val)
	(('symbol) (symbol-name val))
	(otherwise elem))))
	

(defun -print (input &optional nl)
  (princ (cond ((malp input)  (show input))
	       ((listp input) (mapcar #'show input))
	       (t (format t "~S ~S~%" 'ERR (type-of input)))))
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
      (if (or (not in) (equal (car in) "quit"))
	  (return))
      (mapcar #'(lambda (e)
		  (-print (-eval e) t)) in))))


(defun special-p (name)
  "List over special forms."
  (member name
	  '("def!" "let*" "if" "fn*" "do")
	  :test #'equal))

;; FUNCTIONS

;; Math functions
(setf (gethash "+" *env*) #'(lambda (a b) (+ a b)))
(setf (gethash "-" *env*) #'(lambda (a b) (- a b)))
(setf (gethash "*" *env*) #'(lambda (a b) (* a b)))
(setf (gethash "/" *env*) #'(lambda (a b) (/ a b)))
;; List functions
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
(setf (gethash "let*" *env*) #'-let*)

(declaim (optimize (debug 3)))
(rep)
