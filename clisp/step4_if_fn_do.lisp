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
     (malify ;; list of string to lisp structure
      (quotify ;; handle quotes etc.
       (tokenize ;; lexing
	(string-trim '(#\Space #\Tab) input))))))

(defun scar (s) (subseq s 0 1))

(defun is-whitespace (c)
  (member c (coerce '(#\space #\tab #\,) 'list)))

(defun is-whitespaces (s)
  (is-whitespace (char s 0)))

(defvar *parens* '("(" ")"))
(defvar *quotes* '("'" "`" "~" "~@"))
(defvar *tokens* (append *parens* *quotes*))
(defvar *keywords* '("nil" "true" "false"))

;; TODO: move this
(defun specialp (name)
  "List over special forms."
  (member name '(|def!| |let*| |if| |fn*| |do|)))


(defparameter *env* (make-hash-table :test #'equal))

;; Mal type structure, with misc functions
(defstruct mal type value)
(defun malp (e) (eq (type-of e) 'mal))

(defun to-mal (val)
  "Take a value, and fix the correct type."
  (if (malp val)
      val
      (make-mal :type (cond ((stringp val) 'string)
			    ((numberp val) 'number)
			    ((and (listp val)
				  (not (null val))) 'list)
			    ((or (eq 't val) (eq nil val)) 'boolean)
			    ((symbolp val) 'symbol)
			    ((functionp val) 'function)
			    (t (format t "`to-mal` was given ~S~%" val)))
		:value val)))

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

(defun is-paren (str)
  (member str *parens* :test #'equal))

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
	       (and (equal a "\"")
		    (equal z "\""))))
	   (is-number (tok) ;; TODO: add support for double/floats
	     (parse-integer tok :junk-allowed t))
	   (is-boolean (tok)
	     (or (equal tok "false")
		 (equal tok "true"))))
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
		    ((is-boolean head)
		     (setq head (make-mal
				 :type 'boolean
				 :value (equal head "true"))))
		    (t
		     (setq head (make-mal
				 :type 'symbol
				 :value (intern head)))))
	      (cons head (malify tail)))
	    ;; somewhat hacky ?
	    (first (malify (list tokens)))))))


(defun mal-funcall (env f args)
  "Evaluate all arguments, then call the functions with 
   the evaluated args."
  (labels ((fix-mal (e)
	     (if (malp e)
		 e
		 (to-mal e))))
    (let ((eval-args (mapcar #'(lambda (a) (-eval (fix-mal a) env)) args)))
      (to-mal (apply f eval-args)))))

(defun mal-eval-list (input env)
  (let* ((mal-func (first input))
	 (func (mal-value mal-func))
	 (f (gethash func env)))
    (if f
	(if (specialp func)
	    (apply f env (rest input))
	    (mal-funcall env f (rest input)))
	(format t "ERR: Symbol ~S is not a function.~%" func))))

(defun -eval (input &optional (env *env*))
  (if (listp input)
      (mapcar #'(lambda (e) (-eval e env)) input)
      (let ((type (mal-type input))
	    (val (mal-value input)))
	(case type
	  (list (mal-eval-list val env))
	  (symbol (to-mal (gethash val env)))
	  (otherwise input)))))

(defun -eval-inner (input &optional (env *env*))
  (let ((type (mal-type input))
	(val (mal-value input)))
    (if (eq type 'list)
	(let* ((new-val (mal-eval-list val env))
	       (type (cond ((numberp new-val) 'number)
			   ((stringp new-val) 'string)
			   ((listp new-val) 'list)
			   (t 'symbol))))
	  
	  (make-mal :type type
		    :value new-val))
	input)))

(defun newline () (format t "~%"))


(defun show (elem)
  "Take a mal value type, and return its string representation.
   Contrary to 'lisp style' printing, this does not need to 
   be reversable read into the same representation"
  (if (malp elem)
      (let ((type (mal-type elem))
     	    (val (mal-value elem)))
	(case type
     	  (list   (if val
		      (mapcar #'show val)
		      "()"))
	  (number (write-to-string val))
	  (string val)
	  (symbol (symbol-name val))
	  (boolean (if val "true" "false"))
	  (otherwise val)))
      ;; in case we're passing LISP data, show as is.
      (if (not elem)
	  "nil"
	  elem)))
  
	

(defun -print (input &optional nl)
  (princ (if (and input (listp input))
	     (mapcar #'show input)
	     (show input)))
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
      (if (eq (mal-value (car in)) '|quit|)
	  (return))
      (mapcar #'(lambda (e)
		  (-print (-eval e) t)) in))))

(load "../clisp/functions.lisp")
;;(load "clisp/functions.lisp")

(declaim (optimize (debug 3)))
(rep)
