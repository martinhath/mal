(defun -read ()
  (let ((input (read-line *standard-input* nil nil)))
    (string-trim '(#\Space #\Tab) input)))

(defun scar (s) (string (char s 0)))

(defun is-whitespace (c)
  (member c (coerce '(#\space #\tab #\,) 'list)))

(defun is-whitespaces (s)
  (is-whitespace (char s 0)))

(defvar *tokens*
  '("(" ")" "'"))

(defun is-paren (str)
  (member str '("(" ")") :test #'equal))

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

(defun read-car-to (input &optional escape)
  "Returns the string beginning with (car input), and ending
   in (car input). If escape, \\(car input) should not match.
   (read-car-to \"abd 123\\agakek 12 5112 aa a\" t) -> \"abd 123\\aga\""
  (let ((chr (scar input))
	(rest (subseq input 1)))
    (labels ((inner (input)
	       (unless (equal "" input)
		(let ((h (scar input))
		      (rest (subseq input 1)))
		    (cond ((and (equal h "\"") escape)
			    (concatenate 'string (string-take input 2) (inner (string-drop input 2))))
			((equal h chr)
			    h)
			(t (concatenate 'string h (inner rest))))))))
     (concatenate 'string chr (inner rest)))))

(defun tokenize (input)
  (labels ((inner (input acc)
	     (if (not (equal "" input))
		 (let ((f (scar input))
		       (rest (subseq input 1)))
		   (cond
		     ((member f *tokens* :test #'equal)
		      (append (list acc f) (inner rest nil)))
		     ((equal f "\"")
		      (let* ((str (read-car-to input t))
			     (rst (subseq input (length str))))
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
	  

(defun quotify (tokens)
  (if tokens
    (let ((h (first tokens))
	  (tail (rest tokens)))
      (if (equal "'" h)
	  (let ((quoted (next-stmt tail)))
	    (if (listp quoted)
		(append (list "(" "quote")
			quoted
			'(")")
			(quotify (subseq tokens (+ 1 (length quoted)))))
		(append (list "(" "quote" quoted ")")
			(quotify (if tail
				     (rest tail)
				     nil)))))
	  (block nil
	      (cons h (quotify tail)))))))


(defun -eval (input)
  (let ((tokens (tokenize input)))
    (quotify tokens)))

(defun newline () (format t "~%"))

(defun -print (input &optional nl)
  (princ (lisp-format input))
  (when nl
    (newline))
  (force-output))

(defun rep ()
  (let ((prompt "user> ")
	(str ""))
    (do ()
	((null str))
      (-print prompt)
      (setq str (-read))
      (if (not str)
	  (return))
      (-print (-eval str) t))))

(declaim (optimize (debug 3)))
(rep)
