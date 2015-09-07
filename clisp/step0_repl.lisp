(defun -read ()
  (read-line *standard-input* nil nil))

(defun -eval (input)
  input)

(defun newline () (format t "~%"))

(defun -print (string &optional nl)
  (princ string)
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
      (-print (-eval str) t))))
(rep)

;; (defun main ()
;;   (sb-ext:save-lisp-and-die "clisp" :toplevel #'rep :executable t))
;; (main)
