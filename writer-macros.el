(defmacro defun-js (name args &rest forms)
  `(defun
       ,(intern (concat "js/" (stringify name)))
       ,args
     (let* ((real-args (mapcar 'js-eval args))
            (f (lambda ,args ,@forms)))
     (apply f real-args))))

(defmacro defmacro-js (name args &rest forms)
  `(defun
       ,(intern (concat "js/" (stringify name)))
       ,args
     ,@forms))

(defun js-eval-last-sexpr ()
  (interactive)
  (backward-kill-sexp)
  (insert (js-eval (read (current-kill 0)))))

(defun js-eval (obj)
  (cond
    ((eql (type-of obj) 'string) (concat "'" (stringify obj) "'"))
    ((eql (type-of obj) 'cons)   (js-eval-sexpr obj))
    (t                           (stringify obj))))

(defun js-eval-sexpr (sexpr)
  (let ((func (intern (concat "js/" (symbol-name (car sexpr))))))
    (apply func (cdr sexpr))))

(defun stringify (a) (format "%s" a))

(defun infix (op args)
  (concat "("
	  (mapconcat
	   (lambda (n) (stringify n))
	   args
	   (concat " " op " "))
	  ")"))

(defun regfunc (name args)
  (concat name "("
	  (mapconcat 'stringify args ", ")
	  ")"))

(defun str-join (arr sep)
  (mapconcat 'stringify arr sep))

(defun-js + (&rest args) (infix "+" args))
(defun-js * (&rest args) (infix "*" args))
(defun-js log (&rest args) (regfunc "console.log" args))
(defun-js require (&rest args)
  (mapconcat
   (lambda (sym) (concat "var " sym " = require('" sym "')"))
   (mapcar 'stringify args)
   "\n"))

(defmacro-js fn (args &rest body)
  ;; TODO: cool (format ...) string for this?
  (concat
   "function (" (str-join args ", ") ") {\n"
   (str-join (mapcar (lambda (sexp) (concat "  " (js-eval sexp) "\n")) body) "")
   "}"))

;;--------------------------------------------------------------------------------

(+ 1 (* 2 3) "greetings!" 3)

(* 2 3)

"heya!"

14

(log "value of x" x)

(require 'hyperlog 'hypercore 'kappa-core)

(fn (e) (log "e" e))
