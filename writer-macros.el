(define-minor-mode wmac-mode
  "Enable noffle's javascript writer macros."
  :lighter " WriterMacros"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<S-return>") 'wmac--eval-last-sexpr-maybe-and-newline)
            map))

(defmacro defun-js (name args &rest forms)
  `(defun
       ,(intern (concat "js/" (wmac--stringify name)))
       ,args
     (let* ((real-args (mapcar 'wmac--eval args))
            (f (lambda ,args ,@forms)))
     (apply f real-args))))

(defmacro defmacro-js (name args &rest forms)
  `(defun
       ,(intern (concat "js/" (wmac--stringify name)))
       ,args
     ,@forms))

(defun wmac--eval-last-sexpr-maybe-and-newline ()
  (interactive)
  (let ((start (save-excursion (progn (backward-sexp) (point)))))
    (insert (wmac--eval-last-sexpr))
    (indent-region start (point))))

(defun wmac--eval-last-sexpr ()
  (interactive)
  (backward-kill-sexp)
  (wmac--eval (read (current-kill 0))))

(defun wmac--eval (obj)
  (cond
    ((eql (type-of obj) 'string) (concat "'" (wmac--stringify obj) "'"))
    ((eql (type-of obj) 'cons)   (wmac--eval-sexpr obj))
    (t                           (wmac--stringify obj))))

(defun wmac--eval-sexpr (sexpr)
  (let ((func (intern-soft (concat "js/" (symbol-name (car sexpr))))))
    (if func
        (apply func (cdr sexpr))
      (let ((eval-args (mapcar 'wmac--eval (cdr sexpr))))
        (wmac--regfunc (wmac--stringify (car sexpr)) eval-args)))))

(defun wmac--stringify (a) (format "%s" a))

(defun wmac--infix (op args)
  (concat "("
	  (mapconcat
	   (lambda (n) (wmac--stringify n))
	   args
	   (concat " " op " "))
	  ")"))

(defun wmac--regfunc (name args)
  (concat name "("
	  (mapconcat 'wmac--stringify args ", ")
	  ")"))

(defun wmac--str-join (arr sep)
  (mapconcat 'wmac--stringify arr sep))

(defun-js or (&rest args) (wmac--infix "||" args))
(defun-js and (&rest args) (wmac--infix "&&" args))
(defun-js not (&rest args) (concat "!" (car args)))
(defun-js + (&rest args) (wmac--infix "+" args))
(defun-js * (&rest args) (wmac--infix "*" args))
(defun-js log (&rest args) (wmac--regfunc "console.log" args))
(defun-js require (&rest args)
  (mapconcat
   (lambda (sym) (concat "var " sym " = require('" sym "')"))
   (mapcar 'wmac--stringify args)
   "\n"))

(defmacro-js set (var value)
  (format "%s = %s" var (wmac--eval value)))

(defmacro-js fn (args &rest body)
  ;; TODO: cool (format ...) string for this?
  (concat
   "function (" (wmac--str-join args ", ") ") {\n"
   (wmac--str-join (mapcar (lambda (sexp) (concat "  " (wmac--eval sexp) "\n")) body) "")
   "}"))

(defmacro-js var (name value)
  (concat "var " (wmac--stringify name) " = " (wmac--eval value)))

(defmacro-js if (condition a b)
  (concat "if (" (wmac--eval condition) ") {\n"
          "  " (wmac--eval a) "\n} else {\n"
          "  " (wmac--eval b) "\n}"))

(defmacro-js let (vars &rest body)
  (concat
   (wmac--str-join
    (mapcar
     (lambda (pair) (concat "var " (wmac--eval (car pair)) " = " (wmac--eval (cadr pair)) "\n"))
     vars)
    "")
   (wmac--str-join
    (mapcar
     'wmac--eval
     body)
    "\n")))

(defmacro-js proto (class fname args &rest body)
  (format "%s.prototype.%s = %s" class fname (wmac--eval `(fn ,args ,@body))))

(provide 'wmac-mode)

;;--------------------------------------------------------------------------------

;;(+ 1 (* 2 3) "greetings!" 3)
;;
;;(* 2 3)
;;
;;"heya!"
;;
;;14
;;
;;(log "value of x" x)
;;
;;(require 'hyperlog 'hypercore 'kappa-core)
;;
;;(fn (e) (log "e" e))
;;
;;(global-set-key (kbd "<S-return>") 'wmac--eval-last-sexpr)
;;
