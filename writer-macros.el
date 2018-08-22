(define-minor-mode jswm-mode
  "Enable noffle's javascript writer macros."
  :ligher " JS-Writer-Macros"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<S-return>") 'wmjs-eval-last-sexpr-maybe-and-newline)
            map))

(defmacro defun-js (name args &rest forms)
  `(defun
       ,(intern (concat "js/" (stringify name)))
       ,args
     (let* ((real-args (mapcar 'wmjs-eval args))
            (f (lambda ,args ,@forms)))
     (apply f real-args))))

(defmacro defmacro-js (name args &rest forms)
  `(defun
       ,(intern (concat "js/" (stringify name)))
       ,args
     ,@forms))

(defun wmjs-eval-last-sexpr-maybe-and-newline ()
  (interactive)
  (unless (not (and (eql (char-before) 41) (eql 0 (car (syntax-ppss)))))
      (wmjs-eval-last-sexpr))
  (newline-and-indent))

(defun wmjs-eval-last-sexpr ()
  (interactive)
  (backward-kill-sexp)
  (let* ((res (wmjs-eval (read (current-kill 0)))))
;         (cursor-pos (string-match "{|}" res)))
    (insert res)))

(defun wmjs-eval (obj)
  (cond
    ((eql (type-of obj) 'string) (concat "'" (stringify obj) "'"))
    ((eql (type-of obj) 'cons)   (wmjs-eval-sexpr obj))
    (t                           (stringify obj))))

(defun wmjs-eval-sexpr (sexpr)
  (let ((func (intern-soft (concat "js/" (symbol-name (car sexpr))))))
    (if func
        (apply func (cdr sexpr))
      (let ((eval-args (mapcar 'wmjs-eval (cdr sexpr))))
        (regfunc (stringify (car sexpr)) eval-args)))))

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

(defun-js or (&rest args) (infix "||" args))
(defun-js not (arg) (concat "!" (wmjs-eval arg)))
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
   (str-join (mapcar (lambda (sexp) (concat "  " (wmjs-eval sexp) "\n")) body) "")
   "}"))

(defmacro-js var (name value)
  (concat "var " (stringify name) " = " (wmjs-eval value)))

(defmacro-js if (condition a b)
  (concat "if (" (wmjs-eval condition) ") {\n"
          "  " (wmjs-eval a) "\n} else {\n"
          "  " (wmjs-eval b) "\n}"))

(defmacro-js let (vars &rest body)
  (concat
   (str-join
    (mapcar
     (lambda (pair) (concat "var " (wmjs-eval (car pair)) " = " (wmjs-eval (cadr pair)) "\n"))
     vars)
    "")
   (str-join
    (mapcar
     'wmjs-eval
     body)
    "\n")))

(provide 'jswm-mode)

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
;;(global-set-key (kbd "<S-return>") 'wmjs-eval-last-sexpr)
;;
