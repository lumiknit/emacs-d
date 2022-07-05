;;;; chicken.el - Scheme mode modified for use with CHICKEN


(require 'cmuscheme)
(require 'cl)

(define-key scheme-mode-map "\C-c\C-l" 'chicken-load-current-file)
(define-key scheme-mode-map "\C-c\C-k" 'chicken-compile-current-file)
(define-key scheme-mode-map "\C-x\C-e" 'chicken-send-last-sexp-with-info)
(define-key scheme-mode-map "\C-c\C-a" 'chicken-apropos)
(define-key scheme-mode-map "\C-c\C-t" 'chicken-trace)
(define-key scheme-mode-map "\C-c\C-c" 'scheme-compile-definition)

(define-key scheme-mode-map "\C-c\C-d" 'chicken-doc)
(define-key scheme-mode-map "\C-c\C-t" 'chicken-trace)
(define-key scheme-mode-map "\C-c\C-g" 'scheme-grep-current-word)
(define-key inferior-scheme-mode-map "\C-c\C-g" 'scheme-grep-current-word)
(define-key inferior-scheme-mode-map "\C-c\C-d" 'chicken-doc)
(define-key inferior-scheme-mode-map "\C-c\C-t" 'chicken-trace)
(define-key inferior-scheme-mode-map "\C-c\C-a" 'chicken-apropos)

(defun chicken-load-current-file (&optional switch)
  (interactive "P")
  (let ((file-name (buffer-file-name)))
    (comint-check-source file-name)
    (setq chicken-prev-l/c-dir/file
	  (cons (file-name-directory file-name)
		(file-name-nondirectory file-name)))
    (comint-send-string
     (scheme-proc)
     (concat "(begin (load \"" file-name "\"\) (newline))\n"))
    (if switch
      (switch-to-scheme t)
      (message "\"%s\" loaded." file-name) ) ) )

(defun chicken-compile-current-file (&optional switch)
  (interactive "P")
  (let ((file-name (buffer-file-name)))
    (comint-check-source file-name)
    (setq chicken-prev-l/c-dir/file
	  (cons (file-name-directory file-name)
		(file-name-nondirectory file-name)))
    (message "compiling \"%s\" ..." file-name)
    (comint-send-string
     (scheme-proc)
     (concat "(compile-file \"" file-name "\"\)\n"))
    (if switch
      (switch-to-scheme t)
      (message "\"%s\" compiled and loaded." file-name) ) ) )





(defun chicken-send-input (&optional no-newline artificial)
  (interactive)
  (when completion-in-region-mode
    (completion-in-region-mode -1))
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc))
             (intxt (if (>= (point) (marker-position pmark))
                        (progn (if comint-eol-on-send
				   (if comint-use-prompt-regexp
				       (end-of-line)
				     (goto-char (field-end))))
                               (buffer-substring pmark (point)))
                      (let ((copy (funcall comint-get-old-input)))
                        (goto-char pmark)
                        (insert copy)
                        copy)))
             (input (if (not (eq comint-input-autoexpand 'input))
                        intxt
                      (comint-replace-by-expanded-history t pmark)
                      (buffer-substring pmark (point))))
             (history (if (not (eq comint-input-autoexpand 'history))
                          input
                        (comint-replace-by-expanded-history t pmark)
                        (let ((copy (buffer-substring pmark (point)))
                              (start (point)))
                          (insert input)
                          (delete-region pmark start)
                          copy))))
        (unless no-newline
          (insert ?\n))
        (comint-add-to-input-history history)
        (run-hook-with-args 'comint-input-filter-functions
                            (if no-newline input
                              (concat input "\n")))
        (let ((beg (marker-position pmark))
              (end (if no-newline (point) (1- (point)))))
          (with-silent-modifications
            (when (> end beg)
              (add-text-properties beg end
                                   '(front-sticky t))
              (unless comint-use-prompt-regexp
                (add-text-properties
                 beg end
                 '(mouse-face highlight
                   help-echo "mouse-2: insert after prompt as new input"))))
            (unless (or no-newline comint-use-prompt-regexp)
              (add-text-properties end (1+ end)
                                   '(rear-nonsticky t
                                     field boundary
                                     inhibit-line-move-field-capture t)))))
        (comint-snapshot-last-prompt)
        (setq comint-save-input-ring-index comint-input-ring-index)
        (setq comint-input-ring-index nil)
        (set-marker comint-last-input-start pmark)
        (set-marker comint-last-input-end (point))
        (set-marker (process-mark proc) (point))
        (set-marker comint-accum-marker nil)
        (let ((comint-input-sender-no-newline no-newline))
          (funcall comint-input-sender proc input))
        (when (and comint-process-echoes (not artificial))
          (let ((echo-len (- comint-last-input-end
                             comint-last-input-start)))
            (while (and (> (+ comint-last-input-end echo-len)
                           (point-max))
                        (accept-process-output proc)
                        (zerop
                         (compare-buffer-substrings
                          nil comint-last-input-start
                          (- (point-max) echo-len)
                          nil comint-last-input-end (point-max)))))
            (if (and
                 (<= (+ comint-last-input-end echo-len)
                     (point-max))
                 (zerop
                  (compare-buffer-substrings
                   nil comint-last-input-start comint-last-input-end
                   nil comint-last-input-end
                   (+ comint-last-input-end echo-len))))
                (let ((inhibit-read-only t))
                  (delete-region comint-last-input-end
                                 (+ comint-last-input-end echo-len))
                  (when comint-prompt-read-only
                    (save-excursion
                      (goto-char comint-last-input-end)
                      (comint-update-fence)))))))
        (run-hook-with-args 'comint-output-filter-functions "")))))
;;----------


(defun chicken-send-string-newline (process string)
  (with-current-buffer (if (processp process)
                           (process-buffer process)
                         (get-buffer process))
    (let ((pmark (process-mark process)))
      (goto-char pmark)
      (let* ((c (current-column))
             (pr (if (< c 4) (make-string c ? )
                   (concat "#;" (make-string (- c 4) ?=) "> ")))
             (sl (split-string string "\n")))
        (insert (concat
                 (car sl) "\n"
                 (mapconcat (lambda (x) (concat pr x)) (cdr sl) "\n")))
        (chicken-send-input)))))


(defun chicken-send-last-sexp-with-info ()
  "hacked version from cmuscheme.el"
  (interactive)
  (let* ((str
          (buffer-substring (save-excursion (backward-sexp) (point)) (point)))
         (lnbreak (position ?\n str)))
    (chicken-send-string-newline (scheme-proc) str)))

(defun chicken-doc (&optional arg)
  (interactive "P")
  (let ((words
	 (if current-prefix-arg
	     (split-string (read-from-minibuffer "chicken-doc: ") " ")
	   (list (current-word)))))
    (unless (null words)
      (run-chicken-doc words))))

(defun chicken-toc (&optional arg)
  (interactive "P")
  (let ((words
	 (if current-prefix-arg
	     (split-string (read-from-minibuffer "chicken-doc: ") " ")
	   (list (current-word)))))
    (unless (null words)
      (run-chicken-doc (cons "-c" words)))))

(defun run-chicken-doc (args)
  (with-current-buffer (get-buffer-create "*chicken-doc*")
    (erase-buffer)
    (when (zerop (apply 'call-process "chicken-doc" nil "*chicken-doc*" t args))
      (goto-char (point-min))))
  (unless (string= "*chicken-doc*" (buffer-name (current-buffer)))
    (display-buffer "*chicken-doc*" t)))

(defun chicken-apropos ()
  (interactive)
  (let ((func (current-word)))
    (when func
      (process-send-string "*scheme*" (concat "(begin (newline) (apropos \"" func "\"))\n"))
      (unless (string= (buffer-name (current-buffer)) "*scheme*")
	(display-buffer "*scheme*" t)))))

(defun chicken-trace ()
  (interactive)
  (let ((func (current-word)))
    (when func
      (process-send-string "*scheme*" (concat "(begin (newline) (trace/untrace " func "))\n"))
      (unless (string= (buffer-name (current-buffer)) "*scheme*")
	(display-buffer "*scheme*" t)))))

(defun scheme-grep-current-word (&optional arg)
  (interactive "P")
  (let ((word (or (and (not current-prefix-arg)
		       (current-word))
		  (read-from-minibuffer
		   (concat grep-command " <word> *.scm: ")))))
    (grep (concat grep-command " \'" word "\' " "*.scm"))))

(setq scheme-program-name "csi -:c")

(setq chicken-keyword-list
      '((receive 2)
	(match 1)
	(match-lambda 0)
	(match-lambda* 0)
	(match-let scheme-let-indent)
	(match-let* 1)
	(match-letrec 1)
	(declare 0)
	(cond-expand 0)
	(let-values scheme-let-indent)
	(let*-values scheme-let-indent)
	(letrec-values 1)
	(letrec* 1)
	(parameterize scheme-let-indent)
	(let-location 1)
	(foreign-lambda 2)
	(foreign-lambda* 2)
	(foreign-primitive 2)
	(foreign-safe-lambda 2)
	(foreign-safe-lambda* 2)
	(set! 1)
	(let-optionals* 2)
	(let-optionals 2)
	(condition-case 1)
	(fluid-let 1)
	(and-let* 1)
	(assume 1)
	(cut 1)
	(cute 1)
	(when 1)
	(unless 1)
	(dotimes 1)
	(compiler-typecase 1)
	(ecase 1)
	(require-extension 0)
	(import 0)
	(handle-exceptions 2)
	(regex-case 1)
	(define-inline 1)
	(define-constant 1)
	(define-syntax-rule 1)
	(define-record-type 1)
	(define-values 1)
	(define-record 1)
	(define-specialization 1)
	(define-type 1)
	(with-input-from-pipe 1)
	(select 1)
	(functor 3)
	(define-interface 1)
	(module 2) ) )

(setq chicken-indent-list
      '((printf 1)
	(fprintf 2)
	(sprintf 1)))

;(put 'module 'scheme-indent-function 'chicken-module-indent)
;(defun chicken-module-indent (state indent-point normal-indent) 0)

(defun chicken-build-keyword-regexp (kwl)
  (let ((str "\\<\\(module\\>"))
    (dolist (kw kwl)
      (put (car kw) 'scheme-indent-hook (cadr kw))
      (setq str (concat str "\\|" (regexp-quote (symbol-name (car kw))) "\\>")))
    (concat str "\\)")))

(setq chicken-keyword-regexp
      (chicken-build-keyword-regexp chicken-keyword-list))

(dolist (e chicken-indent-list)
  (put (car e) 'scheme-indent-hook (cadr e)))

(add-hook
 'scheme-mode-hook
 (lambda ()
   (font-lock-add-keywords
    'scheme-mode
    `(("\\<\\sw+\\>:" . font-lock-constant-face) ;XXX doesn't work, yet
      ("##\\(core\\|sys\\)#\\sw+\\>" . font-lock-builtin-face)
      (,chicken-keyword-regexp . font-lock-keyword-face)
      )
    1)))

(provide 'chicken)
