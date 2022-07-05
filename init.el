;; lumiknit's init.el
;; 20220705

;;--------------------
;; Debug
;;(setq debug-on-error t) ;; Uncomment for debug
(setq warning-minimum-level :emergency)

;;--------------------
;; Load helper
(setq emacs-d "~/.emacs.d")
(when (file-newer-than-file-p "~/.emacs.d/init-helper.el"
                              "~/.emacs.d/init-helper.elc")
  (byte-compile-file "~/.emacs.d/init-helper.el"))
(load "~/.emacs.d/init-helper")

;;--------------------
;; Environment path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;--------------------
;; Mac Set-up
;; Meta key
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)
;; Title bar
;;(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))
(set-frame-parameter nil 'internal-border-width 0)

;;--------------------
;; Theme and font
(let ((theme 'Parchment)
      (face "PragmataPro Liga")
      (height 16))
  (load-theme theme t)
  (set-font-info face (* 10 height) (+ 4 height) "outline" 'default nil)
  (update-font))

;;--------------------
;; Aquamacs compatible
(de-aqua-macs)

;;--------------------
;; UI
;; (global-linum-mode)
(make-emacs-simple)
(make-dialog-simple)

;;--------------------
;; Cursor
(set-cursor 'box nil
            0.3 0.2 0)
(set-paren-mode 0 'paren)
(set-encoding 'utf-8-unix)

;;--------------------
;; No Backup
(setq-default backup-directory-alia
              `(("." . ,(in-d "backup"))))
(turn-off-any-backup)
(global-auto-revert-mode t)

;;--------------------
;; Indentation
(set-indentation 'space 2)
(set-ret-indent)
(setq c-default-style "BSD")

;;--------------------
;; electric-pair-mode
(electric-pair-mode)
(setq electric-pair-preserve-balance t)
(setq electric-pair-delete-adjacent-pairs t)
(setq electric-pair-open-newline-between-pairs nil)
(setq electric-pair-skip-whitespace nil)

;;--------------------
;; Other
(enable-punc-highlight)
(set-no-extra-whitespaces)
(delete-selection-mode 1)
(setq-default transient-mark-mode t)
(setq scroll-step 4
      scroll-margin 2)
(setq-default fill-column 80)
(set-language-environment "English")
(set-eldoc-turn-on-properly)
(setq-default case-fold-search nil)
(setq query-replace-highlight t)
(setq comint-prompt-read-only t)

;;----------------------------------------
;; Package
;;----------------------------------------
(req-package)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package magit
  :defer t
  :commands magit-get-top-dir
  :bind (("C-x g" . 'magit-status)))

(use-package company
  :init
  (setq company-auto-complete-chars nil
        company-dabbrev-code-time-limit 0
        company-dabbrev-downcase 0
        company-dabbrev-code-other-buffers nil
        company-dabbrev-code-everywhere t
        company-dabbrev-minimum-length 2
        company-tooltip-idle-delay 0
        company-idle-delay 0
        company-tooltip-limit 5
        company-minimum-prefix-length 3)
  :bind (:map company-active-map
         ("<return>" . nil)
         ("RET" . nil)
         ("TAB" . #'company-complete)
         ("<tab>" . #'company-complete))
  :config
  (global-company-mode))

(use-package haskell
  :init
  (setq haskell-doc-prettify-types nil)
  (defun haskell-doc-mode-print-current-symbol-info (&optional arg) 0)
  :config
  (set-haskell))

;; latex
(setq TeX-PDF-mode t)
(setq latex-run-command "pdflatex")

;; docview
(require 'doc-view)
(setq doc-view-resolution 192)

;; Racket
(setq racket-program "/Applications/Racket v6.12/bin/racket")

;; OCaml
(require 'tuareg)
(setq tuareg-indent-align-with-first-arg t)

(defun opam-env ()
  (interactive nil)
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))
(opam-env)

;; Chicken
(load-d "chicken")
(setq scheme-program-name "/usr/local/bin/csi -:c")

;; Lisp
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Coq
(setenv "PATH" (concat "/home/lumiknit/.opam/default/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/home/lumiknit/.opam/default/bin")
(setq coq-compile-before-require t)

(load-d "enclose")

;;------------------------------------------------------------
;; Keybind
(global-set-keys
 `(((kbd "C-z")                )
   ((kbd "<S-tab>")            unindent-by-removing-spaces)
   ((kbd "<S-backspace>")      erase-spaces)
   ((kbd "<M-DEL>")            ,(kbd "C-u -1 C-M-k"))
   ((kbd "<M-SPC>")            toggle-input-method)
   ((kbd "C-*")                region-to-new-buffer)
   ((kbd "C-M-*")              retion-to-new-buffer-other-window)
   ((kbd "C-`")                toggle-shell)
   ;; Size
   ((kbd "C-+")                increase-window-v)
   ((kbd "C-=")                decrease-window-v)
   ((kbd "C-}")                increase-window-h)
   ((kbd "C-{")                decrease-window-h)
   ((kbd "C-M-+")              increase-frame)
   ((kbd "C-M-=")              decrease-frame)
   ((kbd "C-M-{")              move-frame-left)
   ((kbd "C-M-|")              move-frame-center)
   ((kbd "C-M-}")              move-frame-right)
   ;; Multiple Cursor
   ((kbd "C-'")                mc/mark-all-like-this)
   ((kbd "C->")                mc/mark-next-like-this)
   ((kbd "C-<")                mc/mark-previous-like-this)
   ((kbd "C-c C-<")            mc/mark-all-like-this)
   ((kbd "<C-M-mouse-1>")      mc/add-cursor-on-click)
   ))

;;--
;; Reset Warning Level
(setq warning-minimum-level :warning)
