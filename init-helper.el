;; init-helper.el
;; 20220705
;; lumiknit

;;----------------------------------------
;; Loader
(when (not (boundp 'compile-and-load))
  (defun compile-and-load (name)
    "Compile .el to .elc (if not updated) and load it"
    (let ((n (concat emacs-d "/" name))
          (el (concat emacs-d "/" name ".el"))
          (elc (concat emacs-d "/" name ".elc")))
      (when (file-newer-than-file-p el elc)
        (byte-compile-file el))
      (load n))))

;;----------------------------------------
;; Font
(defface mode-line-2
  '((t (:inherit mode-line)))
  "Mode Line Additional Face"
  :group 'basic-faces)
(defface mode-line-3
  '((t (:inherit mode-line)))
  "Mode Line Additional Face"
  :group 'basic-faces)

(defvar font-family-name "PragmataPro Liga")
(defvar font-size 10)
(defvar font-pixel-height 12)
(defvar font-foundry "outline")
(defvar font-bold-option 'optional)
(defvar font-korean nil)

(defun set-font-info (family-name size pixel-height foundry bold-opt korean)
  "Set the font information for 'update-font"
  (setq-default font-family-name family-name
                font-size size
                font-pixel-height pixel-height
                font-foundry foundry
                font-bold-option bold-opt
                font-korean korean))

(defun update-font ()
  "Update font settings (with information give by 'set-font-info)"
  (interactive)
  ;;--- Change Font
  (let ((no (or (not font-bold-option)
                (eq font-bold-option 'no)))
        (always (eq font-bold-option 'always))
        (slant (or (eq font-bold-option 'no)
                   (eq font-bold-option 'always))))
    (custom-set-faces
     `(default ((t (:family ,font-family-name :foundry foundry :slant normal :weight ,(if always 'bold 'normal) :height ,font-size :width normal))))
     `(bold ((t (:inherit default :weight ,(if no 'normal 'bold)))))
     `(italic ((t (:inherit default :slant ,(if slant 'normal 'italic)))))
     `(bold-italic ((t (:inherit default
                          :slant ,(if slant 'normal 'italic)
                          :weight ,(if no 'normal 'bold)))))))

  (when font-korean
    (set-fontset-font "fontset-default" 'hangul font-korean))

  ;;--- Font Ligatures (Only For PragmataPro)
  (let ((pragmata (string-prefix-p "PragmataPro" font-family-name))
        (powerline (or (string-prefix-p "PragmataPro" font-family-name)
                       (string-prefix-p "Fira Code" font-family-name)
                       (string-prefix-p "NeoDunggeunmo" font-family-name)
                       (string-suffix-p "Powerline" font-family-name))))
    (if pragmata
        (progn
          ;;(add-to-list 'load-path "/home/lumiknit/.emacs.d/prag")
          ;;(require 'pragmatapro-lig)
          (compile-and-load "pragmatapro-lig")
          ;;(add-hook 'text-mode-hook 'pragmatapro-lig-mode)
          ;;(add-hook 'prog-mode-hook 'pragmatapro-lig-mode)
          (pragmatapro-lig-global-mode))
      (remove-hook 'text-mode-hook 'pragmatapro-ligatures)
      (remove-hook 'prog-mode-hook 'pragmatapro-ligatures))
    ;;--- Modeline
    (let* ((g1 (face-background 'mode-line-2 nil t))
           (g2 (face-background 'mode-line-3 nil t))
           (special powerline)
           (bg1 `(:background ,g1))
           (fg1 `(:foreground ,g1))
           (fg1-inv `(:foreground ,g1 :inverse-video t))
           (bg2 `(:background ,g2))
           (fg2 `(:foreground ,g2))
           (fg2-inv `(:foreground ,g2 :inverse-video t))
           (f1b2 `(:foreground ,g1
                   :background ,g2))
           (f2b1 `(:foreground ,g2
                   :background ,g1))
           (f2b2 `(:foreground ,g2
                   :background ,g2))
           (id `(:inherit mode-line-buffer-id
                 :background ,g2)))
      (setq-default
       mode-line-format
       `((:propertize (" %e" mode-line-modified " "))
         (:propertize ,(if special "\xe0b0 " " ") face ,fg1-inv)
         (:propertize ,(if pragmata
                           '(:eval (pragmatapro-get-mode-icon))
                         'mode-name)
                      face ,bg1)
         (:propertize ,(if special " " " ") face ,bg1)
         (:propertize ,(if special "\xe0b0 " " ") face ,f1b2)
         (:propertize " %b " face ,id)
         (:propertize ,(if special " \xe0b2" " ") face ,fg2-inv)
         (:propertize ,(if special " %IB \xe0b3 \xe0a1 %l:%c  "
                         "  %IB | %l:%c  "))
         ))))
  t)

;;----------------------------------------
(defconst hidpi-height-criteria 1440)
(defun is-hidpi ()
  "Test current Emacs is running on Hi-DPI Screen"
  (>= (x-display-pixel-height) hidpi-height-criteria))

(defvar frame-x-margin 0)
(defvar frame-y-margin 0)
(defun set-frame-margin (x y)
  "Set margin information for frame movement"
  (setq-default frame-x-margin x
                frame-y-margin y))

(defvar window-increment 4)
(defun set-window-increment-size (v)
  (setq-default window-increment v))

(defvar frame-increment 2)
(defun set-frame-increment-size (v)
  (setq-default frame-increment v))

(defvar linum-length 3)
(defun get-frame-width ()
  (if (member 'global-linum-mode minor-mode-list)
      (+ 80 linum-length)
    80))

;;----------------------------------------
;; Frame size/position adjustment
(defun increase-window-v ()
  (interactive)
  (enlarge-window window-increment))
(defun decrease-window-v ()
  (interactive)
  (shrink-window window-increment))
(defun increase-window-h ()
  (interactive)
  (enlarge-window-horizontally window-increment))
(defun decrease-window-h ()
  (interactive)
  (shrink-window-horizontally window-increment))

(defun increase-frame ()
  (interactive)
  (set-frame-height (selected-frame)
                    (+ (frame-height)
                       (if (is-hidpi) (* 2 frame-increment)
                         frame-increment))))
(defun decrease-frame ()
  (interactive)
  (set-frame-height (selected-frame)
                    (- (frame-height)
                       (if (is-hidpi) (- (* 2 frame-increment) 1)
                             frame-increment))))

(defun max-lines-in-a-screen ()
  "Guess how many lines can be shown in a screen"
  (/ (/ (- (x-display-pixel-height) frame-y-margin)
       font-pixel-height) (if (is-hidpi) 2 1)))
(defun move-frame-left (&optional n)
  (interactive "p")
  (setq n (if n (max n 1) 1))
  (let ((w (* n (get-frame-width))))
    (set-frame-position (selected-frame)
                        frame-x-margin
                        frame-y-margin)
    (set-frame-size (selected-frame) w (max-lines-in-a-screen))))
(defun move-frame-center (&optional n)
  (interactive "p")
  (setq n (if n (max n 1) 1))
  (let ((w (* n (get-frame-width)))
        (pw (* n (frame-pixel-width))))
    (set-frame-position (selected-frame)
                        (/ (- (x-display-pixel-width) pw) 2)
                        frame-y-margin)
    (set-frame-size (selected-frame) w (max-lines-in-a-screen))))
(defun move-frame-right (&optional n)
  (interactive "p")
  (setq n (if n (max n 1) 1))
  (let ((w (* n (get-frame-width)))
        (pw (* n (frame-pixel-width))))
    (set-frame-position (selected-frame)
                        (- (x-display-pixel-width) pw frame-x-margin)
                        (if (is-hidpi) 60 frame-y-margin))
    (set-frame-size (selected-frame) w (max-lines-in-a-screen))))

;;----------------------------------------
;; Simplifier

(defun make-emacs-simple ()
  "Remove every UI, fringe, splash screen, bell, etc."
  (interactive)
  (setq-default frame-title-format "Emacs")
  (custom-set-variables
   '(menu-bar-mode nil)
   '(scroll-bar-mode -1)
   '(tool-bar-mode nil)
   '(inhibit-startup-screen t))
  (fringe-mode (if (is-hidpi) '(16 . 16) '(8 . 8)))
  (setq-default ring-bell-function 'ignore)
  (setq-default linum-format "%6d ")
  (setq linum-length 7)
  (setq-default initial-scratch-message
                ";;      --- Emacs *scratch* ---\n"))

(defun make-dialog-simple ()
  "Change yes-or-no to y-or-n, disable dialog, etc."
  (interactive)
  (setq-default use-dialog-box nil)
  (defalias 'yes-or-no-p 'y-or-n-p))

;;----------------------------------------
;; Cursor
(defun set-cursor (shape size blink-delay blink-interval blinks)
  (setq-default cursor-type
                (if size `(,shape . ,size) shape)
                x-stretch-cursor 0)
  (if (> blink-interval 0)
    (progn (setq-default blink-cursor-delay blink-delay
                         blink-cursor-interval blink-interval
                         blink-cursor-blinks blinks)
           (blink-cursor-mode 1))
    (blink-cursor-mode 0)))

;;----------------------------------------
;; Paren Mode
(defun set-paren-mode (delay style)
  (setq show-paren-delay delay
        show-paren-style style
        show-paren-priority -1)
  (show-paren-mode 1))

;;----------------------------------------
;; Encoding
(defun set-encoding (enc)
  (setq-default buffer-file-coding-system enc
                default-buffer-file-coding-system enc)
  (set-default-coding-systems enc)
  (prefer-coding-system enc))

;;----------------------------------------
;; Backup
(defun turn-off-any-backup ()
  (setq-default make-backup-files nil
                auto-save-default nil))

;;----------------------------------------
;; Whitespaces
(defun set-no-extra-whitespaces ()
  (interactive)
  (setq-default next-line-add-newlines nil)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;----------------------------------------
;; Indentation
(defun set-indentation (tab size)
  (setq-default indent-tabs-mode (eq tab 'tab)
                default-tab-width size
                tab-width size
                standard-indent size
                c-basic-offset size
                cperl-indent-level size
                lua-indent-level size
                rust-indent-offset size
                python-indent-offset size
                js-indent-level size)
  (setq tab-stop-list (number-sequence size 200 size)))
(defun set-ret-indent ()
  (global-set-key (kbd "RET") 'newline-and-indent))

(defun unindent-by-removing-spaces ()
  "Remove (standard-indent) spaces to unindent"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at (concat "^" (make-string tab-width ?\ )))
        (replace-match "")))))

(defun erase-spaces ()
  "Remove one character or spaces"
  (interactive)
  (save-excursion
    (save-match-data
      (backward-char)
      (if (looking-at "[ \t]")
          (progn
            (while (looking-at "[ \t\r\n]")
              (delete-char 1)
              (backward-char)))
        (delete-char 1)))))

(defun indent-as-last ()
  (interactive)
  (save-match-data
    (let ((i (save-excursion
               (catch 'break
                 (while (> (point) 1)
                   (beginning-of-line 0)
                   (when (looking-at "^[ \t]*[^ \t\r\n].*$")
                     (throw 'break nil))))
               (current-indentation))))
      (save-excursion (indent-line-to i))
      (when (looking-at "[ \t]+")
        (goto-char (match-end 0))))))
;;----------------------------------------
;; Face Utility
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;----------------------------------------
;; Highlight Punctuations
(unless (boundp 'font-lock-operator-face)
  (defface font-lock-operator-face
    '((t (:inherit default)))
    "Basic face for operator"
    :group 'basic-faces)
  (setq-default font-lock-operator-face 'font-lock-operator-face))

(defun enable-punc-highlight ()
  (let ((punc-list
         '((c-mode          "[][(){},.;:~!%^&*+=|<>/?-]+")
           (c++-mode        "[][(){},.;:~!%^&*+=|<>/?-]+")
           (java-mode       "[][(){},.;:~!%^&*+=|<>/?-]+")
           (scala-mode      "[][(){},.;:~!%^&*+=|<>/?-]+")
           (rust-mode       "[][(){},.;:~!%^&*+=|<>/?-]+")
           (python-mode     "[][(){},.;:~!%^&*+=|<>/?-]+")
           (ruby-mode       "[][(){},.;:~!%^&*+=|<>/?-]+")
           (crystal-mode    "[][(){},.;:~!%^&*+=|<>/?-]+")
           (perl-mode       "[][(){},.;:~!%^&*+=|<>/?-]+")
           (lua-mode        "[][(){},.;:~!%^&*+=|<>/?#-]+")
           (moonscript-mode "[][(){},.;:~!%^&*+=|<>/?#-]+")
           (go-mode         "[][(){},.;:~!%^&*+=|<>/?-]+")
           (swift-mode      "[][(){},.;:~!%^&*+=|<>/?-]+")
           (js-mode         "[][(){},.;:~!%^&*+=|<>/?-]+")
           (groovy-mode     "[][(){},.;:~!%^&*+=|<>/?-]+")
           (typescript-mode "[][(){},.;:~!%^&*+=|<>/?-]+")
           (emacs-lisp-mode       "[][(){}]+\\|\\b[,.^~!&*+=|<>/?'`]+\\b")
           (lisp-interaction-mode "[][(){}]+\\|\\b[,.^~!&*+=|<>/?'`]+\\b")
           (lisp-mode             "[][(){}]+\\|\\b[,.^~!&*+=|<>/?'`]+\\b")
           (scheme-mode           "[][(){}]+\\|\\b[:,.^~!&*+=|<>/?'`]+\\b")
           (racket-mode           "[][(){}]+\\|\\b[:,.^~!&*+=|<>/?'`]+\\b")
           (clojure-mode          "[][(){}:,.^~!&*+=|<>/?'`]+")
           (haskell-mode          "[][(){}:;,.^~!#$@&*+=|<>/?\\-]+")
           (literate-haskell-mode "[][(){}:;,.^~!#$@&*+=|<>/?\\-]+")
           (elixir-mode "[][(){}:,.@^~!&*+=|<>/?'`-]+")
           (tuareg-mode "[][(){}:;,.^~!#$@&*+=|<>/?\\-]+")
           (sml-mode    "[][(){}:;,.^~!#$@&*+=|<>/?\\-]+")
           (fsharp-mode "[][(){}:;,.^~!#$@&*+=|<>/?\\-]+")
           (coq-mode          "[][(){}:;,.^~!#$@&*+=|<>/?\\-]+")
           (coq-response-mode "[][(){}:;,.^~!#$@&*+=|<>/?\\-]+")
           (coq-goals-mode    "[][(){}:;,.^~!#$@&*+=|<>/?\\-]+")
           )))
    (dolist (i punc-list)
      (font-lock-add-keywords
       (car i)
       `((,(cadr i) 0 'font-lock-operator-face))))))

;;----------------------------------------
;; Keybindings
(defun global-set-keys (lst)
  (define-keys (current-global-map) lst))
(defun define-keys (map lst)
  (dolist (i lst)
    (let ((key (eval (car i)))
          (fn (cadr i)))
      (define-key map key fn))))

;;----------------------------------------
;; Packages
(defun req-package ()
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;;----------------------------------------
;; Load
(defvar path-d "~/.emacs.d")

(defun in-d (cont)
  "return the path concatenated with ...emacs.d"
  (concat path-d "/" cont))

(defmacro load-d (name)
  `(load ,(concat path-d "/" name "/" name)))

;;----------------------------------------
;; No Global Eldoc
(defun set-eldoc-turn-on-properly ()
  ;;(global-eldoc-mode -1)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-interaction-mode-hook 'turn-on-eldoc-mode))

;;----------------------------------------
;; De-aqua-macs
(defun de-aqua-macs ()
  "Disable all aquamacs features"
  (interactive)
  (when (boundp 'aquamacs-version)
    (osx-key-mode -1)
    (aquamacs-autoface-mode -1)
    (set-face-attribute 'mode-line nil :inherit 'unspecified)
    (set-face-attribute 'echo-area nil :family 'unspecified)
    (setq select-enable-clipboard t)
    (setq ns-command-modifier 'meta
          ns-alternate-modifier nil
          ns-use-mac-modifier-symbols nil)
    (setq aquamacs-scratch-file nil
          initial-major-mode 'emacs-lisp-mode)
    (global-smart-spacing-mode -1)
    (remove-hook 'text-mode-hook 'smart-spacing-mode)
    ))

;;----------------------------------------
;; Haskell

(defun set-haskell ()
  (setq haskell-process-type 'stack-ghci
        haskell-process-args-stack-ghci
        (eval-when-compile
          '("--ghci-options=-ferror-spans"
            "--no-build" "--no-load")))
  (eval-after-load
      'haskell-mode
    (define-keys
      haskell-mode-map
      '(((kbd "C-c C-l")     haskell-process-load-file)
        ((kbd "C-c C-k")     haskell-interactive-mode-clear)
        ((kbd "C-c C-z")     haskell-interactive-switch)
        ((kbd "C-c C-n C-t") haskell-process-do-type)
        ((kbd "C-c C-n C-i") haskell-process-do-info)
        ((kbd "C-c C-n C-c") haskell-process-cabal-build)
        ((kbd "C-c C-n c")   haskell-process-cabal)
        ((kbd "C-c C-c")     haskell-compile)
        ((kbd "C-c C-n C-k") haskell-process-cabal-run)
        ((kbd "C-c h")       haskell-hoogle))))
  (eval-after-load
      'haskell-cabal
    (define-keys
      haskell-cabal-mode-map
       '(((kbd "C-c C-z") haskell-interactive-switch)
         ((kbd "C-c C-k") haskell-interactive-mode-clear)
         ((kbd "C-c C-c") haskell-process-cabal-build)
         ((kbd "C-c c")   haskell-process-cabal)))))

;;----------------------------------------
;; Fast Buffer

(defun region-to-new-buffer ()
  "Copy the region and create new buffer with it"
  (interactive)
  (if (not mark-active)
      (message "mark is not actived")
    (let* ((content (buffer-substring-no-properties (region-beginning)
                                                    (region-end)))
           (old-name (buffer-name))
           (buffer (get-buffer-create
                    (concat "*" old-name "[COPIED]*"))))
      (setq mark-active nil)
      (switch-to-buffer buffer)
      (erase-buffer)
      (insert content))))

(defun region-to-new-buffer-other-window ()
  "Copy the region and create new buffer with it"
  (interactive)
  (if (not mark-active)
      (message "mark is not actived")
    (let* ((content (buffer-substring-no-properties (region-beginning)
                                                    (region-end)))
           (old-name (buffer-name))
           (buffer (get-buffer-create
                    (concat "*" old-name "[COPIED]*"))))
      (setq mark-active nil)
      (switch-to-buffer-other-window buffer)
      (erase-buffer)
      (insert content)
      (shrink-window-if-larger-than-buffer))))

;;----------------------------------------
;; Shell Shortcut
(defun toggle-shell ()
  "Toggle shell buffer"
  (interactive)
  (if (string= (buffer-name (current-buffer)) "*shell*")
      (delete-window)
    (let ((bl (buffer-list)))
      (while (and bl (not (string= (buffer-name (car bl)) "*shell*")))
        (setq bl (cdr bl)))
      (if bl
          (switch-to-buffer-other-window (car bl))
        (shell)))))

;;----------------------------------------
(provide 'init-helper)
