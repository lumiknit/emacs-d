;;; Dark-theme.el

(deftheme Dark)

(unless (boundp 'font-lock-operator-face)
  (defface font-lock-operator-face
    '((t (:inherit default)))
    "Basic face for operator"
    :group 'basic-faces)
  (setq-default font-lock-operator-face 'font-lock-operator-face))

(let ( (dp_foreground    "#d4d4d4")
       (dp_background    "#1e1e1e")
       (dp_background2   "#3e3e3e")
       (dp_background3   "#4f4f4f")
       (dp_grey          "#303030")
       (dp_pink          "#c586c0")
       (dp_light_blue    "#9cdcfe")
       (dp_dark_blue     "#569cd6")
       (dp_bright_blue   "#3abadc")
       (dp_select_blue   "#274f75")
       (dp_found_blue    "#444a50")
       (dp_seafoam       "#4ec9b0")
       (dp_green         "#608b4e")
       (dp_puke          "#b5cea8")
       (dp_yellow        "#dcdcaa")
       (dp_dark_yellow   "#d7ba7d")
       (dp_orange        "#ce9178")
       (dp_light_red     "#d16969")
       (dp_dark_red      "#f44747")

       (dp_status_blue   "#137cc9")
       (dp_status_purple   "#662577")


       (dp_grey3          "#303030")
       (dp_grey4          "#404040")
       (dp_grey5          "#505050")
       (dp_grey6          "#606060")
       (dp_grey7          "#707070")
       (dp_grey8          "#808080")

       (fg-1 "#f6f6f6")
       (fg0 "#e5e5e5")
       (fg1 "#d4d4d4")       ;; text
       (fg2 "#c3c3c3")
       (fg3 "#b2b2b2")
       (fg4 "#a1a1a1")
       (fg5 "#909090")
       (fg6 "#7f7f7f")
       (fg7 "#6e6e6e")
       (bg1 "#1E1E1E")       ;; background
       (bg1.5 "#282828")
       (bg2 "#303030")
       (bg3 "#424242")
       (bg4 "#545454")
       (cursor "#a0d0ff")
       (builtin "#569CD6")   ;; dark-blue
       (keyword "#569CD6")   ;; pink
       (const   "#c0c0c0")   ;; light-blue
       (comment "#608b4e")   ;; green
       (func    "#dddddd")   ;; yellow
       (op      "#fafafa")   ;; yellow
       (str     "#CE9178")   ;; orange
       (type    "#569CD6")   ;; seafoam
       (var     "#dddddd")   ;; light-blue
       (warning "#F44747")   ;; dark-red
       (warning2 "#D16969")) ;; light-red
  (custom-theme-set-faces
    'Dark
    `(default ((t (:background ,dp_background :foreground ,dp_foreground))))
    '(bold ((t (:inherit default :bold t))))
    '(italic ((t (:inherit default :italic t))))
    '(bold-italic ((t (:inherit default :bold t :italic t))))
    '(variable-pitch ((t nil)))
    '(fixed-pitch ((t nil)))
    '(fixed-pitch-serif ((t nil)))

    ;; FONT LOCK VARIABLES
    `(font-lock-builtin-face              ((t (:foreground ,builtin  ))))
    `(font-lock-comment-delimiter-face    ((t (:foreground ,comment  ))))
    `(font-lock-comment-face              ((t (:foreground ,comment  ))))
	  `(font-lock-constant-face             ((t (:foreground ,const  ))))
    `(font-lock-doc-face                  ((t (:foreground ,comment  ))))
    `(font-lock-function-name-face        ((t (:foreground ,func     ))))
    `(font-lock-keyword-face              ((t (:foreground ,dp_dark_blue :inherit (bold)))))
    `(font-lock-negation-char-face        ((t (:foreground ,const    ))))
    `(font-lock-operator-face             ((t (:foreground ,op       ))))
    `(font-lock-reference-face            ((t (:foreground ,const    ))))
    `(font-lock-regexp-grouping-backslash ((t (:foreground ,dp_light_red      ))))
    `(font-lock-regexp-grouping-construct ((t (:foreground ,dp_light_red      ))))
    `(font-lock-string-face               ((t (:foreground ,str      ))))
    `(font-lock-type-face                 ((t (:foreground ,type     ))))
    `(font-lock-variable-name-face        ((t (:foreground ,var      ))))
    `(font-lock-warning-face              ((t (:foreground ,warning :background ,bg2))))

    ;; GENERAL
    `(cursor                   ((t (:background ,cursor                   ))))
    `(mouse                    ((t (:background ,fg-1                   ))))
    `(region                   ((t (:background ,dp_select_blue  :distant-foreground ,fg1))))
    `(highlight                ((t (:foreground ,fg3 :background ,dp_select_blue  ))))
    `(hl-line                  ((t (:background ,bg2                              ))))
    `(highlight-numbers-number ((t (:foreground ,dp_puke                          ))))
    `(line-number              ((t (:foreground ,fg7 :background ,bg1.5                              ))))
    `(line-number-current-line ((t (:foreground ,fg4 :background ,bg1.5                              ))))
    `(linum                   ((t (:background ,bg1 :foreground ,fg7   ))))
    `(fringe                   ((t (:background ,bg1 :foreground ,fg7   ))))
    `(show-paren-match    ((t (:distant-foreground ,fg1 :background ,dp_status_blue :bold t))))
    `(show-paren-mismatch    ((t (:foreground ,fg2 :background ,dp_status_purple :bold t))))
    `(isearch                  ((t (:background ,dp_found_blue :bold t ))))
    `(vertical-border          ((t (:foreground ,fg3                              ))))
    `(minibuffer-prompt        ((t (:foreground ,keyword                  ))))
    `(default-italic           ((t (:italic t                                     ))))
    `(link                     ((t (:foreground ,dp_light_blue :underline t       ))))
    `(font-lock-preprocessor-face         ((t (:foreground ,dp_dark_blue))))
    `(warning                  ((t (:foreground ,warning                          ))))
    `(ac-completion-face       ((t (:foreground ,keyword :underline t             ))))
    `(info-quoted-name         ((t (:foreground ,builtin                          ))))
    `(info-string              ((t (:foreground ,str                              ))))
    `(icompletep-determined    ((t  :foreground ,builtin                          )))
    `(ffap                     ((t (:foreground ,fg4                              ))))
    `(lazy-highlight           ((t (:foreground ,fg2 :background ,dp_found_blue             ))))
    `(trailing-whitespace      ((t  :foreground nil  :background ,warning         )))

    ;; UI
    `(widget-button               ((t (:foreground ,dp_seafoam ))))

    ;; MODE SUPPORT: org
    `(org-agenda-structure         ((t (:foreground ,fg3 :background ,bg3 :weight bold :box (:color ,fg4)))))
    `(org-agenda-date              ((t (:foreground ,var     :height 1.1             ))))
    `(org-agenda-date-weekend      ((t (:foreground ,fg4     :weight normal          ))))
    `(org-agenda-date-today        ((t (:foreground ,keyword :weight bold :height 1.4))))
    `(org-agenda-done              ((t (:foreground ,bg4                             ))))
    `(org-block                    ((t (:foreground ,fg3                             ))))
    `(org-code                     ((t (:foreground ,dp_orange                       ))))
    `(org-date                     ((t (:foreground ,var            :underline t     ))))
    `(org-document-title           ((t (:foreground ,dp_seafoam                      ))))
    `(org-document-info-keyword    ((t (:foreground ,dp_grey5                        ))))
    `(org-meta-line                ((t (:foreground ,dp_grey5                        ))))
    `(org-hide                     ((t (:foreground ,fg4                             ))))
    `(org-level-1                  ((t (:foreground ,dp_bright_blue :bold t :height 1.1))))
    `(org-level-2                  ((t (:foreground ,dp_dark_blue   :bold nil        ))))
    `(org-level-3                  ((t (:foreground ,dp_pink        :bold t          ))))
    `(org-level-4                  ((t (:foreground ,dp_green       :bold nil        ))))
    `(org-level-5                  ((t (:foreground ,dp_foreground  :bold nil        ))))
    `(org-level-6                  ((t (:foreground ,dp_foreground  :bold nil        ))))
    `(org-level-7                  ((t (:foreground ,dp_foreground  :bold nil        ))))
    `(org-level-8                  ((t (:foreground ,dp_foreground  :bold nil        ))))
    `(org-footnote                 ((t (:foreground ,fg4            :underline t     ))))
    `(org-link                     ((t (:foreground ,dp_light_blue  :underline t     ))))
    `(org-list-dt                  ((t (:foreground ,dp_light_blue  :underline t     ))))
    `(org-special-keyword          ((t (:foreground ,func                            ))))
    `(org-quote                    ((t (:inherit org-block :slant italic             ))))
    `(org-verse                    ((t (:inherit org-block :slant italic             ))))
    `(org-todo                     ((t (:foreground ,keyword :box (:line-width 1 :color ,fg3) :bold t ))))
    `(org-done                     ((t (:foreground ,bg4     :box (:line-width 1 :color ,bg3) :bold t ))))
    `(org-warning                  ((t (:foreground ,warning :underline t            ))))
    `(org-scheduled                ((t (:foreground ,type                            ))))
    `(org-scheduled-today          ((t (:foreground ,func :weight bold :height 1.2   ))))
    `(org-ellipsis                 ((t (:foreground ,dp_grey5                        ))))
	  `(org-verbatim                 ((t (:foreground ,dp_orange                       ))))
    `(org-sexp-date                ((t (:foreground ,fg4                             ))))

    ;; MODE SUPPORT: git-gutter
    `(git-gutter:added               ((t (:foreground ,dp_green      ))))
    `(git-gutter:deleted             ((t (:foreground ,dp_dark_red   ))))
    `(git-gutter:modified            ((t (:foreground ,dp_pink       ))))
    `(git-gutter:separator           ((t (:foreground ,dp_background ))))
    `(git-gutter:unchanged           ((t (:foreground ,dp_background ))))
    ;; MODE SUPPORT: git-gutter-fr
    `(git-gutter-fr:added            ((t (:foreground ,dp_green      ))))
    `(git-gutter-fr:deleted          ((t (:foreground ,dp_dark_red   ))))
    `(git-gutter-fr:modified         ((t (:foreground ,dp_pink       ))))
    ;; MODE SUPPORT: git-gutter+
    `(git-gutter+-commit-header-face ((t (:foreground ,dp_foreground ))))
    `(git-gutter+-added              ((t (:foreground ,dp_green      ))))
    `(git-gutter+-deleted            ((t (:foreground ,dp_dark_red   ))))
    `(git-gutter+-modified           ((t (:foreground ,dp_pink       ))))
    `(git-gutter+-separator          ((t (:foreground ,dp_foreground ))))
    `(git-gutter+-unchanged          ((t (:foreground ,dp_foreground ))))
    ;; MODE SUPPORT: git-gutter-fr+
    `(git-gutter-fr+-added           ((t (:foreground ,dp_green      ))))
    `(git-gutter-fr+-deleted         ((t (:foreground ,dp_dark_red   ))))
    `(git-gutter-fr+-modified        ((t (:foreground ,dp_pink       ))))


    ;; MODE LINE
    `(mode-line ((t (:foreground ,fg-1 :background ,dp_status_blue))))
    `(mode-line-2 ((t (:background ,dp_background2))))
    `(mode-line-3 ((t (:background ,dp_background))))
    `(mode-line-inactive           ((t (:foreground ,fg2 :background ,dp_status_purple))))
    `(mode-line-buffer-id          ((t (:foreground ,fg-1 :weight bold))))
    `(mode-line-buffer-id-inactive ((t (:foreground ,fg2))))
    `(mode-line-highlight          ((t (:foreground ,keyword       :background nil :weight bold        ))))
    `(mode-line-emphasis           ((t (:foreground ,fg-1           :background nil                     ))))

    ;; MODE SUPPORT: powerline
    ;; `(powerline-active1                         )
    ;; `(powerline-active2                         )
    ;; `(powerline-inactive1                       )
    ;; `(powerline-inactive2                       )

    ;; `(spacemacs-emacs-face               ((t (:background ,dp_green  ))))
    ;; `(spacemacs-insert-face              ((t (:background ,dp_dark_blue        ))))
    ;; `(spacemacs-motion-face              ((t (:background ,dp_))))
    ;; `(spacemacs-normal-face              ((t (:background ,dp_green  ))))
    ;; `(spacemacs-replace-face             ((t (:background ,dp_light_red    ))))
    ;; `(spacemacs-visual-face              ((t (:background ,dp_pink    ))))
    ;; `(spacemacs-micro-state-header-face  ((t (:background ,dp_))))
    ;; `(spacemacs-micro-state-binding-face ((t (:background ,dp_))))
    ;; `(spacemacs-lisp-face        ((t (:background ,dp_))))
    ;; `(spacemacs-transient-state-title-face ((t (:background ,dp_))))


    ;; `(spaceline-evil-emacs       ((t (:background ,dp_))))
    ;; `(spaceline-evil-insert      ((t (:background ,dp_))))
    ;; `(spaceline-evil-motion      ((t (:background ,dp_))))
    ;; `(spaceline-evil-normal      ((t (:background ,dp_))))
    ;; `(spaceline-evil-replace     ((t (:background ,dp_))))
    ;; `(spaceline-evil-visual      ((t (:background ,dp_))))
    ;; `(spaceline-flycheck-error   ((t (:background ,dp_))))
    ;; `(spaceline-flycheck-info    ((t (:background ,dp_))))
    ;; `(spaceline-flycheck-warning ((t (:background ,dp_))))
    ;; `(spaceline-highlight-face   ((t (:background ,dp_))))
    ;; `(spaceline-modified         ((t (:background ,dp_))))
    ;; `(spaceline-python-venv      ((t (:background ,dp_))))
    ;; `(spaceline-read-only        ((t (:background ,dp_))))
    ;; `(spaceline-unmodified       ((t (:background ,dp_))))


    ;; MODE SUPPORT: smart-mode-line
    ;; `(sml/modes                                 )
    ;; `(sml/minor-modes                           )
    ;; `(sml/filename                               ((t (:foreground ,dp_green))))
    ;; `(sml/prefix                                )
    ;; `(sml/git                                   )
    ;; `(sml/process                               )
    ;; `(sml/sudo                                  )
    ;; `(sml/read-only                             )
    ;; `(sml/outside-modified                      )
    ;; `(sml/modified                              )
    ;; `(sml/vc                                    )
    ;; `(sml/vc-edited                             )
    ;; `(sml/charging                              )
    ;; `(sml/discharging                           )
    ;; `(sml/col-number                            )
    ;; `(sml/position-percentage                   )

    `(rainbow-delimiters-depth-1-face   ((t :foreground ,fg1)))
    `(rainbow-delimiters-depth-2-face   ((t :foreground ,fg1)))
    `(rainbow-delimiters-depth-3-face   ((t :foreground ,fg1)))
    `(rainbow-delimiters-depth-4-face   ((t :foreground ,fg1)))
    `(rainbow-delimiters-depth-5-face   ((t :foreground ,fg1)))
    `(rainbow-delimiters-depth-6-face   ((t :foreground ,fg1)))
    `(rainbow-delimiters-depth-7-face   ((t :foreground ,fg1)))
    `(rainbow-delimiters-depth-8-face   ((t :foreground ,fg1)))
    `(rainbow-delimiters-unmatched-face ((t :foreground ,fg1)))

    `(ido-only-match  ((t  (:foreground ,warning         ))))
    `(ido-first-match ((t  (:foreground ,keyword :bold t ))))

    `(magit-item-highlight         ((t :background  ,bg3)))
    `(magit-section-highlight      ((t (:background ,bg2))))
    `(magit-hunk-heading           ((t (:background ,bg3))))
    `(magit-hunk-heading-highlight ((t (:background ,bg3))))
    `(magit-section-heading        ((t (:foreground ,keyword :weight bold     ))))
    `(magit-diff-context-highlight ((t (:foreground ,fg3     :background ,bg3 ))))
    `(magit-diff-file-header       ((t (:foreground ,fg2     :background ,bg3 ))))
    `(magit-diffstat-added         ((t (:foreground ,dp_green                 ))))
    `(magit-diffstat-removed       ((t (:foreground ,dp_light_red             ))))
    `(magit-process-ok             ((t (:foreground ,func    :weight bold     ))))
    `(magit-process-ng             ((t (:foreground ,warning :weight bold     ))))
    `(magit-branch                 ((t (:foreground ,const   :weight bold     ))))
    `(magit-log-author             ((t (:foreground ,fg3                      ))))
    `(magit-hash                   ((t (:foreground ,fg2                      ))))

    `(helm-bookmark-w3m       ((t (:foreground ,type                          ))))
    `(helm-buffer-not-saved   ((t (:foreground ,type          :background ,bg1  ))))
    `(helm-buffer-process     ((t (:foreground ,dp_seafoam    :background ,bg1  ))))
    `(helm-buffer-saved-out   ((t (:foreground ,fg1           :background ,bg1  ))))
    `(helm-buffer-size        ((t (:foreground ,fg1           :background ,bg1  ))))
    `(helm-candidate-number   ((t (:foreground ,bg1           :background ,fg1  ))))
    `(helm-ff-directory       ((t (:foreground ,dp_dark_blue  :background ,bg1 :weight bold))))
    `(helm-ff-file            ((t (:foreground ,dp_light_blue :background ,bg1 :weight normal))))
    `(helm-ff-executable      ((t (:foreground ,dp_yellow     :background ,bg1 :weight normal))))
    `(helm-ff-invalid-symlink ((t (:foreground ,warning2      :background ,bg1 :weight bold))))
    `(helm-ff-symlink         ((t (:foreground ,dp_seafoam    :background ,bg1 :weight bold))))
    `(helm-ff-prefix          ((t (:foreground ,bg1  :background ,keyword :weight normal))))
    `(helm-grep-cmd-line      ((t (:foreground ,fg1  :background ,bg1))))
    `(helm-grep-file          ((t (:foreground ,fg1  :background ,bg1))))
    `(helm-grep-finish        ((t (:foreground ,fg2  :background ,bg1))))
    `(helm-grep-lineno        ((t (:foreground ,fg1  :background ,bg1))))
    `(helm-grep-match         ((t (:foreground nil   :background nil :inherit helm-match))))
    `(helm-grep-running       ((t (:foreground ,func :background ,bg1))))
    `(helm-header             ((t (:foreground ,fg2     :background ,bg1 :underline nil :box nil))))
    `(helm-moccur-buffer      ((t (:foreground ,func    :background ,bg1))))
    `(helm-source-header      ((t (:foreground ,keyword :background ,bg1 :underline nil :weight bold))))
    `(helm-selection          ((t (:background ,dp_select_blue :underline nil))))
    `(helm-selection-line     ((t (:background ,bg2))))
    `(helm-separator          ((t (:foreground ,type    :background ,bg1))))
    `(helm-time-zone-current  ((t (:foreground ,builtin :background ,bg1))))
    `(helm-time-zone-home     ((t (:foreground ,type    :background ,bg1))))
    `(helm-visible-mark       ((t (:foreground ,bg1     :background ,bg3))))
    `(helm-source-go-package-godoc-description      ((t (:foreground ,str))))

    `(term               ((t (:foreground ,dp_foreground :background ,dp_background))))
    `(term-color-black   ((t (:foreground ,bg3           :background ,bg3))))
    `(term-color-blue    ((t (:foreground ,dp_light_blue :background ,func))))
    `(term-color-red     ((t (:foreground ,dp_light_red  :background ,bg3))))
    `(term-color-green   ((t (:foreground ,dp_green      :background ,bg3))))
    `(term-color-yellow  ((t (:foreground ,dp_yellow     :background ,var))))
    `(term-color-magenta ((t (:foreground ,dp_pink       :background ,builtin))))
    `(term-color-cyan    ((t (:foreground ,dp_seafoam    :background ,str))))
    `(term-color-white   ((t (:foreground ,fg2           :background ,fg2))))

    `(company-echo-common    ((t (:foreground ,bg1 :background ,fg1))))
    `(company-preview        ((t (:background ,bg1 :foreground ,var))))
    `(company-preview-common ((t (:foreground ,bg2 :foreground ,fg6))))
    `(company-preview-search ((t (:foreground ,type :background ,bg1))))
    `(company-scrollbar-bg   ((t (:background ,bg2))))
    `(company-scrollbar-fg   ((t (:foreground ,fg6))))
    `(company-tooltip        ((t (:foreground ,fg2 :background ,bg2))))
    `(company-tooltop-annotation       ((t (:foreground ,const))))
    `(company-tooltip-common           ((t (:foreground ,keyword :bold t))))
    `(company-tooltip-common-selection ((t (:foreground ,keyword :bold t))))
    `(company-tooltip-mouse            ((t (:inherit highlight))))
    `(company-tooltip-selection        ((t (:background ,dp_select_blue :foreground ,fg3))))
    `(company-template-field           ((t (:inherit region))))

    `(undo-tree-visualizer-current-face    ((t :foreground ,builtin)))
    `(undo-tree-visualizer-default-face    ((t :foreground ,fg2)))
    `(undo-tree-visualizer-unmodified-face ((t :foreground ,var)))
    `(undo-tree-visualizer-register-face   ((t :foreground ,type)))

    `(font-latex-bold-face                ((t (:foreground ,type))))
    `(font-latex-italic-face              ((t (:foreground ,var :italic t))))
    `(font-latex-string-face              ((t (:foreground ,str))))
    `(font-latex-subscript-face           ((t (:height 0.7))))
    `(font-latex-superscript-face         ((t (:height 0.7))))
    `(font-latex-match-reference-keywords ((t (:foreground ,const))))
    `(font-latex-match-variable-keywords  ((t (:foreground ,var))))

    `(gnus-header-content ((t (:foreground ,keyword))))
    `(gnus-header-from    ((t (:foreground ,var))))
    `(gnus-header-name    ((t (:foreground ,type))))
    `(gnus-header-subject ((t (:foreground ,func :bold t))))

    `(mu4e-view-url-number-face ((t (:foreground ,type))))
    `(mu4e-cited-1-face ((t (:foreground ,fg2))))
    `(mu4e-cited-7-face ((t (:foreground ,fg3))))
    `(mu4e-header-marks-face ((t (:foreground ,type))))

    `(tuareg-font-lock-governing-face ((t (:inherit ,font-lock-builtin-face))))
    `(tuareg-font-lock-multistage-face ((t (:inherit ,font-lock-builtin-face))))
    `(tuareg-font-lock-operator-face ((t (:inherit ,font-lock-operator-face))))
    `(tuareg-font-lock-error-face ((t (:inherit (error)))))
    `(tuareg-font-lock-interactive-output-face ((t (:inherit (font-lock-comment-face)))))
    `(tuareg-font-lock-interactive-error-face ((t (:inherit (error)))))

    `(js2-private-function-call    ((t (:foreground ,const))))
    `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,str))))
    `(js2-jsdoc-html-tag-name      ((t (:foreground ,var))))
    `(js2-jsdoc-value              ((t (:foreground ,str))))
    `(js3-jsdoc-tag-face           ((t (:foreground ,keyword))))
    `(js2-external-variable        ((t (:foreground ,type  ))))
    `(js2-function-param           ((t (:foreground ,const))))
    `(js3-function-param-face      ((t (:foreground ,fg2))))
    `(js2-private-member           ((t (:foreground ,fg3))))
    `(js3-warning-face             ((t (:underline ,keyword))))
    `(js3-error-face               ((t (:underline ,warning))))
    `(js3-external-variable-face   ((t (:foreground ,var))))
    `(js3-instance-member-face     ((t (:foreground ,const))))

    `(web-mode-builtin-face         ((t (:inherit ,font-lock-builtin-face))))
    `(web-mode-comment-face         ((t (:inherit ,font-lock-comment-face))))
    `(web-mode-constant-face        ((t (:inherit ,font-lock-constant-face))))
    `(web-mode-keyword-face         ((t (:foreground ,keyword))))
    `(web-mode-doctype-face         ((t (:inherit ,font-lock-comment-face))))
    `(web-mode-function-name-face   ((t (:inherit ,font-lock-function-name-face))))
    `(web-mode-string-face          ((t (:foreground ,str))))
    `(web-mode-type-face            ((t (:inherit ,font-lock-type-face))))
    `(web-mode-html-attr-name-face  ((t (:foreground ,func))))
    `(web-mode-html-attr-value-face ((t (:foreground ,keyword))))
    `(web-mode-warning-face         ((t (:inherit ,font-lock-warning-face))))
    `(web-mode-html-tag-face        ((t (:foreground ,builtin))))

    `(jde-java-font-lock-package-face   ((t (:foreground ,var))))
    `(jde-java-font-lock-public-face    ((t (:foreground ,keyword))))
    `(jde-java-font-lock-private-face   ((t (:foreground ,keyword))))
    `(jde-java-font-lock-constant-face  ((t (:foreground ,const))))
    `(jde-java-font-lock-modifier-face  ((t (:foreground ,fg2))))
    `(jde-jave-font-lock-protected-face ((t (:foreground ,keyword))))
    `(jde-java-font-lock-number-face    ((t (:foreground ,var))))

    )
  (setq-default ansi-color-names-vector
                (vconcat (list
                          bg1 dp_light_red dp_green "#b09f20"
                          dp_dark_blue dp_pink dp_bright_blue fg1)))
  ;;(setq-default ansi-color-map (ansi-color-make-color-map))
  t)

(provide-theme 'Dark)