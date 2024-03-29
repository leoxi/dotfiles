(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/themes")

(setq inhibit-startup-screen t
      disabled-command-function 'ignore
      custom-file "~/.emacs.d/custom.el"
      confirm-nonexistent-file-or-buffer nil
      frame-title-format '(buffer-file-name "%f" ("%b"))
      vc-follow-symlinks t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      scroll-conservatively 9999)
(setq-default indicate-empty-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defun require-package (ps)
  (mapc (lambda (p) (or (package-installed-p p) (package-install p)))
        (if (listp ps) ps (list ps))))

(require-package '(better-defaults cl s dash))
(autoload 'server-running-p "server")
(unless (server-running-p) (server-start))
(load custom-file t)
(tooltip-mode -1)
(blink-cursor-mode -1)
(fringe-mode 4)
(global-prettify-symbols-mode t)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "<f10>") 'toggle-frame-fullscreen)
(global-unset-key (kbd "C-z"))
(when (member "Consolas" (font-family-list))
  (set-face-font 'default "Consolas-13"))

(when (eq system-type 'darwin)
  (load "mac-conf.el")
  (menu-bar-mode t)
  (require 'kindle))

(require-package 'smex)
(smex-initialize)
(global-set-key (kbd "C-x C-m") 'smex)

(require-package '(ido-vertical-mode flx-ido))
(setq ido-save-directory-list-file nil
      ido-enter-matching-directory 'first
      ido-use-filename-at-point 'guess
      ido-use-faces nil
      ido-vertical-define-keys 'C-n-and-C-p-only
      ido-vertical-show-count t)
(ido-vertical-mode t)
(flx-ido-mode t)

(setq recentf-auto-cleanup 'never
      recentf-max-saved-items 200
      recentf-exclude '("COMMIT_EDITMSG" ".*-autoloads\\.el\\'"))
(recentf-mode t)
(defun recentf-ido-find-file ()
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (file (ido-completing-read
                "Choose recent file: "
                (--map (replace-regexp-in-string home "~" it) recentf-list)
                nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

(with-eval-after-load "hippie-exp"
  (defun try-expand-dabbrev-matching-buffers (old)
    (let ((matching-buffers (--filter
                             (cond ((eq major-mode 'inferior-python-mode)
                                    (with-current-buffer it
                                      (eq major-mode 'python-mode)))
                                   ((eq major-mode 'sbt-mode)
                                    (with-current-buffer it
                                      (eq major-mode 'scala-mode)))
                                   ((eq major-mode 'inferior-scheme-mode)
                                    (with-current-buffer it
                                      (eq major-mode 'scheme-mode)))
                                   (:t (eq major-mode
                                           (with-current-buffer it major-mode))))
                             (buffer-list))))
      (flet ((buffer-list () matching-buffers)) (try-expand-dabbrev-all-buffers old))))

  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-matching-buffers
                                           try-expand-dabbrev-from-kill
                                           try-expand-all-abbrevs)))

(with-eval-after-load "ibuffer"
  (defun ibuffer-project ()
    (-mapcat (lambda (dir)
               (--map (list (concat "Project: " (file-name-base it))
                            `(filename . ,it))
                      (directory-files dir 'full "^[^\.]")))
             '("~/fun/" "~/work/" "~/Dropbox/codes/")))

  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        (list
         (cons "default"
               (append
                '(("Shell" (or (mode . eshell-mode)
                               (mode . shell-mode))))
                (ibuffer-project)
                '(("Magit" (derived-mode . magit-mode))
                  ("ERC" (mode . erc-mode))
                  ("Mail" (or (mode . mu4e-compose-mode)
                              (mode . mu4e-headers-mode)
                              (mode . mu4e-view-mode)
                              (mode . mu4e-main-mode)
                              (name . "mu4e-update"))))))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode t)
              (ibuffer-do-sort-by-filename/process)
              (ibuffer-switch-to-saved-filter-groups "default"))))

(setq display-time-default-load-average nil
      display-time-use-mail-icon t
      display-time-format "%H:%M %m/%d(%a)")
(display-time-mode t)

(require-package 'diminish)
(with-eval-after-load "simple" (diminish 'auto-fill-function))
(with-eval-after-load "hi-lock" (diminish 'hi-lock-mode))
(with-eval-after-load "abbrev" (diminish 'abbrev-mode))
(with-eval-after-load "autorevert" (diminish 'auto-revert-mode))
(with-eval-after-load "isearch" (diminish 'isearch-mode))
(with-eval-after-load "eldoc" (diminish 'eldoc-mode))

(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require-package 'idomenu)
(global-set-key (kbd "C-x C-i") 'idomenu)

(require-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(diminish 'smartparens-mode)
(define-key sp-keymap (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-(") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-}") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-{") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)

(require-package '(magit magit-gitflow))
(with-eval-after-load "magit"
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-mode-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))
  (autoload 'turn-on-magit-gitflow "magit-gitflow")
  (with-eval-after-load "magit-gitflow" (diminish 'magit-gitflow-mode))
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))
(global-set-key (kbd "C-c g") 'magit-status)

(require-package '(gitignore-mode gitconfig-mode git-timemachine pcmpl-git))
(with-eval-after-load "git-timemachine" (diminish 'git-timemachine-mode))
(require-package '(markdown-mode yaml-mode))

(require-package 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'respectful)

(require-package 'anzu)
(global-anzu-mode t)
(diminish 'anzu-mode)
(setq anzu-search-threshold 1000
      anzu-replace-to-string-separator " => ")

(require-package 'ag)
(with-eval-after-load "ag"
  (setq ag-highlight-search t
        ag-reuse-buffers t))

(require-package 'nyan-mode)
(setq nyan-bar-length 20)
(nyan-mode t)

(require-package 'company)
(with-eval-after-load "company"
  (setq company-minimum-prefix-length 2
        company-show-numbers t
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-require-match nil)
  (diminish 'company-mode)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(require-package 'flycheck)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
(global-flycheck-mode t)
(diminish 'flycheck-mode)

(require-package 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode t)
(diminish 'yas-minor-mode)

(require-package '(popwin import-popwin))
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(push '(inferior-python-mode :stick t) popwin:special-display-config)
(push '("*anaconda-doc*" :stick t :noselect t) popwin:special-display-config)
(push '(inferior-scheme-mode :stick t) popwin:special-display-config)
(push '("*ag search*" :stick t) popwin:special-display-config)
(push '("*nodejs*" :stick t) popwin:special-display-config)
(push '(Man-mode :stick t) popwin:special-display-config)
(push '("*ensime-update*" :stick t) popwin:special-display-config)
(push '("*inferior-ensime-server-root*" :stick t) popwin:special-display-config)
(push '("*ensime-inferior-scala*" :stick t) popwin:special-display-config)
(push '("*ENSIME-Compilation-Result*" :stick t) popwin:special-display-config)
(push '("^\*sbt*" :regexp t :stick t) popwin:special-display-config)
(push '("^\*ensime-stacktrace*" :regexp t :stick t) popwin:special-display-config)
(popwin-mode t)
(global-set-key (kbd "C-z") popwin:keymap)

(require-package 'discover-my-major)
(global-set-key (kbd "C-h C-m") 'discover-my-major)

(require-package 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(require-package 'dash-at-point)
(global-set-key (kbd "C-h C-d") 'dash-at-point)

(load "util.el")
(load "shells-conf.el")
(load "mail-conf.el")
(load "erc-conf.el")
(load "programming.el")
(load "customface.el")
