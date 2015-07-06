(require-package 'idle-highlight-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (linum-mode t)
            (set (make-local-variable 'comment-auto-fill-only-comments) t)
            (make-local-variable 'column-number-mode)
            (column-number-mode t)
            (auto-fill-mode t)
            (setq show-trailing-whitespace t)
            (add-hook 'after-save-hook 'check-parens nil t)
            (eldoc-mode t)
            (idle-highlight-mode t)
            (push '("lambda" . 955) prettify-symbols-alist)
            (company-mode t)
            (define-key prog-mode-map (kbd "C-\\") 'company-complete)
            (font-lock-add-keywords
             nil `(("\\<\\(FIXME\\|TODO\\)" 1 'font-lock-warning-face prepend)))))

;; web
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(require-package '(web-mode company-web))
(with-eval-after-load "web-mode"
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing nil
        web-mode-enable-auto-quoting nil
        web-mode-engines-alist '(("jinja" . "\\.html?\\'")))

  (add-to-list 'company-backends 'company-web-html)

  (with-eval-after-load "smartparens-config"
    (sp-with-modes '(web-mode)
      (sp-local-pair "{{" " }}")
      (sp-local-pair "{#" " #}")
      (sp-local-pair "{%" " %}"))))

(with-eval-after-load "css-mode"
  (setq css-indent-offset 2)
  (add-hook 'css-mode-hook
            (lambda ()
              (company-mode t)
              (define-key css-mode-map (kbd "C-\\") 'company-complete))))

;; javascript
(require-package '(js2-mode tern company-tern json-reformat nodejs-repl))
(add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)$" . js2-mode))
(with-eval-after-load "js2-mode"
  (setq-default js2-basic-offset 2)
  (setq js2-missing-semi-one-line-override t)

  (with-eval-after-load "tern"
    (setq tern-command (append tern-command '("--no-port-file")))
    (diminish 'tern-mode)
    (define-key tern-mode-keymap (kbd "C-c C-r") 'js-send-region)
    (define-key tern-mode-keymap (kbd "C-c r") 'tern-rename-variable)
    (define-key tern-mode-keymap (kbd "M-?") 'tern-get-docs)
    (define-key tern-mode-keymap (kbd "M-t") 'tern-get-type))
  (add-to-list 'company-backends 'company-tern)

  (defvar inf-js-buffer "*nodejs*")

  (defun js-send-region (start end)
    (interactive "r")
    (comint-send-region inf-js-buffer start end)
    (comint-send-string inf-js-buffer "\n"))

  (defun js-send-buffer ()
    (interactive)
    (js-send-region (point-min) (point-max)))

  (defun js-load-file (filename)
    (interactive "f")
    (let ((filename (expand-file-name filename)))
      (comint-send-string inf-js-buffer (concat "require(\"" filename "\")\n"))))

  (defun switch-to-js ()
    (interactive)
    (if (comint-check-proc inf-js-buffer)
        (switch-to-buffer-other-window inf-js-buffer)
      (nodejs-repl)))

  (define-key js2-mode-map (kbd "C-`") 'nodejs-repl)
  (define-key js2-mode-map (kbd "C-c C-r") 'js-send-region)
  (define-key js2-mode-map (kbd "C-c C-b") 'js-send-buffer)
  (define-key js2-mode-map (kbd "C-c C-l") 'js-load-file)
  (define-key js2-mode-map (kbd "C-c C-z") 'switch-to-js)

  (add-hook 'js2-mode-hook
            (lambda ()
              (setq mode-name "js2")
              (js2-mode-hide-warnings-and-errors)
              (tern-mode)
              (push '("function" . 402) prettify-symbols-alist))))

;; python
(require-package '(virtualenvwrapper pcmpl-pip py-autopep8))
(require-package '(anaconda-mode company-anaconda))
(with-eval-after-load "python"
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--classic")
  (with-eval-after-load "anaconda-mode"
    (diminish 'anaconda-mode)

    (defadvice anaconda-mode-doc-buffer (after change-mode activate)
      (with-current-buffer "*anaconda-doc*"
        (rst-mode)))

    (define-key anaconda-mode-map (kbd "M-,") 'anaconda-nav-pop-marker))

  (add-to-list 'company-backends 'company-anaconda)

  (defun fun-run-python ()
    (interactive)
    (let ((buffer-proc (format "*%s*" (python-shell-get-process-name nil))))
      (if (comint-check-proc buffer-proc)
          (switch-to-buffer-other-window buffer-proc)
        (run-python (read-string "Run Python: " (python-shell-parse-command)) nil t))))

  (define-key python-mode-map (kbd "C-c TAB") '(lambda () (interactive)))
  (define-key python-mode-map (kbd "C-`") 'fun-run-python)
  (define-key python-mode-map (kbd "C-c C-z") 'fun-run-python)
  (define-key python-mode-map (kbd "C-c C-e") 'python-shell-send-defun)
  (define-key python-mode-map (kbd "C-c C-b") 'python-shell-send-buffer)

  (add-hook 'python-mode-hook
            (lambda ()
              (anaconda-mode t))))

;; scheme
(with-eval-after-load "scheme"
  (setq scheme-program-name "petite"
        scheme-macro-expand-command "(expand `%s)")
  (require 'iuscheme)

  (define-key scheme-mode-map (kbd "C-`") 'run-scheme)
  (define-key scheme-mode-map (kbd "C-c C-z") 'run-scheme))
