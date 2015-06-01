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
            (idle-highlight-mode t)
            (push '("lambda" . 955) prettify-symbols-alist)
            (company-mode t)
            (define-key prog-mode-map (kbd "C-\\") 'company-complete)
            (font-lock-add-keywords
             nil `(("\\<\\(FIXME\\|TODO\\)" 1 'font-lock-warning-face prepend)))))

;; web
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(require-package '(web-mode))
(with-eval-after-load "web-mode"
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing nil
        web-mode-engines-alist '(("jinja" . "\\.html?\\'")))
  (with-eval-after-load "smartparens-config"
    (sp-with-modes '(web-mode)
      (sp-local-pair "{{" " }}")
      (sp-local-pair "{#" " #}")
      (sp-local-pair "{%" " %}"))))

;; javascript
(require-package '(js2-mode tern company-tern skewer-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(with-eval-after-load "js2-mode"
  (setq-default js2-basic-offset 2)
  (setq js2-missing-semi-one-line-override t)

  (with-eval-after-load "tern"
    (setq tern-command (append tern-command '("--no-port-file")))
    (diminish 'tern-mode))
  (add-to-list 'company-backends 'company-tern)

  (with-eval-after-load "skewer-mode"
    (define-key skewer-mode-map (kbd "C-`") 'run-skewer)
    (define-key skewer-mode-map (kbd "C-c C-e") 'skewer-eval-defun)
    (define-key skewer-mode-map (kbd "C-c C-b") 'skewer-load-buffer)
    (diminish 'skewer-mode))

  (add-hook 'js2-mode-hook
            (lambda ()
              (setq mode-name "js2")
              (js2-mode-hide-warnings-and-errors)
              (tern-mode)
              (skewer-mode)
              (push '("function" . 402) prettify-symbols-alist))))

;; python
(require-package '(virtualenvwrapper pcmpl-pip py-autopep8))
(require-package '(anaconda-mode company-anaconda))
(with-eval-after-load "python"
  (with-eval-after-load "anaconda-mode"
    (diminish 'anaconda-mode)

    (defadvice anaconda-mode-doc-buffer (after change-mode activate)
      (with-current-buffer "*anaconda-doc*"
        (rst-mode))))

  (add-to-list 'company-backends 'company-anaconda)

  (defun python-insert-pdb ()
    (interactive)
    (insert "\n")
    (python-indent-line)
    (insert "import pdb; pdb.set_trace()\n"))

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
              (anaconda-mode t)
              (eldoc-mode t))))

;; scala
;; (require-package '(scala-mode2 sbt-mode))
;; (with-eval-after-load "scala-mode2"
;;   (require 'sbt-mode)
;;   (setq compilation-skip-threshold 1)
;;   (define-key sbt:mode-map (kbd "C-a") 'comint-bol)
;;   (define-key sbt:mode-map (kbd "M-RET") 'comint-accumulate)

;;   (defun sbt-start ()
;;     (interactive)
;;     (sbt:run-sbt)
;;     (when (comint-check-proc (sbt:buffer-name))
;;       (pop-to-buffer (sbt:buffer-name))))

;;   (defun sbt-send-buffer ()
;;     (interactive)
;;     (sbt-send-region (point-min) (point-max)))

;;   (defun sbt-add-scala-source-files ()
;;     (let ((source "/usr/local/Cellar/scala/2.11.6/libexec/src/library")
;;           (target (concat (sbt:find-root) ".source")))
;;       (cond ((not (file-exists-p source))
;;              (error (concat source " not exists!")))
;;             ((not (file-exists-p target))
;;              (make-directory target)
;;              (copy-directory source target)))))

;;   (defun scala-toggle-test ()
;;     (interactive)
;;     (let ((bf buffer-file-name))
;;       (find-file
;;        (if (s-match "Test.scala$" bf)
;;            (s-replace-all '(("/src/test/" . "/src/main/")
;;                             ("Test.scala" . ".scala")) bf)
;;          (s-replace-all '(("/src/main/" . "/src/test/")
;;                           (".scala" . "Test.scala")) bf)))))

;;   (defun scala-guess-package-from-path ()
;;     (s-replace
;;      "/" "."
;;      (s-chop-suffix
;;       "/"
;;       (replace-regexp-in-string "^.*/src/\\(main\\|test\\)/scala/" ""
;;                                 (file-name-directory buffer-file-name)))))

;;   (defun scala-package-name-from-path ()
;;     (interactive)
;;     (save-excursion
;;       (goto-char 0)
;;       (when (looking-at "package ")
;;         (kill-line 1))
;;       (insert (format "package %s\n" (scala-guess-package-from-path)))))

;;   (add-hook 'scala-mode-hook
;;             (lambda ()
;;               (when (sbt:find-root)
;;                 (sbt-add-scala-source-files))))

;;   (define-key scala-mode-map (kbd "C-`") 'sbt-start)
;;   (define-key scala-mode-map (kbd "C-c C-z") 'sbt-start)
;;   (define-key scala-mode-map (kbd "C-c C-r") 'sbt-send-region)
;;   (define-key scala-mode-map (kbd "C-c C-b") 'sbt-send-buffer)
;;   (define-key scala-mode-map (kbd "C-c C-k") 'sbt-command)
;;   (define-key scala-mode-map (kbd "M-.") 'sbt-find-definitions))

;; scheme
(with-eval-after-load "scheme"
  (setq scheme-program-name "petite"
        scheme-macro-expand-command "(expand `%s)")
  (require 'iuscheme)

  (defadvice run-scheme (before split-window activate)
    (split-window-sensibly (selected-window))
    (other-window 1))

  (define-key scheme-mode-map (kbd "C-`") 'run-scheme)
  (define-key scheme-mode-map (kbd "C-c C-z") 'run-scheme))
