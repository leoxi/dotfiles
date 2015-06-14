(require-package 'eshell-prompt-extras)
(with-eval-after-load "esh-opt"
  (mapc 'require '(em-term em-smart))
  (setq eshell-cmpl-ignore-case t
        eshell-cmpl-cycle-completions nil
        eshell-save-history-on-exit t
        eshell-history-size 1024
        eshell-buffer-shorthand t
        eshell-banner-message ""
        eshell-rc-script "~/.emacs.d/profile"
        eshell-aliases-file "~/.emacs.d/alias"
        eshell-cp-interactive-query t
        eshell-ln-interactive-query t
        eshell-mv-interactive-query t
        eshell-mv-overwrite-files nil
        eshell-review-quick-commands t)

  (when (require 'virtualenvwrapper nil t)
    (venv-initialize-eshell))
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda)

  (defun eshell/cdu ()
    "Change the directory to the base of the project."
    (eshell/cd (or (locate-dominating-file default-directory "src")
                   (locate-dominating-file default-directory ".git"))))

  (defun eshell/cdd (&optional dir)
    "Change the directory as deep as possible if there is only one directory."
    (or (not dir) (eshell/cd dir))
    (let ((dirfiles (directory-files (eshell/pwd) nil "[^^\\.$\\|^\\..$]" t)))
      (when (= (length dirfiles) 1)
        (let ((firstfile (car dirfiles)))
          (when (file-directory-p firstfile)
            (eshell/cd firstfile)
            (eshell/cdd))))))

  (defun eshell/cdt (&rest args)
    "eshell/cd in remote directorys."
    (when (tramp-tramp-file-p default-directory)
      (let* ((parsed (coerce (tramp-dissect-file-name default-directory) 'list))
             (dir
              (if args
                  (if (s-starts-with? "/" (car args))
                      (apply 'tramp-make-tramp-file-name
                             (-replace-at 3 (car args) parsed))
                    (apply 'tramp-make-tramp-file-name
                           (-replace-at 3 (concat (nth 3 parsed) (car args)) parsed)))
                (apply 'tramp-make-tramp-file-name
                       (-replace-at 3 "~" parsed)))))
        (eshell/cd dir))))

  (defun eshell-next ()
    (interactive)
    (let ((buffers (sort (--filter (with-current-buffer it
                                     (eq major-mode 'eshell-mode))
                                   (buffer-list))
                         (lambda (l r) (string< (buffer-name l) (buffer-name r))))))
      (pop-to-buffer-same-window (or
                                  (nth (1+ (-elem-index (current-buffer) buffers)) buffers)
                                  "*eshell*"))))

  (defadvice eshell-ls-decorated-name (after add-fancy-symbol (file) activate)
    (cond
     ((file-symlink-p ad-return-value)
      (setq ad-return-value (concat ad-return-value "@")))
     ((file-directory-p ad-return-value)
      (unless (s-ends-with? "/" ad-return-value)
        (setq ad-return-value (concat ad-return-value "/"))))
     ((file-executable-p ad-return-value)
      (setq ad-return-value (concat ad-return-value "*"))))
    ad-return-value)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-smart-initialize)
              (define-key eshell-mode-map (kbd "C-c n") 'eshell-next)
              (define-key eshell-mode-map (kbd "C-c h")
                (lambda ()
                  (interactive)
                  (insert
                   (ido-completing-read "History: "
                                        (delete-dups
                                         (ring-elements eshell-history-ring)))))))))

(global-set-key (kbd "C-x M") 'shell)
(defun fun-eshell () (interactive) (eshell t))

(defun eshell-pop ()
  (interactive)
  (unless (get-buffer eshell-buffer-name)
    (save-window-excursion
      (pop-to-buffer (get-buffer-create eshell-buffer-name))
      (eshell-mode)))
  (popwin:popup-buffer (get-buffer eshell-buffer-name) :height 20))
(global-set-key (kbd "C-x m") 'eshell-pop)
