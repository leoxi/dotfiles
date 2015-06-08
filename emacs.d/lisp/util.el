(defun fun-upgrade-packages ()
  (interactive)
  (progn
    (package-list-packages)
    (package-menu-mark-upgrades)
    (package-menu-mark-obsolete-for-deletion)
    (package-menu-execute)
    (when (yes-or-no-p "Killing package list?")
      (kill-buffer "*Packages*"))))

(defun fun-compile-init-files ()
  (interactive)
  (byte-compile-file "~/.emacs.d/init.el")
  (byte-recompile-directory "~/.emacs.d/lisp" 0)
  (byte-recompile-directory "~/.emacs.d/site-lisp" 0))

(defun fun-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun fun-clean-buffer ()
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(global-set-key (kbd "C-c TAB")
                (defun fun-indent ()
                  (interactive)
                  (if (region-active-p)
                      (indent-region (region-beginning) (region-end))
                    (indent-region (point-min) (point-max)))))

(global-set-key (kbd "C-c d")
                (defun fun-top-join-line ()
                  (interactive)
                  (delete-indentation 1)))

;; https://github.com/capitaomorte/holy/blob/master/etc/emacs/common/50extra.el
(defun fun-find-file-in-project ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (when default-directory
      (with-temp-buffer
        (if (zerop (call-process "git" nil t nil "ls-files"
                                 "--cached" "--other" "--exclude-standard" "--full-name"))
            (find-file (ido-completing-read
                        (format "Find file project [%s] : "
                                (file-name-nondirectory
                                 (directory-file-name default-directory)))
                        (split-string (buffer-string) "\n" t)))
          (error "git ls-files failed"))))))
(global-set-key (kbd "C-x f") 'fun-find-file-in-project)

;; https://github.com/Silex/emacs-config/blob/master/config/tramp.el
(defun fun-sudo-edit-file ()
  (interactive)
  (defun local-file-name-as-sudo (file-name)
    (concat "/sudo::" file-name))

  (defun tramp-file-name-as-sudo (file-name)
    (let* ((parts (tramp-dissect-file-name file-name))
           (host (tramp-file-name-host parts)))
      (setq file-name (replace-regexp-in-string (regexp-quote (concat host ":"))
                                                (concat host "|sudo:" host ":")
                                                file-name t t))
      (setq file-name (replace-regexp-in-string "^/scp" "/ssh" file-name))))

  (defun buffer-file-name-as-sudo (&optional buffer)
    (let* ((buffer (or buffer (current-buffer)))
           (file-name (or (buffer-file-name buffer) dired-directory)))
      (if (tramp-tramp-file-p file-name)
          (tramp-file-name-as-sudo file-name)
        (local-file-name-as-sudo file-name))))
  (find-alternate-file (buffer-file-name-as-sudo)))
