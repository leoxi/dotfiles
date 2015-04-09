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
  (require 'eshell-prompt-extras)
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

  (defun eshell/sbt-create-project (project &optional gitignore-p scala-version)
    "Create a scala sbt project with build and gitignore files."
    (defun create-build-file ()
      (append-to-file (format "%s\n  %s\n    %s%s\n    %s\n    %s%s\n  )"
                              "lazy val root = (project in file(\"\.\"))."
                              "settings("
                              "name := "
                              (format "\"%s\"," project)
                              "version := \"0.1\","
                              "scalaVersion := "
                              (format "\"%s\"" (or scala-version "2.11.6")))
                      nil
                      (concat project "/build.sbt")))
    (defun create-gitignore-file ()
      (append-to-file (format "%s%s%s%s%s"
                              "*.class\n*.log\n\n"
                              "# sbt specific\n.cache\n.history\n.lib/\ndist/*\n"
                              "target/lib_managed/\nsrc_managed/\nproject"
                              "/boot/\nproject/plugins/project/\n\n"
                              "# Scala-IDE specific\n.scala_dependencies\n.worksheet")
                      nil
                      (concat project "/.gitignore")))
    (if (file-exists-p project)
        (error (format "Directory %s already exists." project))
      (progn
        (--map
         (mkdir (concat project it) t)
         '("/src/main/scala/com/kaihaosw" "/src/test/scala/com/kaihaosw"
           "/lib" "/project" "/target"))
        (create-build-file)
        (unless gitignore-p (create-gitignore-file))
        (eshell/echo (format "Project %s initialized." project)))))

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
              (define-key eshell-mode-map (kbd "C-c h")
                (lambda ()
                  (interactive)
                  (insert
                   (ido-completing-read "History: "
                                        (delete-dups
                                         (ring-elements eshell-history-ring)))))))))

(defun fun-eshell () (interactive) (eshell t))

(defun eshell-switch-or-create ()
  (interactive)
  (if buffer-file-name
      (let* ((buffer buffer-file-name)
             (result (--filter
                      (with-current-buffer it
                        (and (eq major-mode 'eshell-mode)
                             (string= (file-name-directory buffer)
                                      (concat (eshell/pwd it) "/"))))
                      (buffer-list))))
        (if (null result)
            (eshell t)
          (pop-to-buffer-same-window (car result))))
    (eshell)))

(global-set-key (kbd "C-x m") 'eshell-switch-or-create)
(global-set-key (kbd "C-x M") 'shell)
