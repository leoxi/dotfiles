(require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "WORKON_HOME")
(exec-path-from-shell-copy-env "PYTHONIOENCODING")

(setq mac-option-modifier 'super
      mac-command-modifier 'meta)

(defun ns-get-pasteboard ()
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(defun fun-dict (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((s (s-replace
            "-" " "
            (if (and beg end)
                (buffer-substring-no-properties beg end)
              (substring-no-properties (thing-at-point 'word))))))
    (start-process "Dict" nil "open" (format "dict://%s" s))))
(global-set-key (kbd "s-t") 'fun-dict)

(require-package 'pcmpl-homebrew)
