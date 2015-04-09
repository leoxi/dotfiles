;;; kindle.el --- kindle utils

;; Author: Wei Zhao <kaihaosw@gmail.com>
;; Created: 2015-01-22

(provide 'kindle)
(require 's)
(require 'dash)

(defvar kindle-position "/Volumes/Kindle/documents/"
  "Kindle position used when remove the sdr files.")

(defvar kindle-extensions '(".mobi" ".azw" ".azw3" ".pdf" ".txt")
  "Kindle file extensions.")

(defvar kindle-get-identifier-command
  "diskutil list | grep Kindle | awk '/disk[1-9]s/{ print $NF }'"
  "Command that get kindle's identifier.")

(defun kindle-identifier ()
  (s-chomp (shell-command-to-string kindle-get-identifier-command)))

(defun kindle-unmount ()
  "Unmount your kindle."
  (interactive)
  (start-process "diskutil" nil "diskutil" "unmount" (kindle-identifier)))

(defun kindle-mount ()
  "Mount your kindle."
  (interactive)
  (start-process "diskutil" nil "diskutil" "mount" (kindle-identifier)))

(defun kindle-eject ()
  "Eject your kindle."
  (interactive)
  (start-process "diskutil" nil "diskutil" "eject" (kindle-identifier)))

(defun kindle-remove-redundant-sdrs ()
  "Clean all your redundant str files."
  (interactive)
  (if (file-directory-p kindle-position)
      (let ((sdrfiles (directory-files kindle-position nil "\.sdr$")))
        (-map (lambda (f)
                (let ((files-maybe (--map
                                    (concat kindle-position (file-name-base f) it)
                                    kindle-extensions)))
                  (unless (--filter (file-exists-p it) files-maybe)
                    (let ((sdr (concat kindle-position f)))
                      (message (format "Deleting %s." sdr))
                      (delete-directory sdr t)))))
              sdrfiles)
        (message "Done cleaning."))
    (error "Kindle not exist.")))

;;; kindle.el ends here
