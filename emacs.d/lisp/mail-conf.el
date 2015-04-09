(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(autoload 'mu4e "mu4e" "" t)
(setq mu4e-maildir "~/.Maildir"
      mu4e-attachment-dir  "~/Downloads"
      mu4e-maildir-shortcuts '(("/inbox" . ?i)
                               ("/sent" . ?s)
                               ("/drafts" . ?d)
                               ("/trash" . ?t)
                               ("/all" . ?a)
                               ("/amazon" . ?z)
                               ("/github" . ?g)
                               ("/scala" . ?l))
      mu4e-headers-fields '((:human-date . 9)
                            (:flags . 4)
                            (:from . 20)
                            (:cc . 12)
                            (:subject))
      mu4e-get-mail-command "offlineimap"
      mu4e-headers-leave-behavior 'apply
      mu4e-sent-messages-behavior 'delete
      mu4e-compose-dont-reply-to-self t
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-confirm-quit nil
      mu4e-compose-signature "\n-- Wei"
      mu4e-html2text-command "w3m -T text/html"

      user-mail-address "kaihaosw@gmail.com"
      user-full-name  "Wei Zhao"
      message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      message-kill-buffer-on-exit t)
(global-set-key (kbd "C-c m") 'mu4e)
