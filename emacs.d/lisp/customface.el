(with-eval-after-load "custom"
  (load-theme 'base16-ocean-dark t)
  (let ((base00 "#2b303b") (base01 "#343d46")
        (base02 "#4f5b66") (base03 "#65737e")
        (base04 "#a7adba") (base05 "#c0c5ce")
        (base06 "#dfe1e8") (base07 "#eff1f5")
        (base08 "#bf616a") (base09 "#d08770")
        (base0A "#ebcb8b") (base0B "#a3be8c")
        (base0C "#96b5b4") (base0D "#8fa1b3")
        (base0E "#b48ead") (base0F "#ab7967"))
    (custom-set-faces
     `(mode-line ((t (:background ,base01))))
     `(mode-line-inactive ((t (:background ,base02))))
     `(fringe ((t (:background nil :foreground ,base03))))
     `(linum ((t (:background nil))))
     `(vertical-border ((t (:foreground ,base02))))
     `(eshell-ls-directory ((t (:foreground ,base0C))))
     `(eshell-ls-readonly ((t (:foreground ,base05))))
     `(eshell-ls-product ((t (:foreground ,base05))))
     `(eshell-ls-unreadable ((t (:foreground ,base05))))
     `(eshell-ls-backup ((t (:foreground ,base05))))
     `(eshell-ls-archive ((t (:foreground ,base05))))
     `(eshell-ls-clutter ((t (:foreground ,base05))))
     `(eshell-ls-missing ((t (:foreground ,base05))))
     `(eshell-ls-symlink ((t (:foreground ,base0E))))
     `(eshell-ls-executable ((t (:foreground ,base08))))
     `(eshell-ls-special ((t (:foreground ,base0B))))
     `(anzu-mode-line ((t (:foreground ,base0E))))
     `(idle-highlight ((t (:background ,base03))))
     `(company-tooltip ((t (:inherit highlight))))
     `(company-tooltip-selection ((t (:inherit match))))
     `(company-tooltip-common ((t (:inherit company-tooltip :weight bold))))
     `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold))))
     `(company-echo ((t (:inherit minibuffer-prompt))))
     `(company-echo-common ((t (:inherit minibuffer-noticeable-prompt))))
     `(company-preview ((t (:inherit primary-selection))))
     `(company-preview-common ((t (:inherit company-preview :weight bold))))
     `(company-preview-search ((t (:inherit secondary-selection))))
     `(company-scrollbar-fg ((t (:inherit region))))
     `(company-scrollbar-bg ((t (:inherit company-tooltip)))))))
