(require-package 'erc-hl-nicks)
(with-eval-after-load "erc"
  (setq erc-prompt ">"
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 14
        erc-port 8001
        erc-nick "kaihaosw"
        erc-hide-list '("NICK" "JOIN" "LEAVE" "QUIT" "PART")
        erc-track-exclude-types (append erc-hide-list
                                        '("301" "305" "306" "324"
                                          "329" "332" "333" "353"))
        erc-autojoin-channels-alist '(("freenode.net" "#scala" "#scheme" "#javascript" "#typescript"))))
