
;; Fix centaur tab move issue
(evil-define-key 'normal centaur-tabs-mode-map (kbd "g t") 'centaur-tabs-forward        ; default Doom binding is 'g t'
                                               (kbd "g T")  'centaur-tabs-backward       ; default Doom binding is 'g T'
                                               (kbd "g <down>")  'centaur-tabs-forward-group
                                               (kbd "g <up>")    'centaur-tabs-backward-group)
(map!
 (:map override
  "M-1" #'+neotree/open)
 )
