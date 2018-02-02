(when (configuration-layer/package-usedp 'buffer-move)
  (let ((map1 (lookup-key spacemacs-default-map "bm")))
    (spacemacs/set-leader-keys "bk" map1))

  (spacemacs/set-leader-keys "bm" nil)
  (spacemacs/declare-prefix "bm" "move(buffer)")
  (spacemacs/set-leader-keys
    "bmh" 'buf-move-left
    "bmj" 'buf-move-down
    "bmk" 'buf-move-up
    "bml" 'buf-move-right))
