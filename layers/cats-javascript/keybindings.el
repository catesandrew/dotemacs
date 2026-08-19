;;; keybindings.el --- cats: Programming -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal functions

;;; Code:

(spacemacs/declare-prefix "oj" "jump/join/split")

(spacemacs/declare-prefix "on" "node/react-native")
(spacemacs/set-leader-keys
  "onm" 'cats/react-native-metro-start
  "onM" 'cats/react-native-metro-stop
  "oni" 'cats/react-native-log-ios
  "ona" 'cats/react-native-log-android
  "ond" 'cats/react-native-open-debugger)

;;; keybindings.el ends here
