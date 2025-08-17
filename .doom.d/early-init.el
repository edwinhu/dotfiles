;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27+ introduces early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;; AGGRESSIVE FIX: Completely disable native compilation to fix 
;; "Hot loading function masking core function" error with emacs-jupyter
(when (fboundp 'native-comp-available-p)
  (when (native-comp-available-p)
    ;; Disable all native compilation
    (setq native-comp-jit-compilation nil)
    (setq native-comp-deferred-compilation nil)
    
    ;; Also set deny lists as backup
    (setq native-comp-jit-compilation-deny-list
          '(".*\\.el$"))  ; Deny all .el files
    (setq comp-deferred-compilation-deny-list
          '(".*\\.el$"))))