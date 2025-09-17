;;; early-init.el --- Early initialization for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; Performance optimizations and early setup that must run before package initialization

;;; Code:

;; Increase garbage collection threshold during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore garbage collection settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Enable debugging on error for troubleshooting
(setq debug-on-error t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-splash-screen t
      initial-scratch-message nil)

;; Disable tool-bar, menu-bar, and scroll-bar early
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Prevent frame resize when setting font/theme
(setq frame-inhibit-implied-resize t)

;; Disable file-name-handler during startup for faster loading
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; Native compilation settings
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

;;; early-init.el ends here
