;; init.el --- Where all the magic begins
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; MELPA (Milkypostman’s Emacs Lisp Package Archive)
;; The largest and most up-to-date repository of Emacs packages.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Isolate package configuration in a performance-oriented and tidy way
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Ensure environment variables in Emacs look the same as in the user's shell
(use-package exec-path-from-shell
             :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Keep ~/.emacs.d/ clean from auto-generated configuration and persistent data
(use-package no-littering
  :ensure t)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
