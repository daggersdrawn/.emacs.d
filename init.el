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
