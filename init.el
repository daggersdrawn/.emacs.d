;; init.el --- Where all the magic begins
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; The Emacs Lisp Style Guide
;; https://github.com/bbatsov/emacs-lisp-style-guide

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; A simple package manager for Emacs, and a repository of pre-packed Emacs Lisp code.
;;   https://www.emacswiki.org/emacs/ELPA
;;
;; MELPA (Milkypostman’s Emacs Lisp Package Archive)
;; The largest and most up-to-date repository of Emacs packages.
;;   https://github.com/melpa/melpa
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
				 ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Isolate package configuration in a performance-oriented and tidy way.
;;   https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Ensure environment variables inside Emacs look the same as in the user's shell.
;;   https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Keep ~/.emacs.d/ clean from auto-generated configuration and persistent data.
;;   https://github.com/emacscollective/no-littering
(use-package no-littering
  :ensure t)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

;; GNU/Linux or macOS?
(defconst IS-GNULINUX (eq system-type 'gnu/linux))
(defconst IS-MACOS (eq system-type 'darwin))

;; Zenburn theme
;;   https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)

;; When emacs is running in a window system and not in a character based-terminal.
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Systems.html
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))  ;; https://www.emacswiki.org/emacs/FrameTitle
  (tooltip-mode -1)  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Tooltips.htmlm
  (mouse-wheel-mode t)  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Commands.html
  (blink-cursor-mode -1))  ;; https://www.emacswiki.org/emacs/NonBlinkingCursor

;; Column and line numbers
;;   https://www.gnu.org/software/emacs/manual/html_node/efaq/Displaying-the-current-line-or-column.html
(setq-default column-number-mode t)
(when (version <= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))  ;; https://www.emacswiki.org/emacs/LineNumbers

;; Use only spaces and set the tab width.
;;   https://www.emacswiki.org/emacs/IndentationBasics
(setq-default indent-tabs-mode nil
              tab-width 4)
