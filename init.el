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

;; GNU/Linux or macOS?
(defconst IS-GNULINUX (eq system-type 'gnu/linux))
(defconst IS-MACOS (eq system-type 'darwin))

;; Disable startup message and customize scratch message.
(setq inhibit-startup-message t
      initial-scratch-message ";; Happy Hacking\n")

;; straight.el: next-generation, purely functional package manager for the Emacs hacker.
;;   https://github.com/raxod502/straight.el
(defvar bootstrap-version)
(setq straight-fix-flycheck t)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Effectively replace use-package with straight-use-package
;;   https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Include melpa in package-list-packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Ensure environment variables inside Emacs look the same as in the user's shell.
;;   https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Benchmark Emacs Startup time without ever leaving your Emacs.
;;   https://github.com/jschaf/esup
(use-package esup)

;; Keep ~/.emacs.d/ clean from auto-generated configuration and persistent data.
;;   https://github.com/emacscollective/no-littering
(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

;; This package implements hiding or abbreviation of the mode line
;; displays (lighters) of minor-modes.
;;   https://github.com/emacsmirror/diminish
(use-package diminish)

;; Zenburn theme
;;   https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme)
(load-theme 'zenburn t)

;; Globally set the default font.
;;   https://www.emacswiki.org/emacs/SetFonts
(set-face-attribute 'default nil :font "InconsolataGo Nerd Font-16")

;; Easily adjust the font size in all Emacs frames.
;;   https://github.com/purcell/default-text-scale
(use-package default-text-scale
  :config
  (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
  (global-set-key (kbd "C-M--") 'default-text-scale-decrease))

;; Seed the random-number generator.
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Random-Numbers.html
(random t)

(when IS-MACOS
  ;; Set up Emacs as an edit server, so that it "listens" for external edit requests and acts accordingly.
  ;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
  (server-start)

  ;; Do not make new frames when opening a new file with Emacs.
  (setq ns-pop-up-frames nil)

  ;; Configuring fullscreen mode.
  ;;   https://www.emacswiki.org/emacs/FullScreen
  (custom-set-variables
   '(initial-frame-alist (quote ((fullscreen . maximized))))))

;; When emacs is running in a window system and not in a character based-terminal.
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Systems.html
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))  ;; https://www.emacswiki.org/emacs/FrameTitle
  (tooltip-mode -1)  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Tooltips.htmlm
  (mouse-wheel-mode t)  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Commands.html
  (blink-cursor-mode -1))  ;; https://www.emacswiki.org/emacs/NonBlinkingCursor

;; Turn off alarms completely.
;;   https://www.emacswiki.org/emacs/AlarmBell
(setq ring-bell-function 'ignore)

;; Column and line numbers
;;   https://www.gnu.org/software/emacs/manual/html_node/efaq/Displaying-the-current-line-or-column.html
(setq-default column-number-mode t)
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))  ;; https://www.emacswiki.org/emacs/LineNumbers

;; Use only spaces and set the tab width.
;;   https://www.emacswiki.org/emacs/IndentationBasics
(setq-default indent-tabs-mode nil
              tab-width 4)
(add-hook 'html-mode-hook
      '(lambda()
        (setq c-basic-offset 4)
        (setq indent-tabs-mode nil)))

;; Require final newlines.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Customize-Save.html
(setq-default require-final-newline t)

;; Show trailing whitespace and delete on save.
(setq-default show-trailing-whitespace t)  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Useless-Whitespace.html
(add-hook 'before-save-hook 'delete-trailing-whitespace)  ;; https://www.emacswiki.org/emacs/DeletingWhitespace

;; Configure whitespace mode
(use-package whitespace
  :bind ("\C-c w" . whitespace-mode)
  :config

  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-line-column 80)
  (setq whitespace-style '(empty tabs lines-tail trailing))
  (setq whitespace-style '(face trailing empty indentation space-after-tab space-before-tab))
  (whitespace-mode t))

;; Highlight current line
;;   https://www.emacswiki.org/emacs/HighlightCurrentLine
(global-hl-line-mode 1)

;; Highlight matching pairs of parentheses and other characters when the point is on them.
;;   https://www.emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)

;; Emacs’s built-in ispell package handles spell-checking and correction.
;; GNU Aspell is a Free and Open Source spell checker designed to eventually replace Ispell.
(setq-default ispell-dictionary "en_US")
(setq-default ispell-program-name (if IS-GNULINUX "/usr/bin/aspell" (if IS-MACOS "/usr/local/bin/aspell")))
;; Flyspell provides on-the-fly checking and highlighting of misspellings.
(eval-after-load "ispell"
  '(when (executable-find ispell-program-name)
     (add-hook 'text-mode-hook 'turn-on-flyspell)))

;; An extensible thesaurus mode for emacs.
;;   https://github.com/hpdeifel/synosaurus
;; Install wordnet for a local lexical database:
;;    https://wordnet.princeton.edu
(use-package synosaurus)

;; Set cursor color.
;;   https://www.gnu.org/software/emacs/manual/html_node/eintr/X11-Colors.html
(set-cursor-color "#f0dfaf")

;; If the cursor gets too close to the pointer, displace the pointer by a random distance and direction.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Avoidance.html
(mouse-avoidance-mode 'jump)

;; Keep cursor in same relative row and column during PgUP/DN.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Scrolling.html
(setq scroll-preserve-screen-position t)

;; Automatically rescan the Imenu facility.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html
(setq-default imenu-auto-rescan t)

;; Some users want to always use `y-or-n-p', never `yes-or-no-p'.
;;   https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable dead keys.
;;   https://www.emacswiki.org/emacs/DeadKeys
(require 'iso-transl)

;; Delete selection on a key press.
;;   https://www.emacswiki.org/emacs/DeleteSelectionMode
(delete-selection-mode t)

 ;; Make identical buffer names unique.
 (setq uniquify-buffer-name-style 'reverse
	uniquify-separator "|"
	uniquify-after-kill-buffer-p t
	uniquify-ignore-buffers-re "^\\*")

;; Improve the standard text representation of various identifiers/symbols.
(global-prettify-symbols-mode 1)
(setq prettify-symbols-alist
      '(
        ("lambda" . ?λ)
        ("->" . ?→)
        ("=>" . ?⇒)
        ("map" . ?↦)
        ))

(defun ask-before-closing ()
  "Prompt before quit."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to quit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))
(global-set-key (kbd "C-x C-c") 'ask-before-closing)
(global-set-key (kbd "C-z") 'ask-before-closing)

(defun nuke ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows)
  (recentf-nuke))

(defun recentf-nuke ()
  "Remove all files from `recentf-list'."
  (interactive)
  (let ((count (length recentf-list)))
    (setq recentf-list
          (delq nil
                (mapcar (function
                         (lambda (filename)))
                        recentf-list)))
    (setq count (- count (length recentf-list)))
    (message "%s removed from the list"
             (cond ((= count 0) "No file")
                   ((= count 1) "One file")
                   (t (format "%d files" count)))))
  (setq recentf-update-menu-p t))

;; Follow symlinks and do not ask.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/General-VC-Options.html
(setq vc-follow-symlinks t)

;; Automatically reverts the current buffer when its visited file changes on disk.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Reverting.html
(global-auto-revert-mode 1)

;; Jump to things in Emacs tree-style.
;;   https://github.com/abo-abo/avy
(use-package avy
  :bind (("M-g e" . avy-goto-word-0)))

;; Multiple cursors for emacs.
;;   https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors)

;; A template system for Emacs.
;;   https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/.emacs.d/snippets"))))

;; Which Key displays available keybindings in a popup.
;;   https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-add-key-based-replacements
    "C-c &" "Yasnippet"
    "C-c e" "Eglot"
    "C-c a" "Avy"
    "C-c a m" "Move"
    "C-c a c" "Copy"
    "C-c a k" "Kill"
    "C-c m" "multiple cursors")
  (which-key-mode 1))

;; A better solution for incremental narrowing in Emacs.
;;   https://github.com/raxod502/selectrum
(use-package selectrum
  :init (selectrum-mode +1))

;; Simple but effective sorting and filtering for Emacs.
;;   https://github.com/raxod502/prescient.el
(use-package selectrum-prescient
  :init (selectrum-prescient-mode +1)
        (prescient-persist-mode +1))

;; Company
(use-package company
  :diminish company-mode
  :init (global-company-mode +1))

(add-hook 'company-mode-hook
          '(lambda ()
             (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
             (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)))

(use-package company-prescient)
(company-prescient-mode +1)

;; An interactive tail mode that allows you to filter the tail with unix pipes and highlight
;; the contents of the tailed file. Works locally or on remote files using tramp.
;;   https://github.com/re5et/itail
(use-package itail)

;; Org mode, your life in plain text.
;;   https://orgmode.org/
(use-package org
  :config (setq
           org-src-fontify-natively t
           org-src-tab-acts-natively t
           org-todo-keywords '((sequence "BACKLOG(b)" "TODO(t)" "DOING(n)" "|" "DONE(d)")
                               (sequence "|"  "ONHOLD(h)" "CANCELED(c)"))
           ;; org-agenda-files '("~/.org/agenda.org")
           ))

;; Markdown Mode is a major mode for editing Markdown-formatted text.
;;   https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown"))

;; JSON Mode is a major mode for editing JSON files.
;;   https://github.com/joshwnj/json-mode
(use-package json-mode)

;; Reformat tool for JSON
;;   https://github.com/gongo/json-reformat#configuration
(use-package json-reformat
  :config (setq json-reformat:indent-width 2))

;; Web templating mode for emacs.
;;   https://github.com/fxbois/web-mode
(use-package web-mode
  :mode (("\\.jsx?\\'" . web-mode)
         ("\\.tsx?\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :hook
  (web-mode .
            (lambda ()
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx")
                (message "now set to: %s" web-mode-content-type))))
  :config
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(use-package typescript-mode)

(use-package yaml-mode)

;; Magit: a git porcelain inside emacs.
;;   https://magit.vc
(use-package magit
  :commands (magit-status)
  :bind ("C-x g" . magit-status))

;; Forge allows you to work with Git forges, such as Github and Gitlab.
;;   https://magit.vc/manual/forge/
(use-package forge
  :after magit)

;; GitTimemachine: step through historic versions of git controlled files.
;;   https://gitlab.com/pidu/git-timemachine
(use-package git-timemachine)

;; Git gutter
;;   https://github.com/syohex/emacs-git-gutter
(if (display-graphic-p)
  (use-package git-gutter
    :diminish git-gutter-mode
    :init
    (setq global-linum-mode nil)
    (global-git-gutter-mode +1)))

(use-package major-mode-hydra
  :demand t
  :bind ("M-SPC" . major-mode-hydra))

(use-package eglot)

(use-package eldoc
  :diminish eldoc-mode
  :config
  (global-eldoc-mode 1))

(use-package flycheck)

(use-package py-isort
  :commands (py-isort-buffer py-isort-region))

(use-package blacken)

(use-package python-pytest
  :bind (("C-c C-x t" . python-pytest-dispatch)))

(use-package ansible
  :config
  (use-package ansible-doc
    :init (add-hook 'yaml-mode-hook 'ansible-doc-mode))
  (use-package ansible-vault
    :init (add-hook 'yaml-mode-hook 'ansible-vault-mode-maybe)))

(use-package python-mode
  :after (eglot)
  :hook (python-mode . eglot-ensure)
  :config
  (setq eldoc-message-function #'eldoc-minibuffer-message)
  :mode-hydra
  ("Nav"
   (("n" python-nav-forward-defun "next-defun" :exit nil)
    ("p" python-nav-backward-defun "prev-defun" :exit nil))
   "Errors"
   (("<" flycheck-previous-error "prev" :exit nil)
    (">" flycheck-next-error "next" :exit nil)
    ("l" flycheck-list-errors "list"))
   "Env"
   (("a" pipenv-activate "pipenv-activate" :exit nil)
    ("d" pipenv-deactivate "pipenv-deactivate" :exit nil)
    ("w" pyvenv-workon "workon...")
    ("s" run-python "pyshell"))
   "Tools"
   (("f" blacken-buffer "reformat")
    ("i" py-isort-buffer "sort imports"))
   "Test"
   (("t" python-pytest-popup "pytest..."))))

(use-package pipenv
  :defer t
  :diminish pipenv-mode
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-keymap-prefix (kbd "C-c C-o")))
