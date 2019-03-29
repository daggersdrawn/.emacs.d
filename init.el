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

;; A simple package manager for Emacs, and a repository of pre-packed Emacs Lisp code.
;;   https://www.emacswiki.org/emacs/ELPA
;;
;; MELPA (Milkypostman’s Emacs Lisp Package Archive)
;; The largest and most up-to-date repository of Emacs packages.
;;   https://github.com/melpa/melpa
;;
;; Disable automatic package loading at startup.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html
(require 'package)
(add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	       '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	       '("org" . "https://orgmode.org/elpa/") t)
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

;; Benchmark Emacs Startup time without ever leaving your Emacs.
;;   https://github.com/jschaf/esup
(use-package esup
  :ensure t)

;; Paradox: A modern Emacs package menu.
;;   https://github.com/Malabarba/paradox
(use-package paradox
  :ensure t
  :config
  (setq paradox-github-token (exec-path-from-shell-copy-env "PARADOX_GITHUB_TOKEN")))

;; Keep ~/.emacs.d/ clean from auto-generated configuration and persistent data.
;;   https://github.com/emacscollective/no-littering
(use-package no-littering
  :ensure t)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

;; Zenburn theme
;;   https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)

;; Globally set the default font.
;;   https://www.emacswiki.org/emacs/SetFonts
(set-face-attribute 'default nil
                    :family "InconsolataGo Nerd Font" :height 140)

;; Easily adjust the font size in all Emacs frames.
;;   https://github.com/purcell/default-text-scale
(use-package default-text-scale
  :ensure t
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

;; Require final newlines.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Customize-Save.html
(setq-default require-final-newline t)

;; Show trailing whitespace and delete on save.
(setq-default show-trailing-whitespace t)  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Useless-Whitespace.html
(add-hook 'before-save-hook 'delete-trailing-whitespace)  ;; https://www.emacswiki.org/emacs/DeletingWhitespace

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
(use-package synosaurus
  :ensure t)

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

;; Which Key displays available keybindings in a popup.
;;   https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config (which-key-mode))

;; An interactive tail mode that allows you to filter the tail with unix pipes and highlight
;; the contents of the tailed file. Works locally or on remote files using tramp.
;;   https://github.com/re5et/itail
(use-package itail
  :ensure t)

;; Org mode, your life in plain text.
;;   https://orgmode.org/
(use-package org
  :pin org
  :ensure org-plus-contrib
  :config (setq
           org-src-fontify-natively t
           org-src-tab-acts-natively t
           org-todo-keywords '((sequence "BACKLOG(b)" "TODO(t)" "DOING(n)" "|" "DONE(d)")
                               (sequence "|"  "ONHOLD(h)" "CANCELED(c)"))
           ;; org-agenda-files '("~/.org/agenda.org")
           ))

;; Company is a modular in-buffer text completion framework for Emacs.
;;   https://company-mode.github.io/
(use-package company
  :ensure t
  :config
  (progn
    (setq company-idle-delay 0.2
	      company-tooltip-limit 20
	      company-minimum-prefix-length 2
	      company-echo-delay 0
	      company-dabbrev-downcase nil)
    (add-hook 'after-init-hook 'global-company-mode)

    (eval-after-load 'company
	  `(let (( Map  company-active-map))
	     (define-key company-active-map (kbd "\C-n") 'company-select-next)
	     (define-key company-active-map (kbd "\C-n") 'company-select-next)
	     (define-key company-active-map (kbd "\C-p") 'company-select-previous)
	     (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
	     (define-key company-active-map (kbd "<tab>") 'company-complete)))))

;; Markdown Mode is a major mode for editing Markdown-formatted text.
;;   https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'" . markdown-mode)
	     ("\\.md\\'"       . markdown-mode)))

;; JSON Mode is a major mode for editing JSON files.
;;   https://github.com/joshwnj/json-mode
(use-package json-mode
  :ensure t)

;; Reformat tool for JSON
;;   https://github.com/gongo/json-reformat#configuration
(use-package json-reformat
  :ensure t
  :config
  (setq json-reformat:indent-width 2))

;; Magit: a git porcelain inside emacs.
;;   https://magit.vc
(use-package magit
  :ensure t
  :commands (magit-status)
  :bind ("C-x g" . magit-status))

;; GitTimemachine: step through historic versions of git controlled files.
;;   https://gitlab.com/pidu/git-timemachine
(use-package git-timemachine
  :ensure t)

;; Git gutter
;;   https://github.com/syohex/emacs-git-gutter
;;
;; Fringe version of git-gutter.el
;;   https://github.com/syohex/emacs-git-gutter-fringe
;;
;; git-gutter.el does not work with linum-mode but
;; git-gutter-fringe.el can work with linum-mode.
;;
;; In contrast, git-gutter-fringe.el does not work in tty frame(emacs
;; -nw), but git-gutter.el can work in tty frame.
(if (display-graphic-p)
    (use-package git-gutter-fringe
	  :ensure t
	  :init (global-git-gutter-mode))
  (use-package git-gutter
    :ensure t
    :init (global-git-gutter-mode)))

;; Web Mode is an autonomous emacs major-mode for editing web templates.
;;   https://web-mode.org
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mdx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

;; REST client tool for exploring and testing HTTP REST webservices.
;;   https://github.com/pashky/restclient.el
(use-package restclient
  :ensure t
  :defer 5
  :config (add-hook 'restclient-mode-hook 'company-restclient))

;; Company-mode completion back-end for restclient-mode.
;;   https://github.com/iquiw/company-restclient
(use-package company-restclient
  :ensure t
  :config
  (progn
    (add-hook 'restclient-mode-hook
		      (lambda ()
		        (set (make-local-variable 'company-backends)'(company-restclient))
		        (company-mode t)))))

;; An org-mode extension to restclient.el
;;   https://github.com/alf/ob-restclient.el
(use-package ob-restclient
  :ensure t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t))))
