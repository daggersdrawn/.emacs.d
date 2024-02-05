;; init.el --- Where all the magic begins
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; The Emacs Lisp Style Guide
;; https://github.com/bbatsov/emacs-lisp-style-guide

;; Disable startup message and customize scratch message.
(setq inhibit-startup-message t
      initial-scratch-message ";; Happy Hacking\n")

;; GNU/Linux or macOS?
(defconst IS-GNULINUX (eq system-type 'gnu/linux))
(defconst IS-MACOS (eq system-type 'darwin))

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Elpaca: An Elisp Package Manager
;;   https://github.com/progfolio/elpaca
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; Ensure environment variables inside Emacs look the same as in the user's shell.
;;   https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Benchmark Emacs Startup time without ever leaving your Emacs.
;;   https://github.com/jschaf/esup
(use-package esup)

;; Keep ~/.emacs.d/ clean from auto-generated configuration and persistent data.
;;   https://github.com/emacscollective/no-littering
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;; This package implements hiding or abbreviation of the mode line
;; displays (lighters) of minor-modes.
;;   https://github.com/emacsmirror/diminish
(use-package diminish)

;; Zenburn theme
;;   https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

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
(add-hook 'html-ts-mode-hook
      #'(lambda ()
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
  :elpaca nil
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
(setq prettify-symbols-alist '(
    ("lambda" . ?λ)
    ("->" . ?→)
    ("=>" . ?⇒)
    ("map" . ?↦)))

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
  :init (global-company-mode +1)
  :config
  (add-hook 'company-mode-hook
            #'(lambda ()
               (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
               (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))))

(use-package company-prescient
  :config
  (company-prescient-mode +1))

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
                               (sequence "|"  "WAITING(w)" "CANCELED(c)"))
           org-agenda-files '("~/.org/agenda.org")))

;; Reformat tool for JSON
;;   https://github.com/gongo/json-reformat#configuration
(use-package json-reformat
  :config (setq json-reformat:indent-width 2))

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

;; Dashboard: An extensible emacs startup screen showing you what’s most important.
;;   https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :elpaca t
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

(use-package major-mode-hydra
  :bind ("M-SPC" . major-mode-hydra))

(use-package python-pytest
  :bind (("C-c C-x t" . python-pytest-dispatch)))

(use-package ansible
  :config
  (use-package ansible-doc
    :init (add-hook 'yaml-mode-hook 'ansible-doc-mode))
  (use-package ansible-vault
    :init (add-hook 'yaml-mode-hook 'ansible-vault-mode-maybe)))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(server-start)
