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

;; GNU/Linux or MacOS?
(defconst IS-GNULINUX (eq system-type 'gnu/linux))
(defconst IS-MACOS (eq system-type 'darwin))

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Seed the random-number generator.
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Random-Numbers.html
(random t)

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
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))
;; Block until current queue processed.
(elpaca-wait)

;; Diminish: Implement hiding or abbreviation of the mode line displays of minor-modes.
;;   https://github.com/emacsmirror/diminish
(use-package diminish
  :config (diminish 'eldoc-mode))

;; Zenburn-theme: The Zenburn color theme.
;;   https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :config (load-theme 'zenburn t))

;; Globally set the default font.
;;   https://www.emacswiki.org/emacs/SetFonts
(set-face-attribute 'default nil :font "Inconsolata Nerd Font Mono-16" :weight 'bold)

;; Default-text-scale: Easily adjust the font size in all Emacs frames.
;;   https://github.com/purcell/default-text-scale
(use-package default-text-scale
  :bind
  ("C-M-=" . default-text-scale-increase)
  ("C-M--" . default-text-scale-decrease))

;; Esup: Benchmark Emacs Startup time without ever leaving your Emacs.
;;   https://github.com/jschaf/esup
(use-package esup)

;; Set cursor color.
;;   https://www.gnu.org/software/emacs/manual/html_node/eintr/X11-Colors.html
(set-cursor-color "#f0dfaf")

;; If the cursor gets too close to the pointer, displace the pointer by a random distance and direction.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Avoidance.html
(mouse-avoidance-mode 'jump)

;; Keep cursor in same relative row and column during PgUP/DN.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Scrolling.html
(setq scroll-preserve-screen-position t)

;; No-littering: Keep ~/.emacs.d/ clean from auto-generated configuration and persistent data.
;;   https://github.com/emacscollective/no-littering
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (setq make-backup-files nil))

;; When emacs is running in a window system and not in a character based-terminal.
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Systems.html
(when window-system
  ;; Frame Title: https://www.emacswiki.org/emacs/FrameTitle
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  ;; Tooltips: https://www.gnu.org/software/emacs/manual/html_node/emacs/Tooltips.htmlm
  (tooltip-mode -1)
  ;; Mouse Commands: https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Commands.html
  (mouse-wheel-mode t)
  ;; Non Blinking Cursor: https://www.emacswiki.org/emacs/NonBlinkingCursor
  (blink-cursor-mode -1))

(when IS-MACOS
  ;; Do not make new frames when opening a new file with Emacs.
  (setq ns-pop-up-frames nil)
  ; FullScreen: https://www.emacswiki.org/emacs/FullScreen
  (custom-set-variables
   '(initial-frame-alist (quote ((fullscreen . maximized))))))

;; Exec-path-from-shell: Ensure environment variables inside Emacs look the same as in the user's shell.
;;   https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Turn off alarms completely.
;;   https://www.emacswiki.org/emacs/AlarmBell
(setq ring-bell-function 'ignore)

;; Column and line numbers
;;   https://www.gnu.org/software/emacs/manual/html_node/efaq/Displaying-the-current-line-or-column.html
(setq-default column-number-mode t)
(when (version<= "26.0.50" emacs-version)
  ;; https://www.emacswiki.org/emacs/LineNumbers
  (global-display-line-numbers-mode))

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
(setq-default show-trailing-whitespace t)

;; Delete whitespace on save.
;;   https://www.emacswiki.org/emacs/DeletingWhitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Whitespace: Configure whitespace mode.
;;   https://www.emacswiki.org/emacs/WhiteSpace
(use-package whitespace
  :elpaca nil
  :bind ("\C-c w" . whitespace-mode)
  :config
  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-line-column 80)
  (setq whitespace-style '(empty tabs lines-tail trailing))
  (setq whitespace-style '(face trailing empty indentation space-after-tab space-before-tab))
  (whitespace-mode))

;; Highlight current line
;;   https://www.emacswiki.org/emacs/HighlightCurrentLine
(global-hl-line-mode)

;; Highlight matching pairs of parentheses and other characters when the point is on them.
;;   https://www.emacswiki.org/emacs/ShowParenMode
(show-paren-mode)

;; Rainbow-delimiters: A 'rainbow parentheses'-like mode which highlights
;; delimiters such as parentheses, brackets or braces according to their depth.
;;   https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :diminish
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Rainbow-mode: Colorize color names in buffers.
;;   https://elpa.gnu.org/packages/rainbow-mode.html
(use-package rainbow-mode
  :diminish
  :init (add-hook 'prog-mode-hook 'rainbow-mode))

;; Paredit: Parenthetical editing in Emacs.
;;   https://paredit.org/
(use-package paredit
  :diminish
  :init (autoload 'enable-paredit-mode "paredit" t)
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'yuck-mode-hook #'enable-paredit-mode))

;; Emacs’s built-in ispell package handles spell-checking and correction.
;; GNU Aspell is a Free and Open Source spell checker designed to eventually replace Ispell.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html
(setq-default ispell-dictionary "en_US")
(setq-default ispell-program-name (if IS-GNULINUX "/usr/bin/aspell" (if IS-MACOS "/usr/local/bin/aspell")))
;; Flyspell provides on-the-fly checking and highlighting of misspellings.
(eval-after-load "ispell"
  '(when (executable-find ispell-program-name)
     (add-hook 'text-mode-hook 'turn-on-flyspell)))

;; Synosaurus: An extensible thesaurus mode.
;;   https://github.com/hpdeifel/synosaurus
;; Must install wordnet for a local lexical database:
;;   https://wordnet.princeton.edu
(use-package synosaurus)

;; Automatically rescan the Imenu facility.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html
(setq-default imenu-auto-rescan t)

;; Always use `y-or-n-p', never `yes-or-no-p'.
;;   https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'yes-or-no-p 'y-or-n-p)

;; Prompt before quit.
(defun ask-before-closing ()
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to quit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))
(keymap-global-set "C-x C-c" 'ask-before-closing)
(keymap-global-set "C-z" 'ask-before-closing)

;; Kill all buffers, leaving *scratch* only.
(defun nuke ()
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows)
  (recentf-nuke))

;; Remove all files from `recentf-list'.
(defun nuke-recent-f ()
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

;; Enable dead keys.
;;   https://www.emacswiki.org/emacs/DeadKeys
(require 'iso-transl)

;; Delete selection on a key press.
;;   https://www.emacswiki.org/emacs/DeleteSelectionMode
(delete-selection-mode)

;; Make identical buffer names unique.
(setq uniquify-buffer-name-style 'reverse
   uniquify-separator "|"
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*")

;; Follow symlinks and do not ask.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/General-VC-Options.html
(setq vc-follow-symlinks t)

;; Automatically reverts the current buffer when its visited file changes on disk.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Reverting.html
(global-auto-revert-mode)

;; Improve the standard text representation of various identifiers/symbols.
(global-prettify-symbols-mode)
(setq prettify-symbols-alist '(
    ("lambda" . ?λ)
    ("->" . ?→)
    ("=>" . ?⇒)
    ("map" . ?↦)))

;; Avy: Jump to things in Emacs tree-style.
;;   https://github.com/abo-abo/avy
(use-package avy
  :bind (("M-g e" . avy-goto-word-0)))

;; Multiple-cursors: Multiple cursors for emacs.
;;   https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind
  ("C-c C-SPC" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this))

;; Which-key: Display available keybindings in a popup.
;;   https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish
  :init (setq which-key-separator " ")
  :config
  (which-key-add-key-based-replacements
    "C-c &" "Yasnippet"
    "C-c e" "Eglot"
    "C-c a" "Avy"
    "C-c a m" "Move"
    "C-c a c" "Copy"
    "C-c a k" "Kill"
    "C-c C-SPC" "multiple cursors")
  (which-key-mode))

;; Itail: An interactive tail mode that allows you to filter the tail with unix pipes and
;; highlight the contents of the tailed file. Works locally or on remote files using tramp.
;;   https://github.com/re5et/itail
(use-package itail)

;; Org-mode: Your life in plain text.
;;   https://orgmode.org/
(use-package org
  :config (setq
           org-src-fontify-natively t
           org-src-tab-acts-natively t
           org-todo-keywords '((sequence "BACKLOG(b)" "TODO(t)" "ACTIVE(a)" "|" "DONE(d)")
                               (sequence "|"  "WAITING(w)" "CANCELED(c)"))
           org-agenda-files '("~/.org/agenda.org")))

;; Orglink: Use org-mode links in other modes.
;;   https://github.com/tarsius/orglink
(use-package orglink
  :diminish
  :config (global-orglink-mode))

;; Yankpad: Insert Emacs text snippets from org-mode.
;;   http://github.com/Kungsgeten/yankpad
(use-package yankpad
  :bind ("M-+" . yankpad-insert)
  :init
  (unless (package-installed-p 'yankpad)
    (package-vc-install "https://github.com/Kungsgeten/yankpad")))

;; YASnippet: A template system for Emacs.
;;   https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :diminish
  :bind ("M-+" . yas-insert-snippet)
  :config
  (yas-global-mode)
  (setopt yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/.emacs.d/snippets")))
  (yas-global-mode)
  (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))

;; JSON-reformat: Reformat tool for JSON.
;;   https://github.com/gongo/json-reformat
(use-package json-reformat
  :config (setq json-reformat:indent-width 2))

;; Magit: A git porcelain inside emacs.
;;   https://magit.vc
(use-package magit
  :commands (magit-status)
  :bind ("C-x g" . magit-status))

;; Forge: Allows Magit to work with Git forges, such as Github and Gitlab.
;;   https://magit.vc/manual/forge/
(use-package forge
  :after magit)

;; GitTimemachine: Step through historic versions of git controlled files.
;;   https://codeberg.org/pidu/git-timemachine
(use-package git-timemachine)

;; Git gutter: Show information in the gutter about files in a git repository.
;;   https://github.com/syohex/emacs-git-gutter
(if (display-graphic-p)
  (use-package git-gutter
    :diminish
    :init (setq global-linum-mode nil)(global-git-gutter-mode)))

;; Python-pytest: Integrate the python pytest test runner.
;;   https://github.com/wbolster/emacs-python-pytest
(use-package python-pytest
  :bind (("C-c C-x t" . python-pytest-dispatch)))

;; Ansible: An Ansible minor mode.
;;   https://github.com/k1LoW/emacs-ansible
(use-package ansible)

;; Ansible-doc: Documentation lookup.
;;   https://github.com/lunaryorn/ansible-doc.el
(use-package ansible-doc
  :init (add-hook 'yaml-mode-hook 'ansible-doc-mode))

;; Ansible-vault: Minor mode for in place manipulation of ansible-vault.
;;   http://github.com/zellio/ansible-vault-mode
(use-package ansible-vault
  :init (add-hook 'yaml-mode-hook 'ansible-vault-mode-maybe))

;; Haskell-mode: A Haskell editing mode
;;   https://github.com/haskell/haskell-mode
(use-package haskell-mode)

;; Treesit-auto: Automatically install and use tree-sitter major modes.
;;   https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :custom (treesit-auto-install t)
  :config (treesit-auto-add-to-auto-mode-alist 'all)(global-treesit-auto-mode))

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Consult: Provides search and navigation based on completing-read.
;;   https://github.com/minad/consult
(use-package consult
  :after vertico
  :bind (("C-x b"       . consult-buffer)
         ("C-x C-k C-k" . consult-kmacro)
         ("M-y"         . consult-yank-pop)
         ("M-g g"       . consult-goto-line)
         ("M-g M-g"     . consult-goto-line)
         ("M-g f"       . consult-flymake)
         ("M-g i"       . consult-imenu)
         ("M-s l"       . consult-line)
         ("M-s L"       . consult-line-multi)
         ("M-s u"       . consult-focus-lines)
         ("M-s g"       . consult-ripgrep)
         ("M-s M-g"     . consult-ripgrep)
         ("C-x C-SPC"   . consult-global-mark)
         ("C-x M-:"     . consult-complex-command)
         ("C-c n"       . consult-org-agenda)
         ("C-c m"       . my/notegrep)
         :map minibuffer-local-map
         ("M-r" . consult-history))
  :custom (completion-in-region-function #'consult-completion-in-region)
  :config
  (defun my/notegrep ()
    ;; Use interactive grepping to search my notes.
    (interactive)
    (consult-ripgrep org-directory))
  (recentf-mode))

;; Consult-dir: Insert paths into the minibuffer prompt.
;;   https://github.com/karthink/consult-dir
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; Consult-recoll: Index and search PDFs, org and other text files and emails.
;;   https://codeberg.org/jao/consult-recoll
(use-package consult-recoll
  :bind (("M-s r" . consult-recoll))
  :init (setq consult-recoll-inline-snippets t)
  :config
  ;; Keep searches up to daate by starting the indexing deamon.
  (defun recoll-index (&optional args)
    (interactive)
    (let ((recollindex-buffer "*RECOLLINDEX*"))
      (unless (process-live-p (get-buffer-process (get-buffer recollindex-buffer)))
        (make-process :name "recollindex"
                      :buffer recollindex-buffer
                      :command '("recollindex" "-m" "-D")))))
  (eval-after-load 'consult-recoll
    (recoll-index)))

;; Vertico: VERTical Interactive COmpletion.
;;   https://github.com/minad/vertico
(use-package vertico
  :init
  ;; Enable vertico using the vertico-flat-mode
  (require 'vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  (vertico-mode))

;; Orderless: Completion style for matching regexps in any order.
;;   https://github.com/oantolin/orderless
(use-package orderless
  :commands (orderless)
  :custom (completion-styles '(orderless flex)))

;; Marginalia: Add (marks or annotations) marginalia to the minibuffer completions.
;;   https://github.com/minad/marginalia
(use-package marginalia
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config (marginalia-mode))

;; Embark: Emacs Mini-Buffer Actions Rooted in Keymaps.
;;   https://github.com/oantolin/embark
(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . custom-embark-dwim)
   ([remap describe-bindings] . embark-bindings)
   :map embark-file-map
   ("C-=" . dragon-drop)
   :map embark-defun-map)
  :custom
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator
     embark-minimal-indicator))
  :init
  ;; Optionally replace the key help with a completing-read interface.
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Selecting commands via completions instead of key bindings.
  (setq embark-prompter 'embark-completing-read-prompter)
  :config
  ;; Simple drag-and-drop source/sink for X or Wayland.
  (defun dragon-drop (file)
    (start-process-shell-command "dragon-drop" nil
                                 (concat "dragon-drop " file)))
  ;; Previews candidate in vertico buffer, unless it's a consult command.
  (defun custom-embark-dwim ()
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim))))))

;; Embark-consult: Consult integration for Embark.
;;   https://github.com/oantolin/embark/blob/master/embark-consult.el
(use-package embark-consult
  :after (:all embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Corfu: COmpletion in Region FUnction.
;;  https://github.com/minad/corfu
(use-package corfu
  :custom
  (corfu-auto t)                        ;; Enable auto completion.
  (corfu-auto-delay 0.8)                ;; Delay for auto completion.
  (corfu-auto-prefix 2)                 ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-delay '(0.5 . 0.2))  ;; Automatically update info popup.
  (corfu-cycle t)                       ;; Enable cycling for candidates.
  (corfu-preview-current 'insert)       ;; Insert previewed candidate.
  (corfu-preselect 'prompt)             ;; Preselect the prompt.
  (corfu-on-exact-match nil)            ;; Configure handling of exact matches.
  :bind
  (:map corfu-map
        ("M-SPC"      . corfu-insert-separator)
        ("TAB"        . corfu-next)
        ([tab]        . corfu-next)
        ("S-TAB"      . corfu-previous)
        ([backtab]    . corfu-previous)
        ("S-<return>" . corfu-insert)
        ("RET"        . nil))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto nil)
              (corfu-mode))))

;; Corfu-prescient: Simple but effective sorting and filtering for Emacs.
;;   https://github.com/raxod502/prescient.el
(use-package corfu-prescient
  :init (corfu-prescient-mode)(prescient-persist-mode))

;; Cape: Completion at point extensions.
;;  https://github.com/minad/cape
(use-package cape
  :defer 10
  :bind
  ("M-/" . cape-dabbrev)
  ("C-c f" . cape-file))

;; Org-ai: Use ChatGPT and other LLMs in org-mode and beyond.
;;   https://github.com/rksm/org-ai
(use-package org-ai
  ;; Default keybinding of C-c M-a
  :load-path (lambda () "~/.emacs.d/elpaca/repos/org-ai/")
  :commands (org-ai-mode org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  (org-ai-global-mode)
  :config
  (setq org-ai-default-chat-model "gpt-4")
  (org-ai-install-yasnippets))

;; Speech-to-Text interface using OpenAI’s whisper speech recognition model.
;;   https://github.com/natrys/whisper.el
(use-package whisper
  :elpaca (whisper :host github :repo "natrys/whisper.el")
  :bind ("C-s-r" . whisper-run)
  :config
  (setq whisper-install-directory "~/.cache/"
        whisper-model "base"
        whisper-language "en"
        whisper-translate nil
        whisper-use-threads (/ (num-processors) 2)))

;; Custom keybindings
(keymap-global-set "C-x 4" 'window-swap-states)

;; Use up Emacs as an edit server.
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
(server-start)
