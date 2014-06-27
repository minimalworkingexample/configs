
;;; 
;;; 
;;; Brady Trainor's very clean init.el file
;;; 
;;;  (thanks to outline-minor-mode
;;;                     & outshine.el)
;;; 
;;; 
;;; 
;;; ----------------------------------------
;;; notes:

;; The following packages are used and recommended, but not necessary
;;       bookmark+, dired-details, outshine, todotxt, vimrc-mode, w3m

;;; TODOs
;;;;
;;;; random stuff
;;;;; make bookmakrs part of aliases
;;;;; fix up aliases for fast keys to targets
;;;;;; brainstorm on locations (tech.org)
;;;; hack for ownCloud org use

;; needs to not act on todotxt, make it directory specific

;; org-mode is enabled in .txt files
;; (add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

;;; ----------------------------------------
;;; emacs-aesthetics
;;;; emacs-aesthetics

;; remove menu, toolbar, scrollbar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
;; prevent cursor blink, error beep, answer faster with y/n
(blink-cursor-mode 0)
(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p)
;; prevent cluttered startup screen, message in scratch
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;;;; color theme

(load-theme 'deeper-blue)

;;;; font setting

;; these seem to be the best in windows
(set-face-attribute 'default nil
		    :family "Consolas"
		    :height 100
		    )

;;;; to wrap or not to wrap

;; don't wrap text, except for org and tex files
(setq-default truncate-lines t)
(add-hook 'org-mode-hook 'visual-line-mode t)
(add-hook 'LaTeX-mode-hook 'visual-line-mode t)

;;; locations

(setq home "c:/Users/user-name/"
      ;; in home
      owncloud (concat home "ownCloud/")
      dropbox (concat home "Dropbox/")
      emacsd (concat home "AppData/Roaming/.emacs.d/")
      vimrc (concat home ".vimrc")
      documents (concat home "Documents/")
      local documents
      ;; in emacsd
      init (concat emacsd "init.el")
      bookmarks (concat emacsd "bookmarks")
      ;; in owncloud
      config (concat owncloud "config/")
      main (concat owncloud "main.txt")
      tech (concat owncloud "tech.txt")
      ;; in local
      routines (concat local "routines.org")
      ;; in config
      ;; vimrc (concat config ".vimrc")
      ;; init (concat config "init.el")
      ;; bookmarks (concat config "bookmarks")
      )

;;; packaging setup

;; specify the repositories
(setq package-archives
      '(
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ))

;; package.el knows where to find installed packages
(package-initialize)

;;; config folding
;;;; outline

;; when a .el file is opened, use outline-minor-mode
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

;;;; outshine

;; gives my init.el nice folding and keybinding defaults
(when (locate-library "outshine")
  (autoload 'outshine-hook-function "outshine")
  (add-hook 'emacs-lisp-mode-hook 'outshine-hook-function))

;;;; vimrc mode, hideshow

;; because we sometimes view our .vim and .vimrc files from emacs
(when (locate-library "vimrc-mode")
  (add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))
  (add-to-list 'hs-special-modes-alist '(vimrc-mode "{{{" "}}}" nil nil))
  (add-hook 'vimrc-mode-hook 'hs-minor-mode)
  (autoload 'hs-minor-mode "hideshow" nil t)
  (eval-after-load 'hideshow
    '(define-key hs-minor-mode-map (kbd "TAB") 'hs-toggle-hiding)))

;;; better nav settings
;;;; bookmark+

;; (setq bookmark-default-file "/e/emacs-config/bookmarks.d/main")
;; (setq bmkp-last-as-first-bookmark-file nil)
;; fix for buggy behavior, `L` Does not behave as expected,
;; see approx. line 4000 of bookmark+-1.el
;; (setq bmkp-last-bookmark-file      bookmark-default-file
      ;; bmkp-current-bookmark-file   bookmark-default-file
      ;; )

(setq bookmark-default-file bookmarks)

;; (when (locate-library "bookmark+")  
  ;; (add-hook 'bookmark-bmenu-mode-hook
	    ;; (lambda ()
	      ;; (load "bookmark+"))))

;; '(bmkp-heading ((t nil)))
;; '(bmkp-local-file-without-region ((t nil)))

;; (set-face-background 'bmkp-local-directory "midnight blue")

;;;; dired settings

(setq-default dired-omit-files-p t)
(setq dired-omit-files "^\\.?#\\|^\\.$\\|.csync_journal.db\\|.owncloudsync.log")
(setq dired-details-hidden-string "")
(add-hook 'dired-load-hook 
	  (lambda () 
	    (load "dired-x")
	    (when (locate-library "dired-details")
	      (load "dired-details")
	      (dired-details-install)
	      )
	    ))

;; if dired(-x) is not loaded, C-x C-j is undefined
(defun undefined-c-x-c-j-loads-dired-and-jumps () 
  (interactive) 
  (load "dired") 
  (dired-jump))
(global-set-key (kbd "C-x C-j") 'undefined-c-x-c-j-loads-dired-and-jumps)

;;;; ido

;; make navigation via minibuffer easier to see (vertical list)
(ido-mode 1)
(setq ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
(setq ido-default-buffer-method 'selected-window)

;;;; ibuffer

(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

;;;; window manipulation settings

;; make history of windows configuration traversable
(winner-mode)

;; quick keys for switching windows
(windmove-default-keybindings)
;; fix windmove in org-mode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; quick keys for resizing windows with keyboard
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;; TODO miscellaneous
;;;; TODO custom functions
;;;;; TODO jump to <favorites> (manual bookmarks)

;; because emacs' bookmarks aren't fast enough for me
;; i want to jump directly to my favorite files
;; with very few keystrokes

;; (dolist 
    ;; '((location . key)
      ;; (location . key)
      ;; ...
      ;; )
  ;; (defun jump-to- <location> ()
    ;; (interactive)
    ;; (find-file location))
  ;; (global-set-key (kbd "C-c <key>") 'jump-to- <location>)
  ;; )

(defun jump-to-main-dot-org ()
  (interactive)
  (find-file main))
(global-set-key (kbd "C-c m") 'jump-to-main-dot-org)

(defun jump-to-tech-dot-org ()
  (interactive)
  (find-file tech))
(global-set-key (kbd "C-c t") 'jump-to-tech-dot-org)

(defun jump-to-routines-dot-org ()
  (interactive)
  (find-file routines))
(global-set-key (kbd "C-c r") 'jump-to-routines-dot-org)

(defun jump-to-ownCloud-dir ()
  (interactive)
  (dired owncloud))
(global-set-key (kbd "C-c o") 'jump-to-ownCloud-dir)

(defun jump-to-Documents-dir ()
  (interactive)
  (find-file documents))
(global-set-key (kbd "C-c d") 'jump-to-Documents-dir)

(defun jump-to-init-dot-el ()
  (interactive)
  (find-file init))
(global-set-key (kbd "C-c i") 'jump-to-init-dot-el)

(defun jump-to-dot-vimrc ()
  (interactive)
  (find-file vimrc))
(global-set-key (kbd "C-c v") 'jump-to-dot-vimrc)



;;;;; quicker bookmarks key

(global-set-key (kbd "C-c l") 'bookmark-bmenu-list)

;;;;; highlight a line

(defun find-overlays-specifying (prop pos)
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun highlight-or-dehighlight-line ()
  (interactive)
  (if (find-overlays-specifying
       'line-highlight-overlay-marker
       (line-beginning-position))
      (remove-overlays (line-beginning-position) (+ 1 (line-end-position)))
    (let ((overlay-highlight (make-overlay
                              (line-beginning-position)
                              (+ 1 (line-end-position)))))
      (overlay-put overlay-highlight 'face '(:background "dark green"))
      (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))

(global-set-key [f8] 'highlight-or-dehighlight-line)

(defun remove-all-highlight ()
  (interactive)
  (remove-overlays (point-min) (point-max))
  )

(global-set-key [f9] 'remove-all-highlight)

;;;;; new with scratch

(defun new-with-scratch ()
  (interactive)
  (with-selected-frame (make-frame)
    (switch-to-buffer "*scratch*")
    ))
(global-set-key (kbd "C-c n") 'new-with-scratch)

;;;;; commenting function

;; invaluable, even if only for my init.el file
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end)
              )
      (setq beg (line-beginning-position)
            end (line-end-position)
            )
      )
    (comment-or-uncomment-region beg end)
    (next-logical-line)
    ))
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)

;;;;; fix for org ret

(defun org-N-empty-lines-before-current (N)
  "Make the number of empty lines before current exactly N.
So this will delete or add empty lines."
  (save-excursion
    (goto-char (point-at-bol))
    (unless (looking-back "\* \n")
      (if (looking-back "\\s-+" nil 'greedy)
          (replace-match ""))
      (or (bobp)
          (insert "\n"))
      )
    (while (> N 0)
      (insert "\n")
      (setq N (1- N))
      )
    ))

;;;;; skel and abbrev for emacs lisp source block in org-mode

(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))
(define-skeleton skel-org-block-elisp 
  "Insert a org emacs-lisp block" 
  "" 
  "#+BEGIN_SRC emacs-lisp" \n 
  _ - \n 
  "#+END_SRC" \n)
(eval-after-load "org" 
  '(define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp))

;;;; .txt files in owncloud

(defun use-org-mode-for-dot-txt-files-in-owncloud ()
  (when (and 
	 ;; (equal (file-name-directory buffer-file-name) owncloud)
	 (string-match owncloud buffer-file-name)
	 (string-match "\\.txt\\'" buffer-file-name)
	 )
    (org-mode)))
(add-hook 'find-file-hook 'use-org-mode-for-dot-txt-files-in-owncloud)

;;;; org settings

;; the convention for org-agenda key-binding
(global-set-key (kbd "C-c a") 'org-agenda)

;; align sections for easier viewing
(setq org-startup-indented t)

;; navigate org files easier
(setq org-use-speed-commands t)
(setq org-fast-tag-selection-single-key t)
(setq org-return-follows-link t)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)

;; org protocol, i use this for adding website bookmarks to my notes
;; (server-start)
(require 'org-protocol)
(global-set-key (kbd "C-c f") 'org-insert-link)

;; prevent annoying empty lines in a couple ways
(setq org-cycle-separator-lines 0)
(setq org-blank-before-new-entry
      '(
        (heading)
        (plain-list-item)
        ))

;; editing source code in org
(setq org-src-fontify-natively t)
(setq org-edit-src-content-indentation 0)

;;;; todotxt

;; our favorite plaintext todo app for mobile phones
(when (locate-library "todotxt")
  (autoload 'todotxt "todotxt")
  (setq todotxt-file (concat dropbox "Apps/todotxt/todo.txt"))
  (global-set-key (kbd "C-x t") 'todotxt))

;;;; file preservation

;; if a file is changed somewhere else (another app, another device), revert to prevent branching mess
(global-auto-revert-mode)

;;; ----------------------------------------
;;; unused
;;;; vim folding

;; this may make emacs vimrc browsing niftier
(defun set-vim-foldmarker (fmr)
  "Set Vim-type foldmarkers for the current buffer"
  (interactive "sSet local Vim foldmarker: ")
  (if (equal fmr "")
      (message "Abort")
    (setq fmr (regexp-quote fmr))
    (set (make-local-variable 'outline-regexp)
	 (concat ".*" fmr "\\([0-9]+\\)"))
    (set (make-local-variable 'outline-level)
	 `(lambda ()
	    (save-excursion
	      (save-match-data
		(re-search-forward ,(concat fmr "\\([0-9]+\\)") nil t)
		(string-to-number (match-string 1))))))))

;;;; hideshow-org (UNUSED)

;; (require 'hideshow-org)
;; (add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . hs-org/minor-mode))

;;;; dired load hook

;; (add-hook 'dired-load-hook 
	  ;; (function
	   ;; (lambda ()
	     ;; (dired-details-hide) ; dired-details,   horizontal
	     ;; )))

;;;; dired mode hook

;; (add-hook 'dired-mode-hook
	  ;; (lambda ()
	    ;; (dired-omit-mode)    ; dired-x,         vertical
            ;; ))

;;;; unused windows fonts

;; :family "FixedSys"
;; :family "DejaVu Sans Mono"
;; :family "Courier"
;; :height 130
;; :family "Courier New"
;; :height 110
;; :family "Lucida Console"
;; :height 100

;;;; new window with ___

;; this is broken... i was trying to make a customized list-buffers
;; (defun jump-to-new-window ()
  ;; (interactive)
  ;; (split-window-below)
  ;; (other-window)
  ;; )
;; (global-set-key (kbd "C-c C-j") 'jump-to-new-window)

;;;; dired-details

;; hide details, conserving space horizontally
;; (when (locate-library "dired-details")
  ;; (require 'dired-details)
  ;; (dired-details-install)
  ;; (setq dired-details-hidden-string "")
  ;; )

;;; customize

;; a perfectly cogent place for customizations to land
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "c:\\Users\\user-name\\AppData\\Roaming\\.emacs.d\\bookmarks")
 '(custom-safe-themes (quote ("146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "70cf411fbf9512a4da81aa1e87b064d3a3f0a47b19d7a4850578c8d64cac2353" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-heading ((t nil)))
 '(bmkp-local-directory ((t (:background "midnight blue"))))
 '(bmkp-local-file-without-region ((t nil))))
