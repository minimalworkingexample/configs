
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
;;; set home

(setq home "/h/")

;;; OS

(cond ((eq system-type 'gnu/linux)
       (setq OS "linux"))
      ((eq system-type 'windows-nt)
       (setq OS "windows")))

(when (string= OS "linux")
  (setq emacsd (concat home ".emacs.d/"))
  (set-face-attribute 'default nil
                      :height 100
                      )
  )

(when (string= OS "windows")
  (setq emacsd (concat home "AppData/Roaming/.emacs.d/"))
  (set-face-attribute 'default nil
                      :family "Consolas"
                      :height 100
                      )
  )

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

;;; locations

;; set shareddrive as /f
      ;; local documents

;; (setq ntfspartition "/media/iam/ntfspartition/")
;; (setq local ntfspartition)

(setq home "/h/")
(setq local "/e/local/")
(setq configs "/e/configs/")

      ;; in home
(setq owncloud (concat home "ownCloud/")
      dropbox (concat home "Dropbox/")
      ;; vimrc (concat home ".vimrc")
      documents (concat home "Documents/")
      ;; in emacsd
      ;; init (concat emacsd "init.el")
      ;; bookmarks (concat emacsd "bookmarks")
      ;; in owncloud
      config (concat owncloud "config/")
      main (concat owncloud "main.txt")
      tech (concat owncloud "tech.txt")
      ;; in local
      routines (concat local "routines.org")
      ;; in configs
      vimrc (concat configs ".vimrc")
      init (concat configs "init.el")
      bookmarks (concat configs "bookmarks")
      )

;;; favoriteslist

(setq favoriteslist '(("m" . main)
                      ("t" . tech)
                      ("i" . init)
                      ("L" . local)
                      ("O" . owncloud)
                      ("C" . configs)
                      ("r" . routines)
                      ("v" . vimrc)
                      ("j" . jobsearch)
                      ("d" . documents)))

;;; favorites jump-to- functions

(dolist (x favoriteslist)
  (let* ((sym (cdr x))
         (func (intern (concat "jump-to-" (symbol-name sym)))))
    (defalias func `(lambda ()
                      (interactive)
                      (find-file ,sym)))
    (global-set-key (kbd (concat "C-c " (car x))) func)))

;;; to wrap or not to wrap

;; don't wrap text, except for org and tex files
(setq-default truncate-lines t)
(add-hook 'org-mode-hook 'visual-line-mode t)
(add-hook 'LaTeX-mode-hook 'visual-line-mode t)

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
;;;; outline & outshine

;; when a .el file is opened, use outline-minor-mode
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

;; gives my init.el nice folding and keybinding defaults
(when (locate-library "outshine")
  (autoload 'outshine-hook-function "outshine")
  (add-hook 'emacs-lisp-mode-hook 'outshine-hook-function))
(setq outshine-startup-folded-p t) ; this doesn't work? 

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
;;;; bookmark(+)

(setq bookmark-default-file bookmarks)
(eval-after-load 'bookmark+
  '(progn
     (set-face-background 'bmkp-local-directory "midnight blue")
     (set-face-foreground 'bmkp-heading nil)
     (set-face-foreground 'bmkp-local-directory nil)
     (set-face-foreground 'bmkp-local-file-without-region nil)
     ))


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
(setq ido-decorations '("\n-> " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
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

;;; miscellaneous
;;;; custom functions
;;;;; utf-8 and unix line endings

;; (set-locale-environment "en_US.UTF-8")

;; (prefer-coding-system 'utf-8-unix)
;; (set-default-coding-systems 'utf-8-unix)
;; (set-terminal-coding-system 'utf-8-unix)
;; (set-keyboard-coding-system 'utf-8-unix)
;; (set-selection-coding-system 'utf-8-unix)
;; (setq-default buffer-file-coding-system 'utf-8-unix)

 ;; (setq buffer-file-coding-system 'utf-8-unix)
 ;; (setq default-file-name-coding-system 'utf-8-unix)
 ;; (setq default-keyboard-coding-system 'utf-8-unix)
 ;; (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
 ;; (setq default-sendmail-coding-system 'utf-8-unix)
 ;; (setq default-terminal-coding-system 'utf-8-unix)

;; (set-language-environment "UTF-8")
;; (set-default buffer-file-coding-system 'utf-8-unix)
;; (set-default-coding-systems 'utf-8-unix)
;; (set-default default-buffer-file-coding-system 'utf-8-unix)

;; (setq coding-system-for-read 'utf-8-unix)
;; (setq coding-system-for-write 'utf-8-unix)

;; (prefer-coding-system 'utf-8-unix)

;; (setq file-coding-system-alist (quote (("" utf-8 . utf-8) ("\\.dz\\'" no-conversion . no-conversion) ("\\.xz\\'" no-conversion . no-conversion) ("\\.lzma\\'" no-conversion . no-conversion) ("\\.lz\\'" no-conversion . no-conversion) ("\\.g?z\\'" no-conversion . no-conversion) ("\\.\\(?:tgz\\|svgz\\|sifz\\)\\'" no-conversion . no-conversion) ("\\.tbz2?\\'" no-conversion . no-conversion) ("\\.bz2\\'" no-conversion . no-conversion) ("\\.Z\\'" no-conversion . no-conversion) ("\\.elc\\'" . utf-8-emacs) ("\\.utf\\(-8\\)?\\'" . utf-8) ("\\.xml\\'" . xml-find-file-coding-system) ("\\(\\`\\|/\\)loaddefs.el\\'" raw-text . raw-text-unix) ("\\.tar\\'" no-conversion . no-conversion) ("\\.po[tx]?\\'\\|\\.po\\." . po-find-file-coding-system) ("\\.\\(tex\\|ltx\\|dtx\\|drv\\)\\'" . latexenc-find-file-coding-system) ("" undecided))))

(modify-coding-system-alist 'file "" 'utf-8-unix)


(defun set-bfr-to-8-unx ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix))
(global-set-key (kbd "C-c u") 'set-bfr-to-8-unx)

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
         (string-match "ownCloud" buffer-file-name)
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

;;;; spaces not tabs

(setq-default indent-tabs-mode nil)

;;; ----------------------------------------
;;; unused
;;;; vim folding

;; this may make emacs vimrc browsing niftier
;; (defun set-vim-foldmarker (fmr)
  ;; "Set Vim-type foldmarkers for the current buffer"
  ;; (interactive "sSet local Vim foldmarker: ")
  ;; (if (equal fmr "")
      ;; (message "Abort")
    ;; (setq fmr (regexp-quote fmr))
    ;; (set (make-local-variable 'outline-regexp)
         ;; (concat ".*" fmr "\\([0-9]+\\)"))
    ;; (set (make-local-variable 'outline-level)
         ;; `(lambda ()
            ;; (save-excursion
              ;; (save-match-data
                ;; (re-search-forward ,(concat fmr "\\([0-9]+\\)") nil t)
                ;; (string-to-number (match-string 1))))))))

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

;;;; jump to favorites, drafts

;; (setq somefile "~/somefile.txt")
;; (setq x '("s" . somefile))
;; (concat "C-c " (car x))
;; (find-file (eval (cdr x)))

;; (dolist (x '(("m" . main)
             ;; ("t" . tech))
           ;; (global-set-key (kbd (concat "C-c " (car x)))           
                           ;; (lambda ()
                             ;; (interactive)
                             ;; (find-file (eval (cdr x)))
                             ;; ))))

;; (global-set-key (kbd "C-c A") 
                ;; (lambda () 
                  ;; (interactive) 
                  ;; (find-file fileA)))

;;;; old bookmark+ load, not needed

;; (when (locate-library "bookmark+")  
  ;; (add-hook 'bookmark-bmenu-mode-hook
            ;; (lambda ()
              ;; (load "bookmark+"))))

;;; customize

;; a perfectly cogent place for customizations to land

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/e/configs/bookmarks"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
