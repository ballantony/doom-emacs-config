;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tony Ballantyne"
      user-mail-address "tony@tonyballantyne.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Swap C and M position on MacBook
(cond (IS-MAC
       (setq mac-command-modifier      'meta
             mac-option-modifier       'alt
             mac-function-modifier     'ctrl
             mac-right-option-modifier 'alt)))

;; Screen position for different machines
;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq initial-frame-alist '((top . 450) (left . 1900) (width . 183) (height . 55)))


;; Set the image and title
(setq fancy-splash-image "~/Dropbox/emacs/img/emi.png")
(setq frame-title-format '("%b – Emics"))

;; Because sometimes you just have to go pure Emacs
(map! "<f6>" #'evil-mode)

;; The one emacs keybinding I can't give up
(map!
   (:after evil
     :m  "C-e" #'end-of-visual-line))

;; My leader mappings
(map! :leader
      (:prefix-map ("j" . "my mappings")
       :desc "Kill popup window" "p" #'+popup/close))


;; org and org roam setup
(after! org
  (setq org-agenda-files '("~/Dropbox/org/"
                           "~/Dropbox/org-roam/daily"))
  ;Used to have lots of these, now just use TODO and DONE
  (setq org-todo-keywords
      '((sequence "TODO"  "|" "DONE"))))

(setq org-roam-directory (file-truename "~/Dropbox/org-roam"))
(setq org-roam-dailies-directory "daily/")
(setq org-roam-capture-templates '(("d" "default" plain "%?" :if-new
  (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: ")
  :unnarrowed t)))

;; Set Mouse 3 for flyspell corrections
(after! flyspell
  (define-key flyspell-mode-map [down-mouse-3] 'flyspell-correct-word) )


;; Make a backup of file on save. Essential for fiction writing.
(setq backups "~/Dropbox/emacs/backups")
(setq backup-directory-alist `(("" . ,backups)))

(setq make-backup-files t)
(setq auto-save-default t)
(setq vc-make-backup-files t)

(setq version-control t	     ;; Use version numbers for backups
      kept-new-versions 16   ;; Number of newest versions to keep
      kept-old-versions 2    ;; Number of oldest versions to keep
      delete-old-versions t ;; Don't ask to delete excess backup versions
;      backup-by-copying t ;; Copy all files, don't rename them
      backup-by-copying-when-linked t) ;; Copy linked files, don't rename.

(defun force-backup-of-buffer () ;; Backup each save, not each session
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  #'force-backup-of-buffer)

;; Yes, I really want to quit.
(setq confirm-kill-emacs nil)
