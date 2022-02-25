;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


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

;; Themes according to mood
;(setq doom-theme 'doom-vibrant)
;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-solarized-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Swap C and M position on MacBook
(cond (IS-MAC
       (setq mac-command-modifier      'meta
             mac-option-modifier       'alt
             mac-function-modifier     'ctrl
             mac-right-option-modifier 'alt)))

;; Screen size and position for different machines
;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;(setq initial-frame-alist '((top . 450) (left . 1900) (width . 183) (height . 55)))
(defun tb/set-small-screen ()
  "Set the screen to something like regular emacs size."
  (set-frame-size (selected-frame) 81 43))

(defun tb/set-large-screen ()
  "Set the screen to something like double regular emacs width."
  (set-frame-size (selected-frame) 180 53))

(defun tb/preferred-screen-size ()
  "Set my preferred screen size."
  (interactive)
  (if (< (frame-width) 85)
      (tb/set-large-screen)
    (tb/set-small-screen)))

(map! "<f5>" #'tb/preferred-screen-size)

;; Set the initial screen size to accommodate my image
(tb/set-small-screen)

;; Set the image and title
(setq fancy-splash-image "~/Dropbox/emacs/img/emi.png")
(setq frame-title-format '("%b – Emics"))

;; Because sometimes you just have to go pure Emacs
(map! "<f6>" #'evil-mode)

;; The one emacs keybinding I can't give up
(map!
   (:after evil
     :m  "C-e" #'end-of-visual-line))

;; org and org roam setup
(after! org
  (setq org-agenda-files '("~/Dropbox/org/"
                           "~/Dropbox/org-roam/daily"))

                                        ; Used to have lots of these, now just use TODO IN PROGRESS and DONE
  (setq org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "|" "DONE")))

                                        ; GTD means capturing ideas quickly. I don't want to think about where to refile
                                        ; Everything captured is a TODO, to be refiled later
  (setq org-capture-templates
        (quote (("t" "Todo" entry (file+headline "~/Dropbox/org/gtd.org" "Captured")
                 "** TODO %?"))))

  (setq org-agenda-custom-commands
        '(("p" "Project Overview"
           ((todo "TODO"
                  ((org-agenda-overriding-header "Todo:")))
            (todo "IN PROGRESS"
                  ((org-agenda-overriding-header "In Progress:")))))

          ("P" "Level 1 Overview"
           ((tags-todo  "LEVEL=1+TODO=\"TODO\""
                        ((org-agenda-overriding-header "Level 1 Todos:")))
            (tags-todo  "LEVEL=1+TODO=\"IN PROGRESS\""
                        ((org-agenda-overriding-header "Level 1 In Progress:")))))

          ("d" "Day view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-scheduled-past-days 5)
                        (org-deadline-warning-days 5)))
            (todo "TODO" )))))

  (defun tb/agenda-restrict-this-buffer ()
    "Call projects agenda restricted to this buffer"
    (interactive)
    (org-agenda nil "p" "<"))

  (defun tb/agenda-restrict-this-project ()
    "Restrict agenda to current project"
    (interactive)
    (let* ((org-agenda-files (list (projectile-project-root))))
      (org-agenda)))

  (defun tb/capture ()
    "Capture to do without options"
    (interactive)
    (org-capture nil "t"))

  (defun tb/capture-to-this-buffer ()
    "Capture note to this buffer"
    (interactive)
    (cond  ((not  (eq major-mode 'org-mode))
            (message "Can't capture to non org-mode buffer"))
           (t
            (let* ((this-file buffer-file-name)
                   (org-capture-templates
                    `(("t" "Todo" entry (file+headline ,this-file "Captured")
                       "** TODO %?"))))
              (org-capture)))))


  (setq org-roam-directory (file-truename "~/Dropbox/org-roam"))
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-capture-templates '(("d" "default" plain "%?" :if-new
                                      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: ")
                                      :unnarrowed t))))



;; thanks to zzamboni.org for this: disable completion of words in org
(defun zz/adjust-org-company-backends ()
  (remove-hook 'after-change-major-mode-hook '+company-init-backends-h)
  (setq-local company-backends nil))
(add-hook! org-mode (zz/adjust-org-company-backends))


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

;; My leader mappings
(map! :leader
      (:prefix-map ("j" . "my mappings")
       :desc "Quick capture" "c" #'tb/capture
       :desc "Capture this buffer" "C" #'tb/capture-to-this-buffer
       :desc "Toggle Evil" "e" #'evil-mode
       :desc "Buffer agenda" "b" #'tb/agenda-restrict-this-buffer
       :desc "Project agenda" "p" #'tb/agenda-restrict-this-project
       :desc "Screen Size" "s" #'tb/preferred-screen-size
       :desc "Kill popup window" "w" #'+popup/close))


;; Yes, I really want to quit.
(setq confirm-kill-emacs nil)
