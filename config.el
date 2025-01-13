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

(setq doom-font "Courier New-14")

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

;; Frame size and position for different machines
;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;(setq initial-frame-alist '((top . 450) (left . 1900) (width . 183) (height . 55)))
(defun tb/set-small-frame ()
  "Set the frame to something like regular emacs size."
  (set-frame-size (selected-frame) 81 43))

(defun tb/set-large-frame ()
  "Set the frame to something like double regular emacs width."
  (set-frame-size (selected-frame) 145 53))

(defun tb/toggle-frame-size ()
  "Set my preferred frame size."
  (interactive)
  (if (< (frame-width) 85)
      (tb/set-large-frame)
    (tb/set-small-frame)))

(defun tb/set-font-size-big ()
  (interactive)
  "Set font to Courier 16"
  (setq doom-font "Courier New-16")
  (doom/reload-font)
  (tb/set-small-frame))


;; iA Writer Mono for Splash Frame
;; Some prefer Duo to Quattro
(defun tb/set-font-ia-writer ()
  (interactive)
  "Set font to be like iA Writer"
  (setq doom-font "iA Writer Quattro V-20")
  (doom/reload-font)
  (setq line-spacing 0.3) ; float gives line spacing, int gives pixels
  (set-window-margins nil 24 24)
  (set-frame-position nil 4 0)
  (set-frame-size (selected-frame) 124 53))

(defun tb/set-font-size-small ()
  (interactive)
  "Set font to Courier 14"
  (setq doom-font "Courier New-14")
  (doom/reload-font))


(map! "<f5>" #'tb/toggle-frame-size)
(map! "<f7>" #'tb/set-font-size-big)


;; Set the initial frame size to accommodate my image
;(tb/set-small-frame)
(tb/set-font-size-big)
;; Experiment with iA Writer font
;(tb/set-font-ia-writer)

;; Set the image and title
(setq fancy-splash-image "~/Dropbox/emacs/img/emi.png")
(setq frame-title-format '("%b – Emics"))

;; Because sometimes you just have to go pure Emacs
(map! "<f6>" #'evil-mode)

;; The +one+ two emacs keybindings I can't give up
(map!
   (:after evil
     :m  "C-e" #'end-of-visual-line))

;; Lissner has rebound C-x C-s in insert mode to yasnippet.
;; I've tried, but C-x C-s to save is too deep in muscle memory.
;; The following unbinds the key in insert mode, so it can fall back to the
;; underlying default, which is to save the buffer.
;; I've then added yasnippet to a new binding
(map! :i "C-x C-s" nil)
(map! "C-x C-y" #'company-yasnippet)

;; org and org roam setup
(after! org
  (setq org-agenda-files '("~/Dropbox/org/"
                           "~/Dropbox/org-roam/daily"))

 ; Used to have lots of these, now just use TODO NEXT and DONE
  (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "|" "DONE")))

 ; GTD means capturing ideas quickly. I don't want to think about where to refile
 ; Everything captured is a TODO, to be refiled later
  (setq org-capture-templates
        (quote (("t" "Todo" entry (file+headline "~/Dropbox/org/gtd.org" "Captured")
                 "** TODO %?"))))

  (setq org-agenda-custom-commands
        '(("1" "Level 1 Overview"
           ((tags-todo  "LEVEL=1+TODO=\"NEXT\""
                        ((org-agenda-overriding-header "Level 1 Next:")))
            (tags-todo  "LEVEL=1+TODO=\"TODO\""
                        ((org-agenda-overriding-header "Level 1 Todos:")))
            ))

	  ("2" "Level 2 Overview"
           ((tags-todo  "LEVEL=2+TODO=\"NEXT\""
                        ((org-agenda-overriding-header "Level 2 Next:")))
            (tags-todo  "LEVEL=2+TODO=\"TODO\""
                        ((org-agenda-overriding-header "Level 2 Todos:")))
            ))))

  ;; Not sure I like these
  ;; (setq org-agenda-category-icon-alist
  ;;       `(("gtd" ,(list (all-the-icons-material "star")) nil nil :ascent center)
  ;;         ("Person" ,(list (all-the-icons-material "person")) nil nil :ascent center)
  ;;         ("Planner" ,(list (all-the-icons-faicon "calendar")) nil nil :ascent center)
  ;;         ("Refile" ,(list (all-the-icons-material "move_to_inbox")) nil nil :ascent center)
  ;;         ("School" ,(list (all-the-icons-material "school")) nil nil :ascent center)
  ;;         ("Tech" ,(list (all-the-icons-material "laptop_mac")) nil nil :ascent center)
  ;;         ("Writing" ,(list (all-the-icons-material "edit")) nil nil :ascent center)
  ;;         ))

  (defun tb/agenda-restrict-this-buffer ()
    "Call projects agenda restricted to this buffer"
    (interactive)
    (org-agenda nil "p" "<"))

  (defun tb/agenda-restrict-this-project ()
    "Restrict agenda to current project"
    (interactive)
    (let ((org-agenda-files (list (projectile-project-root))))
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

  (setq org-startup-folded t)

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

;; Display whitespace characters
;; adapted from a solution by adamroyjones
;; https://github.com/doomemacs/doomemacs/issues/2673
(use-package! whitespace
  :config
  (setq
    whitespace-style '(face tabs tab-mark spaces space-mark trailing newline newline-mark)
    whitespace-display-mappings '(
      (space-mark   ?\     [?\u00B7]     [?.])
      (space-mark   ?\xA0  [?\u00A4]     [?_])
      (newline-mark ?\n    [182 ?\n])
      (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])))
  (global-whitespace-mode +1))


;; My leader mappings
(map! :leader
      (:prefix-map ("j" . "my mappings")
       :desc "Quick capture" "c" #'tb/capture
       :desc "Indent sexp" "q" #'indent-sexp
       :desc "Capture this buffer" "C" #'tb/capture-to-this-buffer
       :desc "Toggle Evil" "e" #'evil-mode
       :desc "Buffer agenda" "b" #'tb/agenda-restrict-this-buffer
       :desc "Project agenda" "p" #'tb/agenda-restrict-this-project
       :desc "Frame Size" "t" #'tb/toggle-frame-size
       :desc "Small font size" "4" #'tb/set-font-size-small
       :desc "Large font size" "6" #'tb/set-font-size-big
       :desc "iA Writer" "i" #'tb/set-font-ia-writer
       :desc "Kill popup window" "w" #'+popup/close))
;; Yes, I really want to quit.
(setq confirm-kill-emacs nil)

;; Key Chord Mappings
;;
(require 'key-chord)
(key-chord-mode t)
; (key-chord-define-global "jj" 'evil-normal-state)

(defun tb/leaving-countdown ()
  "Counts down to leaving date"
  (interactive)
    (let ((leaving-date (encode-time (parse-time-string "19 Jul 2025 13:10:00" ))))
    (setq diff (time-subtract leaving-date (current-time)))
    (print(format-seconds "%D %H %M %S" diff))))


(defun tb/clean-up()
  (interactive)
  (whitespace-mode)
  (goto-char (point-min))
  (flush-lines "^\\s-+$")
  (goto-char (point-min))
;; get rid of extra spaces after bullet  point
  (while (re-search-forward "-   " nil t)
    (replace-match "- ")))
