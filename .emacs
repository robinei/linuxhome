;; basic behavior and looks
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default-font "Envy Code R-10")
(setq default-frame-alist
      '((font . "Envy Code R-10")
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)))

(setq default-directory (concat (getenv "HOME") "/"))

;; syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(load-theme 'wombat t)
(mapc
 (lambda (face)
        (when (eq (face-attribute face :weight) 'bold)
          (set-face-attribute face nil :weight 'normal)))
 (face-list))

;; highlight current line
(defface hl-line '((t (:background "#101010")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t)

;; show line numbers
(global-linum-mode 1)

;; make jarring vertical split line meld with margin/fringe
(set-face-attribute 'vertical-border nil :foreground
		    (face-attribute 'fringe :background))

;; text encoding
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(if (eq system-type 'windows-nt)
    (set-clipboard-coding-system 'utf-16le-dos))

;; reload changed files automatically
(global-auto-revert-mode t)

;; make page up/down able to scroll to very first or last line
(defun do-scroll-up ()
  (interactive)
  (condition-case nil
      (scroll-down) (beginning-of-buffer (goto-char (point-min)))))
(defun do-scroll-down ()
  (interactive)
  (condition-case nil
      (scroll-up) (end-of-buffer (goto-char (point-max)))))
(global-set-key (kbd "<prior>") 'do-scroll-up)
(global-set-key (kbd "<next>") 'do-scroll-down)
(global-set-key (kbd "M-v") 'do-scroll-up)
(global-set-key (kbd "C-v") 'do-scroll-down)






;; ssh file editing
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink")
    (setq tramp-default-method "ssh"))


(add-to-list 'auto-mode-alist '("\\.xmobarrc\\'" . haskell-mode))

;; "find" on windows is not what we want. we want GNU find which we can name "find2"
(when (executable-find "find2")
  (setq find-program "find2"))






(require 'package)
(setq package-enable-at-startup nil)   ; To prevent initialising twice
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package mic-paren
  :ensure t
  :config
  (paren-activate)
  (setq paren-priority 'close))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (when (executable-find "find2")
    (setq projectile-generic-command "find2 . -type f -print0"))
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching nil)
  (projectile-global-mode))

(use-package flx-ido
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil) ; disable ido faces to see flx highlights.
  (setq ido-execute-command-cache nil)
  (defun ido-execute-command ()
    (interactive)
    (call-interactively
     (intern
      (ido-completing-read
       "M-x "
       (progn
	 (unless ido-execute-command-cache
	   (mapatoms (lambda (s)
		       (when (commandp s)
			 (setq ido-execute-command-cache
			       (cons (format "%S" s) ido-execute-command-cache))))))
	 ido-execute-command-cache)))))
  (global-set-key "\M-x" 'ido-execute-command))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  :config
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))




;; reload config file when saved
(add-hook 'after-save-hook 'maybe-reload-config)
(defun maybe-reload-config ()
  (let ((suffix (substring (buffer-file-name) -6 nil)))
    (when (string= suffix ".emacs")
	(reload-config))))
(defun reload-config ()
  (interactive)
  (load-file (expand-file-name "~/.emacs")))

;; shortcut for opening the config file
(defun find-config ()
  (interactive)
  (find-file (expand-file-name "~/.emacs")))
(global-set-key [C-f12] 'find-config)



(defvar previous-column nil "Save the column position")

;; Define the nuke-line function. The line is killed, then the newline
;; character is deleted. The column which the cursor was positioned at is then
;; restored. Because the kill-line function is used, the contents deleted can
;; be later restored by usibackward-delete-char-untabifyng the yank commands.
(defun nuke-line()
  "Kill an entire line, including the trailing newline character"
  (interactive)
  (setq previous-column (current-column))
  (end-of-line)
  (if (= (current-column) 0)
    (delete-char 1)
    (progn
      (beginning-of-line)
      (kill-line)
      (delete-char 1)
      (move-to-column previous-column))))

(global-set-key [f8] 'nuke-line)

(global-set-key [f5]
		`(lambda () "Refresh the buffer from the disk (prompt of modified)."
		   (interactive)
		   (revert-buffer t (not (buffer-modified-p)) t)))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))
(global-set-key (kbd "C-c r")  'rename-file-and-buffer)



(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call “perl x.pl” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, java.
File suffix is used to determine what program to run."
  (interactive)
  (let (extention-alist fname suffix progName cmdStr)
    (setq extention-alist ; a keyed list of file suffix to comand-line program to run
          '(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("rb" . "ruby")
            ("js" . "js")
            ("sh" . "bash")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("java" . "javac")
            )
          )
    (setq fname (buffer-file-name))
    (setq suffix (file-name-extension fname))
    (setq progName (cdr (assoc suffix extention-alist)))
    (setq cmdStr (concat progName " \""   fname "\""))

    (if (string-equal suffix "el")
        (load-file fname)
      (if progName                    ; is not nil
          (progn
            (message "Running...")
            (shell-command cmdStr))
        (message "No recognized program file suffix for this file.")))))

(global-set-key (kbd "<f7>") 'run-current-file)





(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
	  (buffer-list))
  (delete-other-windows))


(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)



(defun dos2unix ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))
(global-set-key (kbd "<f6>") 'dos2unix)


(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "C-c C-k") 'kill-other-buffers)


(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key [C-f1] 'show-file-name)


(defun golisp ()
  (interactive)
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")
  (slime))


(add-hook 'c-mode-common-hook
	  (lambda()
	    (setq c-default-style "bsd"
		  c-basic-offset 4
		  c-indent-level 4)
	    (setq indent-tabs-mode nil)
	    (local-set-key  (kbd "C-c o") 'ff-get-other-file)))
(c-set-offset 'innamespace 0)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; autoindent yanked text
(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(emacs-lisp-mode lisp-mode
                                                      clojure-mode    scheme-mode
                                                      haskell-mode    ruby-mode
                                                      rspec-mode      python-mode
                                                      c-mode          c++-mode
                                                      objc-mode       latex-mode
                                                      plain-tex-mode))
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))

(defadvice kill-line (before check-position activate)
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1))))
(put 'dired-find-alternate-file 'disabled nil)


(defun q-r-word ()
  "Query-replace whole words."
  (interactive)
  (let ((current-prefix-arg  t))
    (call-interactively #'query-replace)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company rtags use-package typescript-mode s python-mode projectile paredit mic-paren helm haskell-mode flycheck flx-ido csharp-mode auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
