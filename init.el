(with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode))

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))


(defun template (name path)
  "Insert \label{ARG} \index{\nameref{ARG}} at point"
  (interactive "sFunction name: \nf"(read-file-name "Directory:"))
  (insert "(defun " name "()
(interactive)
(find-file \"" path "\")
)"
))



(defun insert-code-md (code)
  "Insert markdown code style ``` ``` into buffer"
  (interactive "sCode type:")
  (insert "```" code "

```")
  )

(defun insert-iframe (source)
  "Insert markdown/html iframe template"
  (interactive "ssource:")
  (insert "<iframe style='width: 100%; height: 80vh;' src='" source "'></iframe>")
)

(defun e-init()
  (interactive)
  (find-file "~/.emacs.d/init.el")
)

(defun e-spacemacs()
  (interactive)
  (find-file "~/.emacs.d/custom/.spacemacs")
  )

(defun e-custom()
  (interactive)
  (dired "~/.emacs.d")
)

(defun e-zs()
  (interactive)
  (find-file "~/.zshrc")
)


(defun e-todo()
  (interactive)
  (find-file "~/gdrive/notes/Todo/TODO.md")
)


(defun d-infra()
  "Open infra directory"
  (interactive)
  (dired "/home/jeb/Documents/projects/infra/")
)

(defun d-platform()
  "Open infra directory"
  (interactive)
  (dired "/home/jeb/Documents/projects/platform/")
  )

(defun d-tptool-infra()
  "Open infra directory"
  (interactive)
  (dired "/home/jeb/Documents/projects/tptool/infra")
  )

(defun d-notes()
(interactive)
(dired "/home/jeb/gdrive/notes/")
)

(defun d-home()
  (interactive)
  (dired "/home/jeb/")
)

(defun d-projects()
  (interactive)
  (dired "/home/jeb/Documents/projects")
)

(defun e-markdown(file)
  (interactive "f"(read-file-name "Directory:"))
  (httpd-start)
  (find-file file)
  (impatient-mode)
  (imp-set-user-filter 'markdown-html)
  (browse-url-xdg-open (concat "http://127.0.0.1:8080/imp/live/" (file-name-nondirectory file) ))
)

(defun o-markdown()
  (interactive)
  (httpd-start)
  (impatient-mode)
  (imp-set-user-filter 'markdown-html)
  (browse-url-xdg-open (concat "http://127.0.0.1:8080/imp/live/" (file-name-nondirectory buffer-file-name)))
  )


(defun new_vterm ()
  (interactive)
  (vterm 'N)
 )

(defun localhost (port &optional secure-answer)
  (interactive "sPort: \nsSecure? (y/n)")
  (setq my-secure nil)
  (setq my-secure (cl-equalp secure-answer "y"))
  (browse-url (concat"http" (if my-secure "s") "://localhost:" port))
)

(defun choose-keybinding-layout (filename)
  "choose a keybinding layout defined in a file with xmodmap"
  (interactive (list (let ((default-directory "~/.emacs.d/custom/keyboard/"))
                       (read-file-name "Enter file name: "))))
   (shell-command (concat "xmodmap " filename))
)

(defun unzip (file)
  (interactive "f"(read-file-name))
  (shell-command (concat "unzip " file))
)
(defun goto-repo-url ()
  (interactive)
  (setq git-origin (shell-command-to-string "git config remote.origin.url"))
  (setq git-origin-url (s-replace ":" "/" git-origin))
  (setq git-origin-url (s-replace "\n" "" git-origin-url))
  (setq git-origin-url (s-replace "git@" "https://" git-origin-url))
  (setq git-origin-url (s-replace "\.git" "" git-origin-url))
  (browse-url (concat git-origin-url "/pull/"))
)

(defun google-search (query)
  "Opens your standard browser and makes a google search with your input"
  (interactive "squery: ")
  (browse-url (concat "https://www.google.com/search?q=" query))
)
(defun google-search-marked (start end)
  "Opens your standard browser and googles your marked area"
  (interactive "r")
  (setq marked-string (buffer-substring start end))
  (browse-url (concat "https://www.google.com/search?q=" marked-string))
)

(defun ask-openai (prompt)
  "Establish a connection with OpenAI."
  (interactive "sAsk OpenAI:")
  (setq response (shell-command-to-string (concat "print $(curl https://api.openai.com/v1/completions \
                 -H 'Content-Type: application/json' \
                 -H 'Authorization: ' \
                 -d '{
                 \"model\": \"text-davinci-003\",
                 \"prompt\": \"" prompt "\",
                 \"max_tokens\": 500,
                 \"temperature\": 0
                 }' 2>/dev/null" "| jq '.choices[0].text' )"))
  )

  (cond ((get-buffer-window "AI"))
        (t (split-window-right-and-focus) (switch-to-buffer "AI"))
  )
  (with-current-buffer (get-buffer-create "AI") (end-of-buffer) (insert (concat prompt ":" response "
"))
))


(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil)
)

(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun erase-word (arg)
  "erase characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-erase-word (arg)
  "Erase characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun ssh-tramp (arg)
  "Easier ssh tramp access by reading ssh config"
  (interactive (list
                (completing-read "Select host: "
                                 (list (shell-command-to-string "cat ~/.ssh/config | grep Host\ | cut -d ' ' -f 2")
                                       ))))
  (find-file (concat "/ssh:" arg ":~/" ))
)

(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-i") 'previous-line)

(delete-selection-mode 1)
(auto-save-visited-mode 1)
(setq auto-save-interval 1)asdfcs
(setq auto-save-timeout 2)

(setq dgi-auto-hide-details-p nil)
(setq telega-use-docker t)
(setq ac-use-menu-map t)
(setq lsp-signature-auto-activate nil)
(global-set-key (kbd "ĸ") 'kill-this-buffer)
(global-set-key (kbd "“") 'helm-buffers-list)
(global-set-key (kbd "C-<backspace>") 'backward-erase-word)
(global-set-key (kbd "M-d") 'erase-word)
(global-set-key (kbd "M-p") 'drag-stuff-up)
(global-set-key (kbd "M-n") 'drag-stuff-down)
(global-set-key (kbd "œ") 'o-markdown)
(global-set-key (kbd "µ") 'e-markdown)
(global-set-key (kbd "ſ") 'whitespace-cleanup)
(global-set-key (kbd "”") 'd-notes)
(global-set-key (kbd "ß") 'new_vterm)
(global-set-key (kbd "æ") 'vterm-copy-mode)
(global-set-key (kbd "C-(") 'split-window-right-and-focus)
(global-set-key (kbd "C-)") 'split-window-below-and-focus)
(global-set-key (kbd "ð") 'delete-window)
(global-set-key (kbd "đ") 'spacemacs/toggle-maximize-buffer)
(add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable)
(add-hook 'vterm-mode-hook (lambda () (global-hl-line-mode 0)))
(add-hook 'vterm-mode-hook (lambda () (text-scale-decrease 2)))
(add-hook 'yaml-mode-hook (lambda () (auto-complete-mode)))
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode)))
(add-hook 'yaml-mode-hook (lambda () (idle-highlight-mode)))
(global-set-key (kbd "ł") 'switch-to-last-buffer)
(global-set-key (kbd "ŋ") 'magit)
(global-set-key (kbd "←") 'undo)
(global-set-key (kbd "«") 'evil-redo)
(global-set-key (kbd "ŧ") 'tab-new)
(global-set-key (kbd "¢") 'tab-close)
(global-set-key (kbd "↓") 'e-todo)
;; (global-set-key (kbd "“") 'bookmark-set)
;; (global-set-key (kbd "þ") 'bookmark-jump)
(global-set-key (kbd "ħ") 'replace-string)
(global-set-key (kbd "C-x C-r") 'rectangle-mark-mode)
(global-set-key (kbd "C-x C-l") 'string-rectangle)
(set-frame-parameter (selected-frame) 'alpha '(100 100))



(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(load (concat (file-name-directory load-file-name)
              "core/core-versions")
      nil (not init-file-debug))
(load (concat (file-name-directory load-file-name)
              "core/core-load-paths")
      nil (not init-file-debug))
(load (concat spacemacs-core-directory "core-dumper")
      nil (not init-file-debug))

;; Remove compiled core files if they become stale or Emacs version has changed.
(load (concat spacemacs-core-directory "core-compilation")
      nil (not init-file-debug))
(load spacemacs--last-emacs-version-file t (not init-file-debug))
(when (or (not (string= spacemacs--last-emacs-version emacs-version))
          (> 0 (spacemacs//dir-byte-compile-state
                (concat spacemacs-core-directory "libs/"))))
  (spacemacs//remove-byte-compiled-files-in-dir spacemacs-core-directory))
;; Update saved Emacs version.
(unless (string= spacemacs--last-emacs-version emacs-version)
  (spacemacs//update-last-emacs-version))

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  ;; Disabling file-name-handlers for a speed boost during init might seem like
  ;; a good idea but it causes issues like
  ;; https://github.com/syl20bnr/spacemacs/issues/11585 "Symbol's value as
  ;; variable is void: \213" when emacs is not built having:
  ;; `--without-compress-install`
  (let ((please-do-not-disable-file-name-handler-alist nil))
    (require 'core-spacemacs)
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
    (spacemacs/dump-restore-load-path)
    (configuration-layer/load-lock-file)
    (spacemacs/init)
    (configuration-layer/stable-elpa-init)
    (configuration-layer/load)
    (spacemacs-buffer/display-startup-note)
    (spacemacs/setup-startup-hook)
    (spacemacs/dump-eval-delayed-functions)
    (when (and dotspacemacs-enable-server (not (spacemacs-is-dumping-p)))
      (require 'server)
      (when dotspacemacs-server-socket-dir
        (setq server-socket-dir dotspacemacs-server-socket-dir))
      (unless (server-running-p)
        (message "Starting a server...")
        (server-start)))))
