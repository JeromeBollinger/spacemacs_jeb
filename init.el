;;; init.el --- Spacemacs Initialization File -*- no-byte-compile: t -*-
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Without this comment emacs25 adds (package-initialize) here
;;(package-initialize)
;;(require 'ergoemacs-mode)
;; Avoid garbage collection during startup.
;; see `SPC h . dotspacemacs-gc-cons' for more info


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

(defun e-zs()
  (interactive)
  (find-file "~/.zshrc")
)

(defun e-alias()
  (interactive)
  (find-file "~/.emacs.d/eshell/alias")
)

(defun e-todo()
  (interactive)
  (find-file "~/gdrive/notes/Todo/TODO.md")
)

(defun d-bachelor-thesis()
  (interactive)
  (dired "/home/jeb//Documents/projects/bachelor_thesis/")
)

(defun d-infra()
  "Open infra directory"
  (interactive)
  (dired "/home/jeb/Documents/projects/infra/")
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

(defun ansible-playbook(book)
  (interactive (read-file-name "playbook:" "~/Documents/projects/infra/playbooks/"))
(shell-command (concat"/home/jeb/Documents/projects" (file-name-nondirectory book)))
)

(defun new_vterm ()
  (interactive)
  (vterm 'N)
 )
(defun new_vterm_window ()
  (interactive)
  (tab-new)
  (vterm 'N)
)

(defun localhost (port &optional secure-answer)
  (interactive "sPort: \nsSecure? (y/n)")
  (setq my-secure nil)
  (setq my-secure (cl-equalp secure-answer "y"))
  (browse-url (concat"http" (if my-secure "s") "://localhost:" port))
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

(defun ask-openai (prompt length)
  "Establish a connection with OpenAI."
  (interactive "sAsk OpenAI:\nslength")
  (setq response (shell-command-to-string (concat "curl https://api.openai.com/v1/completions \
                 -H 'Content-Type: application/json' \
                 -H 'Authorization: ' \
                 -d '{
                 \"model\": \"text-davinci-003\",
                 \"prompt\": \"" prompt "\",
                 \"max_tokens\": "length",
                 \"temperature\": 0
                 }' 2>/dev/null" "| jq '.choices[0].text'")))
  (message response)
)

(global-set-key (kbd "M-p") 'drag-stuff-up)
(global-set-key (kbd "M-n") 'drag-stuff-down)
(global-set-key (kbd "œ") 'o-markdown)
(global-set-key (kbd "µ") 'e-markdown)
(global-set-key (kbd "ſ") 'whitespace-cleanup)
(global-set-key (kbd "”") 'd-notes)
(global-set-key (kbd "ß") 'new_vterm)
(global-set-key (kbd "»") 'new_vterm_window)
(global-set-key (kbd "æ") 'vterm-copy-mode)
(global-set-key (kbd "C-(") 'split-window-right-and-focus)
(global-set-key (kbd "C-)") 'split-window-below-and-focus)
(global-set-key (kbd "ð") 'delete-window)
(global-set-key (kbd "đ") 'spacemacs/toggle-maximize-buffer)
(add-hook 'vterm-mode-hook (lambda () (global-hl-line-mode 0)))
(add-hook 'vterm-mode-hook (lambda () (text-scale-decrease 2)))
(global-set-key (kbd "ł") 'switch-to-last-buffer)
(global-set-key (kbd "ŋ") 'magit)
(global-set-key (kbd "←") 'undo)
(global-set-key (kbd "«") 'evil-redo)
(global-set-key (kbd "ŧ") 'tab-new)
(global-set-key (kbd "¢") 'tab-close)
(global-set-key (kbd "↓") 'e-todo)
(global-set-key (kbd "“") 'bookmark-set)
(global-set-key (kbd "þ") 'bookmark-jump)
(global-set-key (kbd "ħ") 'replace-string)
(set-frame-parameter (selected-frame) 'alpha '(100 100))



(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil)
)


(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%d-%m-%Y)")))

(defun push-eshell()
  "pushes the ~/.emacs.d/eshell/aliases file to gdrive"
  (interactive)
  (move-file-to-trash "~/gdrive/emacs/alias")
  (copy-file "~/.emacs.d/eshell/alias" "~/gdrive/emacs/" "yes")
)

(defun pull-eshell()
  "pulls the init.el file from gdrive"
  (interactive)
  (move-file-to-trash "~/.emacs.d/eshell/alias")
  (copy-file "~/gdrive/emacs/alias" "~/.emacs.d/eshell/")
  )

(defun pull-init()
  "pulls the init.el file from gdrive"
  (interactive)
  (move-file-to-trash "~/.emacs.d/init.el")
  (copy-file "~/gdrive/emacs/init.el" "~/.emacs.d/")
  )

(defun pull-init()
  "pulls the init.el file from gdrive"
  (interactive)
  (move-file-to-trash "~/.emacs.d/init.el")
  (copy-file "~/gdrive/emacs/init.el" "~/.emacs.d/")
  )

(defun push-init()
  "pushes the ~/.emacs.d/init.el file to gdrive"
  (interactive)
  (move-file-to-trash "~/gdrive/emacs/init.el")
  (copy-file "~/.emacs.d/init.el" "~/gdrive/emacs/" "yes")
  )

(defun pull-init()
  "pulls the init.el file from gdrive"
  (interactive)
  (move-file-to-trash "~/.emacs.d/init.el")
  (copy-file "~/gdrive/emacs/init.el" "~/.emacs.d/")
  )

(defun push-spacemacs()
  "pushes the .spacemacs file to gdrive"
  (interactive)
  (move-file-to-trash "~/gdrive/emacs/.spacemacs")
  (copy-file "~/.spacemacs" "~/gdrive/emacs/" "yes")
  )

(defun pull-spacemacs()
  "pulls the .spacemacs file from gdrive"
  (interactive)
  (move-file-to-trash "~/.spacemacs")
  (copy-file "~/gdrive/emacs/.spacemacs" "~/")
  )

(defun push-gdrive()
  "pushes the init and spacmeacs file to gdrive"
  (interactive)
  (push-init)
  (push-spacemacs)
  (push-eshell)
  )

(defun pull-gdrive()
  "pulls the init and spacmeacs file from gdrive"
  (interactive)
  (pull-init)
  (pull-spacemacs)
  (pull-eshell)
  )


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
