;;; todos.el --- Emacs extract todos from buffer

;; Copyright (C) 2017  Gaby Launay

;; Author: Gaby Launay <gaby.launay@tutanota.com>
;; URL: https://github.com/galaunay/todos.el
;; Version: 1.16.1
;; Keywords: Python, IDE, Languages, Tools
;; Package-Requires: (())

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Set of functions to extract and visualize inline todos from buffer

;;; Code:

(defface todos-header-face
  '((t (:inherit header-line
                 :underline t
                 :weight bold)))
  "Face used to highlight headers in `todos'."
  :group 'todos)

(defvar todos-header-face
  'todos-header-face
  "Face used to highlight headers in `todos'.")

(defun todos-display-todos (prefix &optional suffix title grep-in-project
                                   append-to)
  "Grep for strings situated between PREFIX and SUFFIX.
If SUFFIX is nil, used the end of the line.
TITLE is used to add a title to the results (default to 'todos').
if GREP-IN-PROJECT is non-nil, grep in the current projectile project.
If APPEND-TO is non-nil, append the result to an already present
grep buffer."
  (interactive)
  (let* ((current-prefix-arg '-)
         (proj-name (or (if (and (fboundp 'projectile-project-name)
                                 grep-in-project)
                            (projectile-project-name))
                        (buffer-name)))
         (title (or title "todos"))
         (buff-grep-name (format "*[%s]%s*" proj-name title))
         (filename (file-name-nondirectory (buffer-file-name)))
         (nearest-match nil))
    ;; save current buffer
    (save-buffer)
    ;; get nearest match
    (save-mark-and-excursion
     (let ((curpts (point))
           (forpts (save-excursion
                     (search-forward-regexp prefix (point-max) t)))
           (bacpts (save-excursion
                     (search-backward-regexp prefix (point-min) t))))
       (if (and forpts bacpts)
           (if (> (- forpts curpts) (- curpts bacpts))
               (setq nearest-match (line-number-at-pos bacpts))
             (setq nearest-match (line-number-at-pos forpts)))
         (setq nearest-match (line-number-at-pos (or forpts bacpts))))))
     ;; grep
    (if (and grep-in-project
             (fboundp 'projectile-project-p)
             (projectile-project-p)
             (fboundp 'projectile-grep))
      (projectile-grep prefix '-)
      (grep (format "grep -nH -e \"%s.*%s\" %s" prefix suffix filename)))
     (switch-to-buffer-other-window "*grep*")
     (while (not (search-backward "Grep finished" nil t))
       (sleep-for 0 100)
       (goto-char (point-max)))
     ;; check if something was found
     (goto-char (point-min))
     (when (search-forward "no matches found" (point-max) t)
       (with-current-buffer "*grep*"
         (kill-buffer-and-window))
       (error "No matches found"))
     (let ((inhibit-read-only t))
       ;; remove useless lines
       (goto-char (point-min))
       (keep-lines "^[^:\n]+?:[[:digit:]]+?:[^[:digit:]\n]+?[^\n]+$")
       ;; add face for header
       (font-lock-add-keywords nil
                               `((,(concat "^" title) . todos-header-face)))
       ;; remove regexp match
       (let ((regexp (concat "^\\(?2:.+:[[:digit:]]+:\\)" "\\(?3:.*"
                             prefix "\\)" "\\(?1:.*?\\)" suffix))
             (max-length 0))
         ;; Get max header length
         (goto-char (point-min))
         (while (re-search-forward regexp nil t)
           (when (and
                  (> (length (match-string 2)) max-length)
                  (< (length (match-string 2)) 30))
             (setq max-length (length (match-string 2)))))
         ;; Clean lines
         (goto-char (point-min))
         (setq max-length (+ max-length 2))
         (while (re-search-forward regexp nil t)
           (let ((id (match-string 2)))
             (replace-match (concat "\\2"
                                    (make-string (- max-length (length id)) 32)
                                    "\\1")
                            nil nil))))
     ;; copy result to adequat buffer
     (switch-to-buffer buff-grep-name)
       (when (not append-to)
         (delete-region (point-min) (point-max)))
       (goto-char (point-max))
       (insert-buffer-substring "*grep*")
       (kill-buffer "*grep*"))
     ;; remove visual line
     (grep-mode)
     (visual-line-mode 0)
     (setq truncate-lines t)
     (goto-char (point-min))
     ;; go to nearest match if possible
     (if (not nearest-match)
         (forward-line 3)
       (search-forward-regexp (format "^%s:%s:" filename nearest-match) (point-max) t))
     (message "Finished grepping %s" (downcase title))))

(provide 'todos)
;;; todos.el ends here
