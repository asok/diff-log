;;; diff-log.el --- Track the last changes to the edited files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Adam Sokolnicki

;; Author: Adam Sokolnicki <asok@Adam-Sokolnickis-MacBook-Pro.local>
;; Keywords: files, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; Code:

(require 'diff)
(require 's)
(require 'cl)
(require 'hl-line)

(defgroup diff-log nil
  "Track the last changes to the edited files."
  :group 'tools
  :group 'files)

(defgroup diff-log-faces nil
  "Faces used by diff-log."
  :group 'diff-log
  :group 'faces)

(defcustom diff-log-buffer-name "*diff-log*"
  "Name of the buffer with the list of the files' diffs."
  :group 'diff-log
  :type 'string)

(defcustom diff-log-max-diffs-length 10
  "Number of diffs to remember per buffer."
  :group 'diff-log
  :type 'integer)

(defface diff-log-file-header-face
  '((t :inherit diff-file-header-face))
  "Face for diff hunk header lines."
  :group 'diff-log-faces)

(defface diff-log-added-face
  '((t :inherit diff-added))
  "Face for diff hunk header lines."
  :group 'diff-log-faces)

(defface diff-log-removed-face
  '((t :inherit diff-removed))
  "Face for diff hunk header lines."
  :group 'diff-log-faces)

(defface diff-log-time-face
  '((t :inherit diff-hunk-header))
  "Face for diff time header lines."
  :group 'diff-log-faces)

(defface diff-log-hunk-header
  '((t :inherit diff-hunk-header))
  "Face for diff hunk header lines."
  :group 'diff-log-faces)

(defvar diff-log--diffs (make-hash-table :test 'equal)
  "The diffs for all of the files are stored in this variable.")

(defvar-local diff-log--last-modified-at nil
  "The timestamp for the last save action on given buffer.")

(defun diff-log-buffer ()
  "Return diff log buffer if exists."
  (get-buffer diff-log-buffer-name))

(defun diff-log--list-entries ()
  "WIP."
  (save-excursion
    (goto-char 0)
    (maphash
     (lambda (file diffs)
       (diff-log--insert-file-name file)
       (cl-loop for diff in (reverse diffs)
                do (diff-log--insert-diff diff file)))
     diff-log--diffs)))

(defun diff-log--insert-file-name (filename)
  "Insert FILENAME into the buffer and put the correct face."
  (insert (concat "Diff log for " filename ":"))
  (diff-log--put-property-on-line 'face 'diff-log-file-header-face)
  (diff-log--put-property-on-line :file-header filename)
  (insert "\n"))

(defun diff-log--insert-diff (diff filename)
  "Insert DIFF of FILENAME into the buffer and put the corrent face."
  (let ((start (point))
        (current-line-num)
        (current-time))
    (cl-loop for line in (butlast (s-split "\n" diff))
             do
             (insert line)
             (cond
              ((s-starts-with? "---" line)
               (diff-log--put-property-on-line 'face 'diff-log-hunk-header))
              ((s-starts-with? "+++" line)
               (diff-log--put-property-on-line 'face 'diff-log-hunk-header))
              ((s-starts-with? "@@" line)
               (diff-log--put-property-on-line 'face 'diff-log-hunk-header))
              ((s-starts-with? "+" line)
               (diff-log--put-property-on-line 'face 'diff-log-added-face))
              ((s-starts-with? "-" line)
               (diff-log--put-property-on-line 'face 'diff-log-removed-face)))
             (insert "\n"))
    ))

(defun diff-log--line-num (line)
  "Extract the line number from the LINE."
  (when (string-match "^@@ -\\([0-9]+\\)" line)
    (string-to-int (match-string 1 line))))

(defun diff-log--time (line)
  "Extract the time from the LINE."
  (when (string-match "^--- .+ @ \\(.+\\)$" line)
    (match-string 1 line)))

(defun diff-log--put-property-on-line (prop value)
  "Set PROP to VALUE in the current line."
  (put-text-property (line-beginning-position) (point) prop value))

;;;###autoload
(defun diff-log--diff-output (buf)
  "Return the output of diff run against file associated with BUF and current content of BUF."
  (let ((old (buffer-file-name buf))
        (new (diff-file-local-copy buf)))
    (shell-command-to-string (format "diff -u --label '%s' --label '%s' %s %s"
                                     (diff-log--timestamped-buffer-name diff-log--last-modified-at)
                                     (diff-log--timestamped-buffer-name (current-time-string))
                                     old
                                     new))))

(defun diff-log--timestamped-buffer-name (time)
  "TIME WIP."
  (concat (buffer-name) " @ " time))


(defun diff-log-store-diff ()
  "Store the diff between current buffer and its file in the `diff-log--diffs'."
  (let* ((buf (current-buffer))
         (filename (buffer-file-name buf))
         (diff (diff-log--diff-output buf))
         (diff-list (gethash filename diff-log--diffs)))
    (puthash
     filename
     (if diff-list
         (concatenate 'list
                      (if (diff-log--will-excede-limit? diff-list)
                          (cdr diff-list)
                        diff-list)
                      (list diff))
       (list diff))
     diff-log--diffs)
    (setq diff-log--last-modified-at (current-time-string))))

(defun diff-log--will-excede-limit? (diff-list)
  "Return t if DIFF-LIST lack 1 element to be full according to `diff-log-max-diffs-length'"
  (eq (length diff-list) diff-log-max-diffs-length))

(defun diff-log-list-refresh ()
  "Refresh the `diff-log-buffer'."
  (interactive)
  (let ((point (point))
        (inhibit-read-only t))
    (erase-buffer)
    (diff-log--list-entries)
    (goto-char point)))

(defun diff-log-list-next ()
  "WIP."
  (interactive)
  (next-line))

(defun diff-log-list-previous ()
  "WIP."
  (interactive)
  (previous-line))

(defun diff-log-goto-next-section ()
  "Move point to the next sibling section."
  (interactive)
  (goto-char (next-single-property-change (point) :section)))

(defun diff-log-goto-previous-section ()
  "Move point to the previous sibling section."
  (interactive)
  (goto-char (previous-single-property-change (point) :section)))

(defconst diff-log--time-header-begin-suffix-re
  ".+ @ [A-Za-z]\\{3\\} [A-Za-z]\\{3\\} [0-9]\\{1,2\\} [0-9]\\{1,2\\}:[0-9]\\{1,2\\}:[0-9]\\{1,2\\} [0-9]\\{4\\}")

(defconst diff-log--time-header-begin-re
  (concat "--- " diff-log--time-header-begin-suffix-re))

(defconst diff-log--time-header-end-re
  (concat "+++ " diff-log--time-header-begin-suffix-re))

(defconst diff-log--hunk-header-re
  "^@@ -[0-9]+.+ @@$")

(defconst diff-log--file-header-re
  "^Diff log for .+:$")

(defun diff-log--section-start-end ()
  "Return a cons cell with format (start . end).

Where start & end are start and end point of a section."
  (cond
   ((diff-log--at-file-header?)
    (cons
     (line-beginning-position)
     (diff-log--search-forward diff-log--file-header-re)))
   ((diff-log--at-time-header-begin?)
    (cons
     (line-beginning-position)
     (diff-log--search-forward diff-log--time-header-begin-re)))
   ((diff-log--at-time-header-end?)
    (cons
     (diff-log--search-backward diff-log--time-header-begin-re)
     (diff-log--search-forward diff-log--time-header-begin-re)))
   ((diff-log--at-hunk-header?)
    (cons
     (line-beginning-position)
     (min (diff-log--search-forward diff-log--hunk-header-re)
          (diff-log--search-forward diff-log--time-header-begin-re))))
   (t
    (cons
     (diff-log--search-backward diff-log--hunk-header-re)
     (min
      (diff-log--search-forward diff-log--hunk-header-re)
      (diff-log--search-forward diff-log--time-header-begin-re))))))

(defun diff-log--at-file-header? ()
  "WIP."
  (string-match-p diff-log--file-header-re (diff-log--current-line)))

(defun diff-log--at-time-header-end? ()
  "WIP."
  (string-match-p diff-log--time-header-end-re (diff-log--current-line)))

(defun diff-log--at-time-header-begin? ()
  "WIP."
  (string-match-p diff-log--time-header-begin-re (diff-log--current-line)))

(defun diff-log--at-hunk-header? ()
  "WIP."
  (string-match-p diff-log--hunk-header-re (diff-log--current-line)))

(defun diff-log--current-line ()
  "Return contents of the current line."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun diff-log--search-backward (re)
  "RE WIP."
  (save-excursion
    (re-search-backward re nil t)))

(defun diff-log--search-forward (re)
  "Return point at the start of the line for which the match against RE was found.
If the match was not found return 1 less than `point-max'."
  (save-excursion
    (forward-char) ;; make sure that the first char in the line is skipped
    (if (re-search-forward re nil t)
        (line-beginning-position)
      (- (point-max) 1))))

(defun diff-log-toggle-section ()
  "WIP."
  (interactive)
  (let ((inhibit-read-only t)
        (start-end (diff-log--section-start-end)))
    (if (get-text-property (diff-log--section-content-start (car start-end)) 'invisible)
        (put-text-property (car start-end) (cdr start-end) 'invisible nil)
      (goto-char (car start-end))
      (put-text-property
       (+ (line-end-position) 1) (cdr start-end) 'invisible t))))

(defun diff-log--section-content-start (header-start)
  "Return point of the first character of the content of a section.

HEADER-START is the point where the header for the section starts."
  (save-excursion
    (goto-char header-start)
    (+ (line-end-position) 1)))

(defun diff-log-on ()
  "Turn the `diff-log-mode' on if the current buffer has a file."
  (when (buffer-file-name (current-buffer))
    (diff-log-mode +1)))

;;;###autoload
(define-minor-mode diff-log-mode
  "Mode for tracking diffs of the edited files"
  :lighter " Diff log"
  (cond
   (diff-log-mode
    (add-hook 'before-save-hook #'diff-log-store-diff t t))
   (t
    (remove-hook 'before-save-hook #'diff-log-store-diff))))

;;;###autoload
(define-globalized-minor-mode diff-log-global-mode
  diff-log-mode
  diff-log-on)

(define-derived-mode diff-log-list-mode special-mode "diff-log"
  "Special mode for diff log list buffer."
  (buffer-disable-undo)
  (use-local-map diff-log-list-mode-map)
  (font-lock-mode)
  (hl-line-mode)
  (setq mode-name "diff-log"
        truncate-lines nil
        diff-log--last-modified-at (current-time-string))
  (set (make-local-variable 'hl-line-range-function) 'diff-log--section-start-end))

(define-key diff-log-list-mode-map (kbd "n")     #'diff-log-list-next)
(define-key diff-log-list-mode-map (kbd "p")     #'diff-log-list-previous)
(define-key diff-log-list-mode-map (kbd "M-n")   #'diff-log-goto-next-section)
(define-key diff-log-list-mode-map (kbd "M-p")   #'diff-log-goto-previous-section)
(define-key diff-log-list-mode-map (kbd "M-<")   #'beginning-of-buffer)
(define-key diff-log-list-mode-map (kbd "M->")   #'end-of-buffer)
(define-key diff-log-list-mode-map (kbd "M-p")   #'diff-log-goto-previous-section)
(define-key diff-log-list-mode-map (kbd "g")     #'diff-log-list-refresh)
(define-key diff-log-list-mode-map (kbd "<tab>") #'diff-log-toggle-section)

;;;###autoload
(defun diff-log ()
  "Switch to `diff-log-buffer' with list of diffs of the edited files."
  (interactive)
  (let ((buffer-p (diff-log-buffer))
        (buffer (get-buffer-create diff-log-buffer-name)))
    (pop-to-buffer buffer)
    (unless buffer-p
      (diff-log-list-mode))
    (diff-log-list-refresh)))

(provide 'diff-log)

;;; diff-log.el ends here
