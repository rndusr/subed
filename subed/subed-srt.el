;;; subed-srt.el --- SubRip/srt implementation for subed  -*- lexical-binding: t; -*-

;;; License:
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; SubRip/srt implementation for subed-mode.


;;; Code:

(require 'subed)
(require 'subed-config)
(require 'subed-debug)
(require 'subed-common)

;;; Syntax highlighting

(defconst subed-srt-font-lock-keywords
  (list
   '("^[0-9]+$" . 'subed-id-face)
   '("[0-9]+:[0-9]+:[0-9]+,[0-9]+" . 'subed-time-face)
   '(",[0-9]+ +\\(-->\\) +[0-9]+:" 1 'subed-time-separator-face t))
  "Highlighting expressions for `subed-mode'.")


;;; Parsing

(defconst subed-srt--regexp-timestamp "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\),\\([0-9]+\\)")
(defconst subed-srt--regexp-separator "\\(?:[[:blank:]]*\n\\)+[[:blank:]]*\n")

(cl-defmethod subed--timestamp-to-msecs (time-string &context (major-mode subed-srt-mode))
  "Find HH:MM:SS,MS pattern in TIME-STRING and convert it to milliseconds.
Return nil if TIME-STRING doesn't match the pattern.
Use the format-specific function for MAJOR-MODE."
  (when (string-match subed--regexp-timestamp time-string)
    (let ((hours (string-to-number (match-string 1 time-string)))
          (mins  (string-to-number (match-string 2 time-string)))
          (secs  (string-to-number (match-string 3 time-string)))
          (msecs (string-to-number (subed--right-pad (match-string 4 time-string) 3 ?0))))
      (+ (* (truncate hours) 3600000)
         (* (truncate mins) 60000)
         (* (truncate secs) 1000)
         (truncate msecs)))))

(cl-defmethod subed--msecs-to-timestamp (msecs &context (major-mode subed-srt-mode))
  "Convert MSECS to string in the format HH:MM:SS,MS.
Use the format-specific function for MAJOR-MODE."
  (concat (format-seconds "%02h:%02m:%02s" (/ (floor msecs) 1000))
          "," (format "%03d" (mod (floor msecs) 1000))))

(cl-defmethod subed--subtitle-id (&context (major-mode subed-srt-mode))
  "Return the ID of the subtitle at point or nil if there is no ID.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (when (subed-jump-to-subtitle-id)
      (string-to-number (current-word)))))

(cl-defmethod subed--subtitle-id-at-msecs (msecs &context (major-mode subed-srt-mode))
  "Return the ID of the subtitle at MSECS milliseconds.
Return nil if there is no subtitle at MSECS.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (goto-char (point-min))
    (let* ((secs       (/ msecs 1000))
           (only-hours (truncate (/ secs 3600)))
           (only-mins  (truncate (/ (- secs (* only-hours 3600)) 60))))
      ;; Move to first subtitle in the relevant hour
      (when (re-search-forward (format "\\(%s\\|\\`\\)[0-9]+\n%02d:" subed--regexp-separator only-hours) nil t)
        (beginning-of-line)
        ;; Move to first subtitle in the relevant hour and minute
        (re-search-forward (format "\\(\n\n\\|\\`\\)[0-9]+\n%02d:%02d" only-hours only-mins) nil t)))
    ;; Move to first subtitle that starts at or after MSECS
    (catch 'subtitle-id
      (while (<= (or (subed-subtitle-msecs-start) -1) msecs)
        ;; If stop time is >= MSECS, we found a match
        (let ((cur-sub-end (subed-subtitle-msecs-stop)))
          (when (and cur-sub-end (>= cur-sub-end msecs))
            (throw 'subtitle-id (subed-subtitle-id))))
        (unless (subed-forward-subtitle-id)
          (throw 'subtitle-id nil))))))

;;; Traversing

(cl-defmethod subed--jump-to-subtitle-id (&context (major-mode subed-srt-mode) &optional sub-id)
  "Move to the ID of a subtitle and return point.
If SUB-ID is not given, focus the current subtitle's ID.
Return point or nil if no subtitle ID could be found.
Use the format-specific function for MAJOR-MODE."
  (if sub-id
      ;; Look for a line that contains only the ID, preceded by one or more
      ;; blank lines or the beginning of the buffer.
      (let* ((orig-point (point))
             (regex (format "\\(%s\\|\\`\\)\\(%d\\)$" subed--regexp-separator sub-id))
             (match-found (progn (goto-char (point-min))
                                 (re-search-forward regex nil t))))
        (if match-found
            (goto-char (match-beginning 2))
          (goto-char orig-point)))
    ;; Find one or more blank lines.
    (re-search-forward "\\([[:blank:]]*\n\\)+" nil t)
    ;; Find two or more blank lines or the beginning of the buffer, followed
    ;; by line composed of only digits.
    (let* ((regex (concat "\\(" subed--regexp-separator "\\|\\`\\)\\([0-9]+\\)$"))
           (match-found (re-search-backward regex nil t)))
      (when match-found
        (goto-char (match-beginning 2)))))
  ;; Make extra sure we're on an ID, return nil if we're not
  (when (looking-at "^\\([0-9]+\\)$")
    (point)))

(cl-defmethod subed--jump-to-subtitle-time-start (&context (major-mode subed-srt-mode) &optional sub-id)
  "Move point to subtitle's start time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no start time could be found.
Use the format-specific function for MAJOR-MODE."
  (when (subed-jump-to-subtitle-id sub-id)
    (forward-line)
    (when (looking-at subed--regexp-timestamp)
      (point))))

(cl-defmethod subed--jump-to-subtitle-time-stop (&context (major-mode subed-srt-mode) &optional sub-id)
  "Move point to subtitle's stop time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no stop time could be found.
Use the format-specific function for MAJOR-MODE."
  (when (subed-jump-to-subtitle-id sub-id)
    (forward-line 1)
    (re-search-forward " *--> *" (line-end-position) t)
    (when (looking-at subed--regexp-timestamp)
      (point))))

(cl-defmethod subed--jump-to-subtitle-text (&context (major-mode subed-srt-mode) &optional sub-id)
  "Move point on the first character of subtitle's text.
If SUB-ID is not given, use subtitle on point.
Return point or nil if a the subtitle's text can't be found.
Use the format-specific function for MAJOR-MODE."
  (when (subed-jump-to-subtitle-id sub-id)
    (forward-line 2)
    (point)))

(cl-defmethod subed--jump-to-subtitle-end (&context (major-mode subed-srt-mode) &optional sub-id)
  "Move point after the last character of the subtitle's text.
If SUB-ID is not given, use subtitle on point.
Return point or nil if point did not change or if no subtitle end
can be found.  Use the format-specific function for MAJOR-MODE."
  (let ((orig-point (point)))
    (subed-jump-to-subtitle-text sub-id)
    (unless (looking-at "\n") ; handle empty subtitle text
      ;; Look for next separator or end of buffer.
      (let ((regex (concat "\\(" subed--regexp-separator "[0-9]+\n\\|\\([[:blank:]]*\n*\\)\\'\\)")))
        (when (re-search-forward regex nil t)
          (goto-char (match-beginning 0)))))
    (unless (= (point) orig-point)
      (point))))

(cl-defmethod subed--forward-subtitle-id (&context (major-mode subed-srt-mode))
  "Move point to next subtitle's ID.
Return point or nil if there is no next subtitle.
Use the format-specific function for MAJOR-MODE."
  (let ((pos (point)))
    (when (and (bolp) (> (point) (point-min))) (forward-char -1))
    (if (re-search-forward (concat subed--regexp-separator "[0-9]+\n") nil t)
        (subed-jump-to-subtitle-id)
      (goto-char pos)
      nil)))

(cl-defmethod subed--backward-subtitle-id (&context (major-mode subed-srt-mode))
  "Move point to previous subtitle's ID.
Return point or nil if there is no previous subtitle.
Use the format-specific function for MAJOR-MODE."
  (let ((orig-point (point)))
    (when (subed-jump-to-subtitle-id)
      (if (re-search-backward (concat "\\(" subed--regexp-separator "\\|\\`[[:space:]]*\\)" "\\([0-9]+\\)\n") nil t)
          (progn
            (goto-char (match-beginning 2))
            (point))
        (goto-char orig-point)
        nil))))

;;; Manipulation

(cl-defmethod subed--make-subtitle (&context (major-mode subed-srt-mode) &optional id start stop text _)
  "Generate new subtitle string.

ID, START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
COMMENT is ignored.

A newline is appended to TEXT, meaning you'll get two trailing
newlines if TEXT is nil or empty.  Use the format-specific
function for MAJOR-MODE."
  (format "%s\n%s --> %s\n%s\n"
          (or id 0)
          (subed-msecs-to-timestamp (or start 0))
          (subed-msecs-to-timestamp (or stop (+ (or start 0)
                                                subed-default-subtitle-length)))
          (or text "")))

(cl-defmethod subed--prepend-subtitle (&context (major-mode subed-srt-mode) &optional id start stop text comment)
  "Insert new subtitle before the subtitle at point.

ID and START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
COMMENT is ignored.

Move point to the text of the inserted subtitle.
Return new point.  Use the format-specific function for MAJOR-MODE."
  (subed-jump-to-subtitle-id)
  (insert (subed-make-subtitle id start stop text comment))
  (when (looking-at "\\([[:space:]]*\\|^\\)[0-9]+$")
    (insert "\n"))
  (forward-line -2)
  (subed-jump-to-subtitle-text))

(cl-defmethod subed--append-subtitle (&context (major-mode subed-srt-mode) &optional id start stop text comment)
  "Insert new subtitle after the subtitle at point.

ID, START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
COMMENT is ignored.

Move point to the text of the inserted subtitle.
Return new point.  Use the format-specific function for MAJOR-MODE."
  (unless (subed-forward-subtitle-id)
    ;; Point is on last subtitle or buffer is empty
    (subed-jump-to-subtitle-end)
    (when (looking-at "[[:space:]]+")
      (replace-match ""))
    ;; Moved point to end of last subtitle; ensure separator exists
    (while (not (looking-at "\\(\\`\\|[[:blank:]]*\n[[:blank:]]*\n\\)"))
      (save-excursion (insert ?\n)))
    ;; Move to end of separator
    (goto-char (match-end 0)))
  (insert (subed-make-subtitle id start stop text comment))
  ;; Complete separator with another newline unless we inserted at the end
  (when (looking-at "\\([[:space:]]*\\|^\\)[0-9]+$")
    (insert ?\n))
  (forward-line -2)
  (subed-jump-to-subtitle-text))

(cl-defmethod subed--kill-subtitle :after (&context (major-mode subed-srt-mode))
  "Remove subtitle at point.
Use the format-specific function for MAJOR-MODE."
  (subed-regenerate-ids-soon))

(cl-defmethod subed--split-subtitle :after (&context (major-mode subed-srt-mode) &optional _)
  "Split current subtitle at point.
Use the format-specific function for MAJOR-MODE."
  (subed-regenerate-ids-soon))

(cl-defmethod subed--merge-with-next (&context (major-mode subed-srt-mode))
  "Merge the current subtitle with the next subtitle.
Update the end timestamp accordingly.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (subed-jump-to-subtitle-end)
    (let ((pos (point)) new-end)
      (if (subed-forward-subtitle-time-stop)
          (progn
            (when (looking-at subed--regexp-timestamp)
              (setq new-end (subed-timestamp-to-msecs (match-string 0))))
            (subed-jump-to-subtitle-text)
            (delete-region pos (point))
            (insert " ")
            (let ((subed-enforce-time-boundaries nil))
              (subed-set-subtitle-time-stop new-end))
            (run-hooks 'subed-subtitle-merged-hook)
            (subed-regenerate-ids-soon))
        (error "No subtitle to merge into")))))

;;; Maintenance

(cl-defmethod subed--regenerate-ids (&context (major-mode subed-srt-mode))
  "Ensure consecutive, unduplicated subtitle IDs.
Format-specific for MAJOR-MODE."
  (atomic-change-group
    (save-excursion
      (goto-char (point-min))
      (subed-jump-to-subtitle-id)
      (when (looking-at "^[[:digit:]]+$")
        (unless (string= (current-word) "1")
          (delete-region (point) (progn (forward-word 1) (point)))
          (insert "1")))
      (let ((id 2))
        (while (subed-forward-subtitle-id)
          (let ((id-str (number-to-string id)))
            (unless (string= (current-word) id-str)
              (delete-region (point) (progn (forward-word 1) (point)))
              (insert id-str)))
          (setq id (1+ id)))))))

(cl-defmethod subed--sanitize-format (&context (major-mode subed-srt-mode))
  "Remove surplus newlines and whitespace.
Use the format-specific function for MAJOR-MODE."
  (atomic-change-group
    (subed-save-excursion
     ;; Remove trailing whitespace from each line
     (delete-trailing-whitespace (point-min) (point-max))

     ;; Remove leading spaces and tabs from each line
     (goto-char (point-min))
     (while (re-search-forward "^[[:blank:]]+" nil t)
       (replace-match ""))

     ;; Remove leading newlines
     (goto-char (point-min))
     (while (looking-at "\\`\n+")
       (replace-match ""))

     ;; Replace separators between subtitles with double newlines
     (goto-char (point-min))
     (while (subed-forward-subtitle-id)
       (let ((prev-sub-end (save-excursion (when (subed-backward-subtitle-end)
                                             (point)))))
         (when (and prev-sub-end
                    (not (string= (buffer-substring prev-sub-end (point)) "\n\n")))
           (delete-region prev-sub-end (point))
           (insert "\n\n"))))

     ;; Two trailing newline if last subtitle text is empty, one trailing
     ;; newline otherwise; do nothing in empty buffer (no graphical
     ;; characters)
     (goto-char (point-min))
     (when (re-search-forward "[[:graph:]]" nil t)
       (goto-char (point-max))
       (subed-jump-to-subtitle-end)
       (unless (looking-at "\n\\'")
         (delete-region (point) (point-max))
         (insert "\n")))

     ;; One space before and after " --> "
     (goto-char (point-min))
     (while (re-search-forward (format "^%s" subed--regexp-timestamp) nil t)
       (when (looking-at "[[:blank:]]*-->[[:blank:]]*")
         (unless (= (length (match-string 0)) 5)
           (replace-match " --> ")))))))

(cl-defmethod subed--validate-format (&context (major-mode subed-srt-mode))
  "Move point to the first invalid subtitle and report an error.
Use the format-specific function for MAJOR-MODE."
  (when (> (buffer-size) 0)
    (atomic-change-group
      (let ((orig-point (point)))
        (goto-char (point-min))
        (while (and (re-search-forward (format "\\(%s\\|\\`\\)" subed--regexp-separator)
                                       nil t)
                    (looking-at "[[:alnum:]]"))
          (unless (looking-at "^[0-9]+$")
            (error "Found invalid subtitle ID: %S" (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1)))
          (forward-line)
          ;; This regex is stricter than `subed-srt--regexp-timestamp'
          (unless (looking-at "^[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\},[0-9]\\{,3\\}")
            (error "Found invalid start time: %S"  (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1)))
          (when (re-search-forward "[[:blank:]]" (line-end-position) t)
            (goto-char (match-beginning 0)))
          (unless (looking-at " --> ")
            (error "Found invalid separator between start and stop time: %S"
                   (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1)))
          (condition-case nil
              (forward-char 5)
            (error nil))
          (unless (looking-at "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\},[0-9]\\{,3\\}$")
            (error "Found invalid stop time: %S" (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1))))
        (goto-char orig-point)))))

(cl-defmethod subed--insert-subtitle :after (&context (major-mode subed-srt-mode) &optional _)
  "Renumber afterwards.  Format-specific for MAJOR-MODE."
  (subed-regenerate-ids-soon)
  (point))

(cl-defmethod subed--insert-subtitle-adjacent :after (&context (major-mode subed-srt-mode) &optional _)
  "Renumber afterwards.  Format-specific for MAJOR-MODE."
  (subed-regenerate-ids-soon)
  (point))

;;;###autoload
(define-derived-mode subed-srt-mode subed-mode "Subed-SRT"
  "Major mode for editing SubRip subtitle files."
  (setq-local subed--subtitle-format "srt")
  (setq-local subed--regexp-timestamp subed-srt--regexp-timestamp)
  (setq-local subed--regexp-separator subed-srt--regexp-separator)
  (setq-local font-lock-defaults '(subed-srt-font-lock-keywords))
  (setq-local comment-start "{\\")
  (setq-local comment-end "}")
  (modify-syntax-entry ?\{ ". 1")
  (modify-syntax-entry ?\\ ". 2")
  (modify-syntax-entry ?\} ">")
  ;; Support for fill-paragraph (M-q)
  (let ((timestamps-regexp (concat subed--regexp-timestamp
                                   " *--> *"
                                   subed--regexp-timestamp)))
    (setq-local paragraph-separate (concat "^\\("
                                           (mapconcat 'identity `("[[:blank:]]*"
                                                                  "[[:digit:]]+"
                                                                  ,timestamps-regexp) "\\|")
                                           "\\)$"))
    (setq-local paragraph-start (concat "\\("
                                        ;; Mulitple speakers in the same
                                        ;; subtitle are often distinguished with
                                        ;; a "-" at the start of the line.
                                        (mapconcat 'identity '("^-"
                                                               "[[:graph:]]*$") "\\|")
                                        "\\)")))
  (add-hook 'subed-sanitize-functions #'subed-regenerate-ids t t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.srt\\'" . subed-srt-mode))

(provide 'subed-srt)
;;; subed-srt.el ends here
