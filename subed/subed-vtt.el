;;; subed-vtt.el --- WebVTT implementation for subed  -*- lexical-binding: t; -*-

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

;; WebVTT implementation for subed-mode.
;; Since WebVTT doesn't use IDs, we'll use the starting timestamp.

;;; Code:

(require 'subed-config)
(require 'subed-debug)
(require 'subed-common)

;;; Syntax highlighting

(defconst subed-vtt-font-lock-keywords
  (list
   '("[0-9]+:[0-9]+:[0-9]+\\.[0-9]+" . 'subed-vtt-time-face)
   '(",[0-9]+ \\(-->\\) [0-9]+:" 1 'subed-vtt-time-separator-face t)
   '("^.*$" . 'subed-vtt-text-face))
  "Highlighting expressions for `subed-mode'.")


;;; Parsing

(defconst subed-vtt--regexp-timestamp "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)\\.\\([0-9]+\\)")
(defconst subed-vtt--regexp-separator "\\(?:[[:blank:]]*\n\\)+[[:blank:]]*\n")

(defun subed-vtt--timestamp-to-msecs (time-string)
  "Find HH:MM:SS.MS pattern in TIME-STRING and convert it to milliseconds.
Return nil if TIME-STRING doesn't match the pattern."
  (save-match-data
    (when (string-match subed-vtt--regexp-timestamp time-string)
      (let ((hours (string-to-number (match-string 1 time-string)))
            (mins  (string-to-number (match-string 2 time-string)))
            (secs  (string-to-number (match-string 3 time-string)))
            (msecs (string-to-number (subed--right-pad (match-string 4 time-string) 3 ?0))))
        (+ (* (truncate hours) 3600000)
           (* (truncate mins) 60000)
           (* (truncate secs) 1000)
           (truncate msecs))))))

(defun subed-vtt--msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS,MS."
  ;; We need to wrap format-seconds in save-match-data because it does regexp
  ;; stuff and we need to preserve our own match-data.
  (concat (save-match-data (format-seconds "%02h:%02m:%02s" (/ msecs 1000)))
          "." (format "%03d" (mod msecs 1000))))

(defun subed-vtt--subtitle-id ()
  "Return the ID of the subtitle at point or nil if there is no ID."
  (save-excursion
    (when (subed-vtt--jump-to-subtitle-id)
      (when (looking-at subed-vtt--regexp-timestamp)
        (match-string 0)))))

(defun subed-vtt--subtitle-id-max ()
  "Return the ID of the last subtitle or nil if there are no subtitles."
  (save-excursion
    (goto-char (point-max))
    (subed-vtt--subtitle-id)))

(defun subed-vtt--subtitle-id-at-msecs (msecs)
  "Return the ID of the subtitle at MSECS milliseconds.
Return nil if there is no subtitle at MSECS."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let* ((secs       (/ msecs 1000))
             (only-hours (truncate (/ secs 3600)))
             (only-mins  (truncate (/ (- secs (* only-hours 3600)) 60))))
        ;; Move to first subtitle in the relevant hour
        (when (re-search-forward (format "\\(%s\\|\\`\\)%02d:" subed-vtt--regexp-separator only-hours) nil t)
          (beginning-of-line)
          ;; Move to first subtitle in the relevant hour and minute
          (re-search-forward (format "\\(\n\n\\|\\`\\)%02d:%02d" only-hours only-mins) nil t)))
      ;; Move to first subtitle that starts at or after MSECS
      (catch 'subtitle-id
        (while (<= (or (subed-vtt--subtitle-msecs-start) -1) msecs)
          ;; If stop time is >= MSECS, we found a match
          (let ((cur-sub-end (subed-vtt--subtitle-msecs-stop)))
            (when (and cur-sub-end (>= cur-sub-end msecs))
              (throw 'subtitle-id (subed-vtt--subtitle-id))))
          (unless (subed-vtt--forward-subtitle-id)
            (throw 'subtitle-id nil)))))))

(defun subed-vtt--subtitle-msecs-start (&optional sub-id)
  "Subtitle start time in milliseconds or nil if it can't be found.
If SUB-ID is not given, use subtitle on point."
  (let ((timestamp (save-excursion
                     (when (subed-vtt--jump-to-subtitle-time-start sub-id)
                       (when (looking-at subed-vtt--regexp-timestamp)
                         (match-string 0))))))
    (when timestamp
      (subed-vtt--timestamp-to-msecs timestamp))))

(defun subed-vtt--subtitle-msecs-stop (&optional sub-id)
  "Subtitle stop time in milliseconds or nil if it can't be found.
If SUB-ID is not given, use subtitle on point."
  (let ((timestamp (save-excursion
                     (when (subed-vtt--jump-to-subtitle-time-stop sub-id)
                       (when (looking-at subed-vtt--regexp-timestamp)
                         (match-string 0))))))
    (when timestamp
      (subed-vtt--timestamp-to-msecs timestamp))))

(defun subed-vtt--subtitle-text (&optional sub-id)
  "Return subtitle's text or an empty string.
If SUB-ID is not given, use subtitle on point."
  (or (save-excursion
        (let ((beg (subed-vtt--jump-to-subtitle-text sub-id))
              (end (subed-vtt--jump-to-subtitle-end sub-id)))
          (when (and beg end)
            (buffer-substring beg end))))
      ""))

(defun subed-vtt--subtitle-relative-point ()
  "Point relative to subtitle's ID or nil if ID can't be found."
  (let ((start-point (save-excursion
                       (when (subed-vtt--jump-to-subtitle-id)
                         (point)))))
    (when start-point
      (- (point) start-point))))

;;; Traversing

(defun subed-vtt--jump-to-subtitle-id (&optional sub-id)
  "Move to the ID of a subtitle and return point.
If SUB-ID is not given, focus the current subtitle's ID.
Return point or nil if no subtitle ID could be found.
WebVTT doesn't use IDs, so we use the starting timestamp instead."
  (interactive)
  (save-match-data
    (if (stringp sub-id)
        ;; Look for a line that contains only the ID, preceded by one or more
        ;; blank lines or the beginning of the buffer.
        (let* ((orig-point (point))
               (regex (concat "\\(" subed-srt--regexp-separator "\\|\\`\\)\\(" (regexp-quote sub-id) "\\)"))
               (match-found (progn (goto-char (point-min))
                                   (re-search-forward regex nil t))))
          (if match-found
              (goto-char (match-beginning 2))
            (goto-char orig-point)))
      ;; Find one or more blank lines.
      (re-search-forward "\\([[:blank:]]*\n\\)+" nil t)
      ;; Find two or more blank lines or the beginning of the buffer, followed
      ;; by line starting with a timestamp.
      (let* ((regex (concat  "\\(" subed-srt--regexp-separator "\\|\\`\\)\\(" subed-vtt--regexp-timestamp "\\)"))
             (match-found (re-search-backward regex nil t)))
        (when match-found
          (goto-char (match-beginning 2)))))
    ;; Make extra sure we're on a timestamp, return nil if we're not
    (when (looking-at "^\\([0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\)")
      (point))))

(defun subed-vtt--jump-to-subtitle-id-at-msecs (msecs)
  "Move point to the ID of the subtitle that is playing at MSECS.
Return point or nil if point is still on the same subtitle.
See also `subed-vtt--subtitle-id-at-msecs'."
  (let ((current-sub-id (subed-vtt--subtitle-id))
        (target-sub-id (subed-vtt--subtitle-id-at-msecs msecs)))
    (when (and target-sub-id current-sub-id (not (= target-sub-id current-sub-id)))
      (subed-vtt--jump-to-subtitle-id target-sub-id))))

(defun subed-vtt--jump-to-subtitle-text-at-msecs (msecs)
  "Move point to the text of the subtitle that is playing at MSECS.
Return point or nil if point is still on the same subtitle.
See also `subed-vtt--subtitle-id-at-msecs'."
  (when (subed-vtt--jump-to-subtitle-id-at-msecs msecs)
    (subed-vtt--jump-to-subtitle-text)))

(defun subed-vtt--jump-to-subtitle-time-start (&optional sub-id)
  "Move point to subtitle's start time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no start time could be found."
  (interactive)
  (save-match-data
    (when (subed-vtt--jump-to-subtitle-id sub-id)
      (when (looking-at subed-vtt--regexp-timestamp)
        (point)))))

(defun subed-vtt--jump-to-subtitle-time-stop (&optional sub-id)
  "Move point to subtitle's stop time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no stop time could be found."
  (interactive)
  (save-match-data
    (when (subed-vtt--jump-to-subtitle-id sub-id)
      (re-search-forward " *--> *" (point-at-eol) t)
      (when (looking-at subed-vtt--regexp-timestamp)
        (point)))))

(defun subed-vtt--jump-to-subtitle-text (&optional sub-id)
  "Move point on the first character of subtitle's text.
If SUB-ID is not given, use subtitle on point.
Return point or nil if a the subtitle's text can't be found."
  (interactive)
  (when (subed-vtt--jump-to-subtitle-id sub-id)
    (forward-line 1)
    (point)))

(defun subed-vtt--jump-to-subtitle-end (&optional sub-id)
  "Move point after the last character of the subtitle's text.
If SUB-ID is not given, use subtitle on point.
Return point or nil if point did not change or if no subtitle end
can be found."
  (interactive)
  (save-match-data
    (let ((orig-point (point)))
      (subed-vtt--jump-to-subtitle-text sub-id)
      ;; Look for next separator or end of buffer.  We can't use
      ;; `subed-vtt--regexp-separator' here because if subtitle text is empty,
      ;; it may be the only empty line in the separator, i.e. there's only one
      ;; "\n".
      (let ((regex (concat "\\([[:blank:]]*\n+" subed-vtt--regexp-timestamp "\\|\\([[:blank:]]*\n*\\)\\'\\)")))
        (when (re-search-forward regex nil t)
          (goto-char (match-beginning 0))))
      (unless (= (point) orig-point)
        (point)))))

(defun subed-vtt--forward-subtitle-id ()
  "Move point to next subtitle's ID.
Return point or nil if there is no next subtitle."
  (interactive)
  (save-match-data
    (when (re-search-forward (concat subed-vtt--regexp-separator subed-vtt--regexp-timestamp) nil t)
      (subed-vtt--jump-to-subtitle-id))))

(defun subed-vtt--backward-subtitle-id ()
  "Move point to previous subtitle's ID.
Return point or nil if there is no previous subtitle."
  (interactive)
  (let ((orig-point (point)))
    (when (subed-vtt--jump-to-subtitle-id)
      (if (re-search-backward (concat "\\(" subed-vtt--regexp-separator "\\|\\`[[:space:]]*\\)\\(" subed-vtt--regexp-timestamp "\\)") nil t)
          (progn
            (goto-char (match-beginning 2))
            (point))
        (goto-char orig-point)
        nil))))

(defun subed-vtt--forward-subtitle-text ()
  "Move point to next subtitle's text.
Return point or nil if there is no next subtitle."
  (interactive)
  (when (subed-vtt--forward-subtitle-id)
    (subed-vtt--jump-to-subtitle-text)))

(defun subed-vtt--backward-subtitle-text ()
  "Move point to previous subtitle's text.
Return point or nil if there is no previous subtitle."
  (interactive)
  (when (subed-vtt--backward-subtitle-id)
    (subed-vtt--jump-to-subtitle-text)))

(defun subed-vtt--forward-subtitle-end ()
  "Move point to end of next subtitle.
Return point or nil if there is no next subtitle."
  (interactive)
  (when (subed-vtt--forward-subtitle-id)
    (subed-vtt--jump-to-subtitle-end)))

(defun subed-vtt--backward-subtitle-end ()
  "Move point to end of previous subtitle.
Return point or nil if there is no previous subtitle."
  (interactive)
  (when (subed-vtt--backward-subtitle-id)
    (subed-vtt--jump-to-subtitle-end)))

(defun subed-vtt--forward-subtitle-time-start ()
  "Move point to next subtitle's start time."
  (interactive)
  (when (subed-vtt--forward-subtitle-id)
    (subed-vtt--jump-to-subtitle-time-start)))

(defun subed-vtt--backward-subtitle-time-start ()
  "Move point to previous subtitle's start time."
  (interactive)
  (when (subed-vtt--backward-subtitle-id)
    (subed-vtt--jump-to-subtitle-time-start)))

(defun subed-vtt--forward-subtitle-time-stop ()
  "Move point to next subtitle's stop time."
  (interactive)
  (when (subed-vtt--forward-subtitle-id)
    (subed-vtt--jump-to-subtitle-time-stop)))

(defun subed-vtt--backward-subtitle-time-stop ()
  "Move point to previous subtitle's stop time."
  (interactive)
  (when (subed-vtt--backward-subtitle-id)
    (subed-vtt--jump-to-subtitle-time-stop)))


;;; Manipulation

(defun subed-vtt--set-subtitle-time-start (msecs &optional sub-id)
  "Set subtitle start time to MSECS milliseconds.

If SUB-ID is not given, set the start of the current subtitle.

Return the new subtitle start time in milliseconds."
  (save-excursion
    (when (or (not sub-id)
              (and sub-id (subed-vtt--jump-to-subtitle-id sub-id)))
      (subed-vtt--jump-to-subtitle-time-start)
      (when (looking-at subed-vtt--regexp-timestamp)
        (replace-match (subed-vtt--msecs-to-timestamp msecs))))))

(defun subed-vtt--set-subtitle-time-stop (msecs &optional sub-id)
  "Set subtitle stop time to MSECS milliseconds.

If SUB-ID is not given, set the stop of the current subtitle.

Return the new subtitle stop time in milliseconds."
  (save-excursion
    (when (or (not sub-id)
              (and sub-id (subed-vtt--jump-to-subtitle-id sub-id)))
      (subed-vtt--jump-to-subtitle-time-stop)
      (when (looking-at subed-vtt--regexp-timestamp)
        (replace-match (subed-vtt--msecs-to-timestamp msecs))))))

(defun subed-vtt--make-subtitle (&optional id start stop text)
  "Generate new subtitle string.

ID, START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.

A newline is appended to TEXT, meaning you'll get two trailing
newlines if TEXT is nil or empty."
  (interactive "P")
  (format "%s --> %s\n%s\n"
          (subed-vtt--msecs-to-timestamp (or start 0))
          (subed-vtt--msecs-to-timestamp (or stop (+ (or start 0)
                                                     subed-default-subtitle-length)))
          (or text "")))

(defun subed-vtt--prepend-subtitle (&optional id start stop text)
  "Insert new subtitle before the subtitle at point.

ID and START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.

Move point to the text of the inserted subtitle.
Return new point."
  (interactive "P")
  (subed-vtt--jump-to-subtitle-id)
  (insert (subed-vtt--make-subtitle id start stop text))
  (save-match-data
    (when (looking-at (concat "\\([[:space:]]*\\|^\\)" subed-vtt--regexp-timestamp))
      (insert "\n")))
  (forward-line -2)
  (subed-vtt--jump-to-subtitle-text))

(defun subed-vtt--append-subtitle (&optional id start stop text)
  "Insert new subtitle after the subtitle at point.

ID, START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.

Move point to the text of the inserted subtitle.
Return new point."
  (interactive "P")
  (unless (subed-vtt--forward-subtitle-id)
    ;; Point is on last subtitle or buffer is empty
    (subed-vtt--jump-to-subtitle-end)
    ;; Moved point to end of last subtitle; ensure separator exists
    (while (not (looking-at "\\(\\`\\|[[:blank:]]*\n[[:blank:]]*\n\\)"))
      (save-excursion (insert ?\n)))
    ;; Move to end of separator
    (goto-char (match-end 0)))
  (insert (subed-vtt--make-subtitle id start stop text))
  ;; Complete separator with another newline unless we inserted at the end
  (save-match-data
    (when (looking-at (concat "\\([[:space:]]*\\|^\\)" subed-vtt--regexp-timestamp))
      (insert ?\n)))
  (forward-line -2)
  (subed-vtt--jump-to-subtitle-text))

(defun subed-vtt--kill-subtitle ()
  "Remove subtitle at point."
  (interactive)
  (let ((beg (save-excursion (subed-vtt--jump-to-subtitle-id)
                             (point)))
        (end (save-excursion (subed-vtt--jump-to-subtitle-id)
                             (when (subed-vtt--forward-subtitle-id)
                               (point)))))
    (if (not end)
        ;; Removing the last subtitle because forward-subtitle-id returned nil
        (setq beg (save-excursion (goto-char beg)
                                  (subed-vtt--backward-subtitle-end)
                                  (1+ (point)))
              end (save-excursion (goto-char (point-max)))))
    (delete-region beg end)))


(defun subed-vtt--merge-with-next ()
  "Merge the current subtitle with the next subtitle.
Update the end timestamp accordingly."
  (interactive)
  (subed-vtt--jump-to-subtitle-end)
  (save-excursion
    (let ((pos (point)) new-end)
      (subed-vtt--forward-subtitle-time-stop)
      (when (looking-at subed-vtt--regexp-timestamp)
        (setq new-end (subed-vtt--timestamp-to-msecs (match-string 0))))
      (subed-vtt--jump-to-subtitle-text)
      (delete-region pos (point))
      (insert "\n")
      (subed-vtt--set-subtitle-time-stop new-end))))


;;; Maintenance

(defun subed-vtt--regenerate-ids ()
  "Not applicable to WebVTT."
  (interactive))

(defvar-local subed-vtt--regenerate-ids-soon-timer nil)
(defun subed-vtt--regenerate-ids-soon ()
  "Not applicable to WebVTT."
  (interactive))

(defun subed-vtt--sanitize ()
  "Remove surplus newlines and whitespace."
  (interactive)
  (atomic-change-group
    (save-match-data
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
       (while (subed-vtt--forward-subtitle-id)
         (let ((prev-sub-end (save-excursion (when (subed-vtt--backward-subtitle-end)
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
         (subed-vtt--jump-to-subtitle-end)
         (unless (looking-at "\n\\'")
           (delete-region (point) (point-max))
           (insert "\n")))

       ;; One space before and after " --> "
       (goto-char (point-min))
       (while (re-search-forward (format "^%s" subed-vtt--regexp-timestamp) nil t)
         (when (looking-at "[[:blank:]]*-->[[:blank:]]*")
           (unless (= (length (match-string 0)) 5)
             (replace-match " --> "))))))))

(defun subed-vtt--validate ()
  "Move point to the first invalid subtitle and report an error."
  (interactive)
  (when (> (buffer-size) 0)
    (atomic-change-group
      (save-match-data
        (let ((orig-point (point)))
          (goto-char (point-min))
          (while (and (re-search-forward (format "\\(%s[[^\\']]\\|\\`\\)" subed-vtt--regexp-separator) nil t)
                      (looking-at "[[:alnum:]]"))
            ;; This regex is stricter than `subed-vtt--regexp-timestamp'
            (unless (looking-at "^[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]\\{3\\}")
              (error "Found invalid start time: %S"  (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1)))
            (when (re-search-forward "[[:blank:]]" (point-at-eol) t)
              (goto-char (match-beginning 0)))
            (unless (looking-at " --> ")
              (error "Found invalid separator between start and stop time: %S"
                     (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1)))
            (condition-case nil
                (forward-char 5)
              (error nil))
            (unless (looking-at "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\},[0-9]\\{3\\}$")
              (error "Found invalid stop time: %S" (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1))))
          (goto-char orig-point))))))

(defun subed-vtt--sort ()
  "Sanitize, then sort subtitles by start time and re-number them."
  (interactive)
  (atomic-change-group
    (subed-vtt--sanitize)
    (subed-vtt--validate)
    (subed-save-excursion
     (goto-char (point-min))
     (sort-subr nil
                ;; nextrecfun (move to next record/subtitle or to end-of-buffer
                ;; if there are no more records)
                (lambda () (unless (subed-vtt--forward-subtitle-id)
                             (goto-char (point-max))))
                ;; endrecfun (move to end of current record/subtitle)
                #'subed-vtt--jump-to-subtitle-end
                ;; startkeyfun (return sort value of current record/subtitle)
                #'subed-vtt--subtitle-msecs-start))))

(defun subed-vtt--init ()
  "This function is called when subed-mode is entered for a SRT file."
  (setq-local subed--subtitle-format "vtt")
  (setq-local font-lock-defaults '(subed-vtt-font-lock-keywords))
  ;; Support for fill-paragraph (M-q)
  (let ((timestamps-regexp (concat subed-vtt--regexp-timestamp
                                   " *--> *"
                                   subed-vtt--regexp-timestamp)))
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
                                        "\\)"))))

(provide 'subed-vtt)
;;; subed-vtt.el ends here
