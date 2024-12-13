;;; subed-common.el --- Subtitle-format agnostic functions  -*- lexical-binding: t; -*-

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
;;
;; The functions in this file do not expect any particular subtitle format.
;; Instead, they expect certain functions to exist that provide navigation and
;; manipulation for whatever format the current buffer contains.

;;; Code:

(require 'cl-macs)
(require 'subed-config)
(require 'subed-debug)
(require 'subed-mpv)

(declare-function subed-tsv-mode "subed-tsv" ())
(declare-function subed-guess-format "subed" (&optional filename))

;;; Generic functions and variables

(defvar-local subed--regexp-separator nil "Regexp separating subtitles.")
(defvar-local subed--regexp-timestamp nil "Regexp matching timestamps.")
(defvar-local subed--enable-point-to-player-sync-after-disabling-loop nil)

;;; Macros

(defmacro subed-define-generic-function (name args &rest body)
  "Declare an object method and provide the old way of calling it.
NAME is the part of the function name that will go after the
subed- prefix.  ARGS are the arguments for the function.  BODY is
the body of the function, and may include a docstring or an
interactive form."
  (declare
   (indent defun)
   (debug defun))
  (let (is-interactive
        doc)
    (when (stringp (car body))
      (setq doc (pop body)))
    (setq is-interactive (eq (caar body) 'interactive))
    `(progn
       (cl-defgeneric ,(intern (concat "subed--" (symbol-name name)))
           ,args
         ,doc
         ,@(if is-interactive
               (cdr body)
             body))
       ;; Define old internal functions as obsolete aliases
       ,@(mapcar (lambda (sub-format)
                   `(define-obsolete-function-alias
                      (quote ,(intern (format "subed-%s--%s" sub-format (symbol-name name))))
                      (function ,(intern (format "subed-%s" (symbol-name name))))
                      "1.0.0"
                      ,doc))
                 '("srt" "vtt" "ass"))
       ,(if is-interactive
            `(defun ,(intern (concat "subed-" (symbol-name name))) ,args
               ,(concat doc "\n\nThis function calls the generic function\n`"
                        (concat "subed--" (symbol-name name)) "' for the actual implementation.")
               ,(car body)
               (,(intern (concat "subed--" (symbol-name name)))
                ,@(delq nil (mapcar (lambda (a)
                                      (unless (string-match "^&" (symbol-name a))
                                        a))
                                    args))))
          `(defalias ',(intern (concat "subed-" (symbol-name name)))
             #',(intern (concat "subed--" (symbol-name name)))
             ,doc)))))

(defmacro subed-save-excursion (&rest body)
  "Restore relative point within current subtitle after executing BODY.
This also works if the buffer changes (e.g. when sorting
subtitles) as long the subtitle IDs don't change."
  (declare (debug t))
  (save-excursion
    `(let ((sub-id (subed-subtitle-id))
           (sub-pos (subed-subtitle-relative-point))
           (pos (point)))
       (progn ,@body)
       (if sub-id
           (progn
             (subed-jump-to-subtitle-id sub-id)
             ;; Subtitle text may have changed and we may not be able to move to the
             ;; exact original position
             (condition-case nil
                 (forward-char sub-pos)
               (beginning-of-buffer nil)
               (end-of-buffer nil)))
         (goto-char pos)))))

(defmacro subed-for-each-subtitle (beg end reverse &rest body)
  "Run BODY for each subtitle between the region specified by BEG and END.
If END is nil, it defaults to `point-max'.
If BEG and END are both nil, run BODY only on the subtitle at point.
If REVERSE is non-nil, start on the subtitle at END and move backwards.
Before BODY is run, point is placed on the subtitle's ID."
  (declare (indent 3)
           (debug t))
  `(atomic-change-group
     (if (not ,beg)
         ;; Run body on subtitle at point
         (save-excursion (subed-jump-to-subtitle-id)
                         ,@body)
       (let ((begm (make-marker))
             (endm (make-marker)))
         (set-marker begm ,beg)
         (set-marker endm (or ,end (point-max)))
         ;; Run body on multiple subtitles
         (if ,reverse
             ;; Iterate backwards
             (save-excursion
               (goto-char endm)
               (unless (subed-jump-to-subtitle-id)
                 (subed-backward-subtitle-id))
               (catch 'first-subtitle-reached
                 (while t
                   ;; The subtitle includes every character up to the next subtitle's ID (or eob)
                   (let ((sub-end (save-excursion (subed-jump-to-subtitle-end))))
                     (when (< sub-end begm)
                       (throw 'first-subtitle-reached t)))
                   (progn ,@body)
                   (unless (subed-backward-subtitle-id)
                     (throw 'first-subtitle-reached t)))))
           ;; Iterate forwards
           (save-excursion
             (goto-char begm)
             (unless (subed-jump-to-subtitle-id)
               (subed-forward-subtitle-id))
             (catch 'last-subtitle-reached
               (while t
                 (when (> (point) endm)
                   (throw 'last-subtitle-reached t))
                 (progn ,@body)
                 (unless (subed-forward-subtitle-id)
                   (throw 'last-subtitle-reached t))))))))))

(defmacro subed-with-subtitle-replay-disabled (&rest body)
  "Run BODY while automatic subtitle replay is disabled."
  (declare (indent defun))
  `(let ((replay-was-enabled-p (subed-replay-adjusted-subtitle-p)))
     (subed-disable-replay-adjusted-subtitle :quiet)
     (progn ,@body)
     (when replay-was-enabled-p
       (subed-enable-replay-adjusted-subtitle :quiet))))

(defvar-local subed--batch-editing nil "Non-nil means suppress hooks and commands meant for interactive use.")
(defmacro subed-batch-edit (&rest body)
  "Run BODY as a batch edit.  Suppress hooks and replays."
  (declare (indent defun))
  `(progn
     (let ((subed--batch-editing t))
       (subed-with-subtitle-replay-disabled
         (subed-disable-sync-point-to-player-temporarily)
         (progn ,@body)))
     (unless subed--batch-editing
       ;; I wonder if we should do this here or if we should rely on
       ;; it being in post-command-hook...
       (when (subed-show-cps-p)
         (subed--move-cps-overlay-to-current-subtitle)
         (subed--update-cps-overlay)))))

(subed-define-generic-function timestamp-to-msecs (time-string)
  "Find timestamp pattern in TIME-STRING and convert it to milliseconds.
Return nil if TIME-STRING doesn't match the pattern.")

(subed-define-generic-function msecs-to-timestamp (msecs)
  "Convert MSECS to string in the subtitle's timestamp format.")

(defun subed-to-msecs (time-string)
  "Convert TIME-STRING to milliseconds."
  (or (and (stringp time-string) (subed-timestamp-to-msecs time-string))
      (cond
       ((numberp time-string) time-string)
       ((string-match "^[0-9\\.]+$" time-string) (string-to-number time-string)))))

(subed-define-generic-function subtitle-id ()
  "Return the ID of the subtitle at point or nil if there is no ID.")

(subed-define-generic-function subtitle-id-max ()
  "Return the ID of the last subtitle or nil if there are no subtitles."
  (save-excursion
    (goto-char (point-max))
    (subed-subtitle-id)))

(subed-define-generic-function subtitle-id-at-msecs (msecs)
  "Return the ID of the subtitle at MSECS milliseconds.
Return nil if there is no subtitle at MSECS.")

(subed-define-generic-function subtitle-start-pos (&optional sub-id)
  "Return the position of the start of the subtitle.
If SUB-ID is not given, use the current subtitle."
  (interactive)
  (save-excursion
    (or
     (subed-jump-to-subtitle-comment sub-id)
     (subed-jump-to-subtitle-id sub-id))
    (point)))

(subed-define-generic-function jump-to-subtitle-start-pos (&optional sub-id)
  "Move to the beginning of a subtitle and return point.
If SUB-ID is not given, focus the current subtitle.
Return point or nil if no subtitle could be found."
  (interactive)
  (or
   (subed-jump-to-subtitle-comment sub-id)
   (subed-jump-to-subtitle-id sub-id)))

(subed-define-generic-function jump-to-subtitle-id (&optional sub-id)
  "Move to the ID of a subtitle and return point.
If SUB-ID is not given, focus the current subtitle's ID.
Return point or nil if no subtitle ID could be found."
  (interactive))

(subed-define-generic-function jump-to-subtitle-time-start (&optional sub-id)
  "Move point to subtitle's start time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no start time could be found."
  (interactive))

(subed-define-generic-function jump-to-subtitle-time-stop (&optional sub-id)
  "Move point to subtitle's stop time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no stop time could be found."
  (interactive))

(subed-define-generic-function jump-to-subtitle-text (&optional sub-id)
  "Move point on the first character of subtitle's text.
If SUB-ID is not given, use subtitle on point.
Return point or nil if a the subtitle's text can't be found."
  (interactive))

(subed-define-generic-function jump-to-subtitle-comment (&optional sub-id)
  "Move point on the first character of subtitle's comment.
If SUB-ID is not given, use subtitle on point.
Return point or nil if a the subtitle's comment can't be found."
  (interactive)
  (ignore sub-id)
  nil)

(subed-define-generic-function jump-to-subtitle-end (&optional sub-id)
  "Move point after the last character of the subtitle's text.
If SUB-ID is not given, use subtitle on point.
Return point or nil if point did not change or if no subtitle end
can be found."
  (interactive))

(subed-define-generic-function jump-to-subtitle-id-at-msecs (msecs)
  "Move point to the ID of the subtitle that is playing at MSECS.
Return point or nil if point is still on the same subtitle.
See also `subed-subtitle-id-at-msecs'."
  (let ((current-sub-id (subed-subtitle-id))
        (target-sub-id (subed-subtitle-id-at-msecs msecs)))
    (when (and target-sub-id (not (equal target-sub-id current-sub-id)))
      (subed-jump-to-subtitle-id target-sub-id))))

(subed-define-generic-function jump-to-subtitle-text-at-msecs (msecs)
  "Move point to the text of the subtitle that is playing at MSECS.
Return point or nil if point is still on the same subtitle.
See also `subed-vtt--subtitle-id-at-msecs'."
  (when (subed-jump-to-subtitle-id-at-msecs msecs)
    (subed-jump-to-subtitle-text)))

(subed-define-generic-function forward-subtitle-start-pos ()
  "Move point to the beginning of the next subtitle.
Return point or nil if there is no next subtitle."
  (interactive)
  (when (subed-forward-subtitle-id)
    (subed-jump-to-subtitle-start-pos)))

(subed-define-generic-function backward-subtitle-start-pos ()
  "Move point to the beginning of the previous subtitle.
Return point or nil if there is no previous subtitle."
  (interactive)
  (when (subed-backward-subtitle-id)
    (subed-jump-to-subtitle-start-pos)))

(subed-define-generic-function forward-subtitle-id ()
  "Move point to next subtitle's ID.
Return point or nil if there is no next subtitle."
  (interactive))

(subed-define-generic-function backward-subtitle-id ()
  "Move point to previous subtitle's ID.
Return point or nil if there is no previous subtitle."
  (interactive))

(subed-define-generic-function forward-subtitle-text ()
  "Move point to next subtitle's text.
Return point or nil if there is no next subtitle."
  (interactive)
  (when (subed-forward-subtitle-id)
    (subed-jump-to-subtitle-text)))

(subed-define-generic-function backward-subtitle-text ()
  "Move point to previous subtitle's text.
Return point or nil if there is no previous subtitle."
  (interactive)
  (when (subed-backward-subtitle-id)
    (subed-jump-to-subtitle-text)))

(subed-define-generic-function forward-subtitle-comment ()
  "Move point to next subtitle's comment.
Return point or nil if there is no next subtitle."
  (interactive)
  (let ((pos (point)))
    (if (and (subed-forward-subtitle-id)
             (subed-jump-to-subtitle-comment))
        (point)
      (goto-char pos)
      nil)))

(subed-define-generic-function backward-subtitle-comment ()
  "Move point to previous subtitle's comment.
Return point or nil if there is no previous subtitle."
  (interactive)
  (let ((pos (point)))
    (if (and (subed-backward-subtitle-id)
             (subed-jump-to-subtitle-comment))
        (point)
      (goto-char pos)
      nil)))

(subed-define-generic-function forward-subtitle-end ()
  "Move point to end of next subtitle.
Return point or nil if there is no next subtitle."
  (interactive)
  (when (subed-forward-subtitle-id)
    (subed-jump-to-subtitle-end)))

(subed-define-generic-function backward-subtitle-end ()
  "Move point to end of previous subtitle.
Return point or nil if there is no previous subtitle."
  (interactive)
  (when (subed-backward-subtitle-id)
    (subed-jump-to-subtitle-end)))

(subed-define-generic-function forward-subtitle-time-start ()
  "Move point to next subtitle's start time."
  (interactive)
  (when (subed-forward-subtitle-id)
    (subed-jump-to-subtitle-time-start)))

(subed-define-generic-function backward-subtitle-time-start ()
  "Move point to previous subtitle's start time."
  (interactive)
  (when (subed-backward-subtitle-id)
    (subed-jump-to-subtitle-time-start)))

(subed-define-generic-function forward-subtitle-time-stop ()
  "Move point to next subtitle's stop time."
  (interactive)
  (when (subed-forward-subtitle-id)
    (subed-jump-to-subtitle-time-stop)))

(subed-define-generic-function backward-subtitle-time-stop ()
  "Move point to previous subtitle's stop time."
  (interactive)
  (when (subed-backward-subtitle-id)
    (subed-jump-to-subtitle-time-stop)))

(subed-define-generic-function subtitle-msecs-start (&optional sub-id)
  "Subtitle start time in milliseconds or nil if it can't be found.
If SUB-ID is not given, use subtitle on point."
  (let ((timestamp (save-excursion
                     (when (subed-jump-to-subtitle-time-start sub-id)
                       (when (looking-at subed--regexp-timestamp)
                         (match-string 0))))))
    (when timestamp
      (subed-timestamp-to-msecs timestamp))))

(subed-define-generic-function subtitle-msecs-stop (&optional sub-id)
  "Subtitle stop time in milliseconds or nil if it can't be found.
If SUB-ID is not given, use subtitle on point."
  (let ((timestamp (save-excursion
                     (when (subed-jump-to-subtitle-time-stop sub-id)
                       (when (looking-at subed--regexp-timestamp)
                         (match-string 0))))))
    (when timestamp
      (subed-timestamp-to-msecs timestamp))))

(subed-define-generic-function subtitle-text (&optional sub-id)
  "Return subtitle's text or an empty string.
If SUB-ID is not given, use subtitle on point."
  (or (save-excursion
        (let ((beg (subed-jump-to-subtitle-text sub-id))
              (end (subed-jump-to-subtitle-end sub-id)))
          (when (and beg end)
            (buffer-substring beg end)))) ""))

(subed-define-generic-function set-subtitle-text (text &optional sub-id)
  "Set subtitle text to TEXT.

If SUB-ID is not given, set the text of the current subtitle."
  (interactive "MNew text: ")
  (subed-jump-to-subtitle-text sub-id)
  (delete-region (point) (or (subed-jump-to-subtitle-end) (point)))
  (insert text))

(subed-define-generic-function subtitle-relative-point ()
  "Point relative to subtitle's ID or nil if ID can't be found."
  (let ((start-point (save-excursion
                       (when (subed-jump-to-subtitle-id)
                         (point)))))
    (when start-point
      (- (point) start-point))))

(subed-define-generic-function subtitle-comment (&optional _)
  "Return subtitle comment or nil if none."
  nil)

(subed-define-generic-function set-subtitle-comment (comment)
  "Set the current subtitle's comment to COMMENT.
If COMMENT is nil or the empty string, remove the comment."
  (interactive "MComment: ")
  (ignore comment)
  (error "Not implemented"))

(subed-define-generic-function set-subtitle-time-start (msecs
                                                        &optional sub-id
                                                        ignore-negative-duration
                                                        ignore-overlap)
  "Set subtitle start time to MSECS milliseconds.

If SUB-ID is not given, set the start of the current subtitle.

If `subed-enforce-time-boundaries' is set to `adjust', adjust the
current subtitle's stop time to avoid negative durations (unless
IGNORE-NEGATIVE-DURATION is non-nil) and adjust the previous subtitle's
stop time to maintain `subed-subtitle-spacing' (unless IGNORE-OVERLAP is
non-nil) if needed.  If `subed-enforce-time-boundaries' is set to
`error', throw an error in those cases.  If
`subed-enforce-time-boundaries' is nil, make the changes without
checking.

Return the new subtitle start time in milliseconds."
  (save-excursion
    (when (or (not sub-id)
              (and sub-id (subed-jump-to-subtitle-id sub-id)))
      (when (< msecs 0)
        (if (eq subed-enforce-time-boundaries 'error)
            (error "Start time %d is negative." msecs)
          (setq msecs 0)))
      (when (and
             (not ignore-negative-duration)
             subed-enforce-time-boundaries
             (> msecs (subed-subtitle-msecs-stop)))
        (pcase subed-enforce-time-boundaries
          ('error
           (error "Start time %s will be after stop time %s"
                  (subed-msecs-to-timestamp msecs)
                  (subed-msecs-to-timestamp (subed-subtitle-msecs-stop))))
          ('clip
           (setq msecs (subed-subtitle-msecs-stop))
           (message "Clipping to %s" (subed-msecs-to-timestamp msecs)))
          ('adjust
           (let ((subed-enforce-time-boundaries nil))
             (subed-set-subtitle-time-stop msecs)
             (message "Adjusted stop time to %s to avoid negative duration"
                      (subed-msecs-to-timestamp (subed-subtitle-msecs-stop)))))))
      (when (and subed-enforce-time-boundaries
                 (not ignore-overlap))
        (subed-save-excursion
         (when (and (subed-backward-subtitle-time-stop)
                    subed-subtitle-spacing
                    (> (subed-subtitle-msecs-stop)
                       (- msecs subed-subtitle-spacing)))
           (pcase subed-enforce-time-boundaries
             ('error
              (error "Start time %s will be too close to previous stop time of %s"
                     (subed-msecs-to-timestamp msecs)
                     (subed-msecs-to-timestamp (subed-subtitle-msecs-stop))))
             ('clip
              (setq msecs (+ (subed-subtitle-msecs-stop) subed-subtitle-spacing))
              (message "Clipping to %s to maintain spacing from previous stop time of %s"
                       msecs
                       (subed-subtitle-msecs-stop)))
             ('adjust
              (let ((subed-enforce-time-boundaries nil))
                (subed-set-subtitle-time-stop (- msecs subed-subtitle-spacing))
                (message "Adjusted previous stop time to %s to maintain spacing"
                         (subed-msecs-to-timestamp (subed-subtitle-msecs-stop)))))))))
      ;; Update loop start if it's within the current subtitle
      (when (and subed--subtitle-loop-start
                 (>= subed--subtitle-loop-start
                    (floor (- (subed-subtitle-msecs-start)
                              (* 1000 (or subed-loop-seconds-before 0)))))
                 (< subed--subtitle-loop-start (subed-subtitle-msecs-stop)))
        (setq subed--subtitle-loop-start
              (floor (- msecs (* 1000 (or subed-loop-seconds-before 0))))))
      (when (and (subed-jump-to-subtitle-time-start sub-id)
                 (looking-at subed--regexp-timestamp))
        (replace-match
         (save-match-data (subed-msecs-to-timestamp msecs)))
        (subed-subtitle-msecs-start)))))

(subed-define-generic-function set-subtitle-time-stop (msecs &optional sub-id
                                                             ignore-negative-duration
                                                             ignore-overlap)
  "Set subtitle stop time to MSECS milliseconds.

If SUB-ID is not given, set the stop of the current subtitle.

If `subed-enforce-time-boundaries' is set to `adjust', adjust the
current subtitle's start time to avoid negative durations (unless
IGNORE-NEGATIVE-DURATION is non-nil) and adjust the next
subtitle's start time to maintain
`subed-subtitle-spacing' (unless IGNORE-OVERLAP is non-nil) if
needed.  If `subed-enforce-time-boundaries' is set to `error',
throw an error in those cases.  If
`subed-enforce-time-boundaries' is nil, make the changes without
checking.

Return the new subtitle stop time in milliseconds."
  (save-excursion
    (when (or (not sub-id)
              (and sub-id (subed-jump-to-subtitle-id sub-id)))
      (let ((current-start (subed-subtitle-msecs-start)))
        (when (subed-jump-to-subtitle-time-stop sub-id)
          (when (and subed-enforce-time-boundaries
                     (not ignore-negative-duration)
                     (< msecs current-start))
            (pcase subed-enforce-time-boundaries
              ('error
               (error "Stop time %s will be before start time %s"
                      (subed-msecs-to-timestamp msecs)
                      (subed-msecs-to-timestamp current-start)))
              ('clip
               (message "Clipping time to %s" (subed-msecs-to-timestamp current-start))
               (setq msecs (subed-subtitle-msecs-start)))
              ('adjust
               (let ((subed-enforce-time-boundaries nil))
                 (subed-set-subtitle-time-start msecs)
                 (message "Adjusted start time to %s to avoid negative duration"
                          (subed-msecs-to-timestamp current-start))))))))
      (when (and subed-enforce-time-boundaries
                 (not ignore-overlap))
        (subed-save-excursion
         (when (and (subed-forward-subtitle-time-stop)
                    subed-subtitle-spacing
                    (< (subed-subtitle-msecs-start)
                       (+ msecs subed-subtitle-spacing)))
           (pcase subed-enforce-time-boundaries
             ('error
              (error "Stop time %s will be too close to next start time of %s"
                     (subed-msecs-to-timestamp msecs)
                     (subed-msecs-to-timestamp (subed-subtitle-msecs-start))))
             ('clip
              (setq msecs (- (subed-subtitle-msecs-start) subed-subtitle-spacing))
              (message "Clipping to %s to preserve spacing"
                       (subed-msecs-to-timestamp msecs)))
             ('adjust
              (let ((subed-enforce-time-boundaries nil))
                (subed-set-subtitle-time-start (+ msecs subed-subtitle-spacing))
                (message "Adjusted next start time to %s to maintain spacing"
                         (subed-msecs-to-timestamp (subed-subtitle-msecs-start)))))))))
      ;; Update loop end if it's within the current subtitle
      (when (and subed--subtitle-loop-stop
                 (> subed--subtitle-loop-stop (subed-subtitle-msecs-start))
                 (<= subed--subtitle-loop-stop
                     (floor (+ (subed-subtitle-msecs-stop)
                               (* 1000 (or subed-loop-seconds-after 0))))))
        (setq subed--subtitle-loop-stop
              (floor (+ msecs (* 1000 (or subed-loop-seconds-after 0))))))
      (when (and
             (subed-jump-to-subtitle-time-stop)
             (looking-at subed--regexp-timestamp))
        (replace-match
         (save-match-data (subed-msecs-to-timestamp msecs)))
        (subed-subtitle-msecs-stop)))))

(subed-define-generic-function make-subtitle (&optional id start stop text comment)
  "Generate new subtitle string.

ID, START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
COMMENT defaults to nil."
  (interactive "P"))

(subed-define-generic-function prepend-subtitle (&optional id start stop text comment)
  "Insert new subtitle before the subtitle at point.

ID and START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
COMMENT defaults to nil.

Move point to the text of the inserted subtitle.
Return new point."
  (interactive "P"))

(subed-define-generic-function append-subtitle (&optional id start stop text comment)
  "Insert new subtitle after the subtitle at point.

ID, START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
COMMENT defaults to nil.

Move point to the text of the inserted subtitle.
Return new point."
  (interactive "P"))

(subed-define-generic-function kill-subtitle ()
  "Remove subtitle at point."
  (interactive)
  (let ((beg (save-excursion (subed-jump-to-subtitle-start-pos)))
        (end (save-excursion (subed-forward-subtitle-start-pos))))
    (if (not end)
        ;; Removing the last subtitle because forward-subtitle-id returned nil
        (setq
         beg (save-excursion (goto-char beg)
                             (subed-backward-subtitle-end)
                             (1+ (point)))
         end (point-max)))
    (when beg
      (remove-overlays beg end)
      (delete-region beg end))))

(defun subed-parse-file (filename &optional mode-func)
  "Return the subtitles from FILENAME in a list.
If MODE-FUNC is non-nil, use that function to initialize the mode.
Otherwise, initialize the mode based on the filename."
  (when (and filename (file-exists-p filename))
    (with-temp-buffer
      (let ((subed-auto-play-media nil))
        (insert-file-contents filename)
        (if mode-func
            (funcall mode-func)
          (let ((mode-entry
                 (seq-find (lambda (mode-alist)
                             (string-match (car mode-alist) filename))
                           auto-mode-alist)))
            (if mode-entry
                (funcall (cdr mode-entry))
              (subed-tsv-mode))))
        (subed-subtitle-list)))))

(defun subed-subtitle ()
  "Return the subtitle at point as a list.
The list is of the form (id start stop text comment)."
  (list
   (subed-subtitle-id)
   (subed-subtitle-msecs-start)
   (subed-subtitle-msecs-stop)
   (subed-subtitle-text)
   (subed-subtitle-comment)))

(defun subed-subtitle-list (&optional beg end)
  "Return the subtitles from BEG to END as a list.
The list will contain entries of the form (id start stop text comment).
If BEG and END are not specified, use the whole buffer."
  (let (result)
    (subed-for-each-subtitle
      (or beg (point-min))
      (or end (point-max))
      nil
      (when (subed-subtitle-msecs-start)
        (setq result (cons (subed-subtitle) result))))
    (nreverse result)))

(defun subed-append-subtitle-list (subtitles)
  "Append SUBTITLES.
SUBTITLES should be a list with entries of the form (id start stop text
 comment)."
  (mapc (lambda (sub) (apply #'subed-append-subtitle sub)) subtitles))

(defun subed-subtitle-list-text (subtitles &optional include-comments)
  "Return the text in SUBTITLES.
If INCLUDE-COMMENTS is non-nil, include the comments.
If INCLUDE-COMMENTS is a function, call the function on comments
before including them."
  (mapconcat
   (lambda (sub)
     (if (and include-comments (elt sub 4) (not (string= (elt sub 4) "")))
         (concat "\n"
                 (if (functionp include-comments)
                     (funcall include-comments (elt sub 4))
                   (elt sub 4))
                 "\n\n"
                 (elt sub 3) "\n")
       (concat (elt sub 3) "\n")))
   subtitles
   ""))

(defun subed-copy-region-text (&optional beg end include-comments)
  "Copy the text from BEG to END to the kill ring.
If BEG and END are not specified, use the whole buffer.
If INCLUDE-COMMENTS is non-nil, include the comments.
If INCLUDE-COMMENTS is a function, call the function on comments
before including them."
  (interactive (list (and (use-region-p) (min (point) (mark)))
                     (and (use-region-p) (max (point) (mark)))
                     current-prefix-arg))
  (kill-new (subed-subtitle-list-text
             (subed-subtitle-list
              beg
              end)
             include-comments)))

(subed-define-generic-function sanitize ()
  "Sanitize this file."
  (interactive)
  (run-hooks 'subed-sanitize-functions))

(subed-define-generic-function sanitize-format ()
  "Remove surplus newlines and whitespace."
  nil)

(subed-define-generic-function validate ()
  "Move point to the first invalid subtitle and report an error."
  (interactive)
  (run-hooks 'subed-validate-functions))

(subed-define-generic-function validate-format ()
  "Validate format-specific rules."
  nil)

(subed-define-generic-function regenerate-ids ()
  "Ensure consecutive, unduplicated subtitle IDs in formats that use them."
  nil)

(defvar-local subed--regenerate-ids-soon-timer nil)
(subed-define-generic-function regenerate-ids-soon ()
  "Delay regenerating subtitle IDs for a short amount of time.

  Run `subed-regenerate-ids' in 100ms unless this function is
called again within the next 100ms, in which case the previously
scheduled call is canceled and another call is scheduled in
100ms."
  (interactive)
  (when subed--regenerate-ids-soon-timer
    (cancel-timer subed--regenerate-ids-soon-timer))
  (setq subed--regenerate-ids-soon-timer
        (run-at-time 0.1 nil (lambda ()
                               (setq subed--regenerate-ids-soon-timer nil)
                               (subed-regenerate-ids)))))

;;; Utilities

(defun subed--right-pad (string length fillchar)
  "Use FILLCHAR to make STRING LENGTH characters long."
  (concat string (make-string (- length (length string)) fillchar)))


;;; Hooks for point motion and subtitle motion

(defvar-local subed--current-point -1)
(defvar-local subed--buffer-chars-modified-tick -1)
(defvar-local subed--current-subtitle-id -1)
(defun subed--post-command-handler ()
  "Detect point motion and user entering text and signal hooks."
  ;; Check for point motion first to avoid expensive calls to subed-subtitle-id
  ;; as often as possible.
  (let ((new-point (point))
        (new-buffer-chars-modified-tick (buffer-chars-modified-tick)))
    (when (and
           new-point subed--current-point
           (not (and (= new-point subed--current-point)
                     (= new-buffer-chars-modified-tick subed--buffer-chars-modified-tick))))

      ;; If point is synced to playback position, temporarily disable that so
      ;; that manual moves aren't cancelled immediately by automated moves.
      (subed-disable-sync-point-to-player-temporarily)

      ;; Store new point and fire signal.
      (setq subed--current-point new-point
            subed--buffer-chars-modified-tick new-buffer-chars-modified-tick)
      (run-hooks 'subed-point-motion-hook)

      ;; Check if point moved across subtitle boundaries.
      (let ((new-sub-id (subed-subtitle-id)))
        (when (and new-sub-id subed--current-subtitle-id
                   (not (funcall (if (stringp subed--current-subtitle-id) 'string= 'equal)
                                 new-sub-id subed--current-subtitle-id)))
          ;; Store new ID and fire signal.
          (setq subed--current-subtitle-id new-sub-id)
          (run-hooks 'subed-subtitle-motion-hook))))))


;;; Adjusting start/stop time individually

(defun subed-adjust-subtitle-time-start (msecs &optional
                                               ignore-negative-duration
                                               ignore-overlap)
  "Add MSECS milliseconds to start time (use negative value to subtract).

Unless either IGNORE-NEGATIVE-DURATION is non-nil or
`subed-enforce-time-boundaries' is nil, adjust MSECS so that the
stop time isn't smaller than the start time.  Zero-length
subtitles are always allowed.

Unless either IGNORE-OVERLAP is non-nil or `subed-enforce-time-boundaries'
is nil, ensure that there are no gaps between subtitles
smaller than `subed-subtitle-spacing' milliseconds by adjusting
MSECS if necessary.

Return the number of milliseconds the start time was adjusted or
nil if nothing changed."
  (subed-disable-sync-point-to-player-temporarily)
  (let* ((msecs-start (subed-subtitle-msecs-start))
         (msecs-new (when msecs-start (+ msecs-start msecs))))
    (when msecs-new
      ;; MSECS-NEW must be bigger than the current start time if we are adding
      ;; or smaller if we are subtracting.
      (setq msecs-new
            (subed-set-subtitle-time-start msecs-new nil
                                           ignore-negative-duration
                                           ignore-overlap))
      (subed--run-subtitle-time-adjusted-hook)
      (- msecs-new msecs-start))))

(defun subed-adjust-subtitle-time-stop (msecs &optional
                                              ignore-negative-duration
                                              ignore-overlap)
  "Add MSECS milliseconds to stop time (use negative value to subtract).

Unless either IGNORE-NEGATIVE-DURATION or
`subed-enforce-time-boundaries' are non-nil, adjust MSECS so that
the stop time isn't smaller than the start time.  Zero-length
subtitles are always allowed.

Unless either IGNORE-OVERLAP or `subed-enforce-time-boundaries'
are non-nil, ensure that there are no gaps between subtitles
smaller than `subed-subtitle-spacing' milliseconds by adjusting
MSECS if necessary.

Return the number of milliseconds the stop time was adjusted or
nil if nothing changed."
  (subed-disable-sync-point-to-player-temporarily)
  (let* ((msecs-stop (subed-subtitle-msecs-stop))
         (msecs-new (when msecs-stop (+ msecs-stop msecs))))
    ;; MSECS-NEW must be bigger than the current stop time if we are adding or
    ;; smaller if we are subtracting.
    (when (and (>= msecs-new 0) ;; Ignore negative times
               (or (and (> msecs 0) (> msecs-new msecs-stop)) ;; Adding
                   (and (< msecs 0) (< msecs-new msecs-stop)))) ;; Subtracting
      (setq msecs-new
            (subed-set-subtitle-time-stop msecs-new nil ignore-negative-duration ignore-overlap))
      (subed--run-subtitle-time-adjusted-hook)
      (- msecs-new msecs-stop))))

(defun subed-increase-start-time (&optional arg)
  "Add `subed-milliseconds-adjust' milliseconds to start time.

Return new start time in milliseconds or nil if it didn't change.

If prefix argument ARG is given, it is used to set
`subed-milliseconds-adjust' before moving subtitles.  If the
prefix argument is given but not numerical,
`subed-milliseconds-adjust' is reset to its default value.

Example usage:
  \\[universal-argument] 1000 \\[subed-increase-start-time]  Increase start time by 1000ms
           \\[subed-increase-start-time]  Increase start time by 1000ms again
   \\[universal-argument] 500 \\[subed-increase-start-time]  Increase start time by 500ms
           \\[subed-increase-start-time]  Increase start time by 500ms again
       \\[universal-argument] \\[subed-increase-start-time]  Increase start time by 100ms (the default)
           \\[subed-increase-start-time]  Increase start time by 100ms (the default) again"
  (interactive "P")
  (subed-adjust-subtitle-time-start (subed-get-milliseconds-adjust arg))
  (when (subed-loop-over-current-subtitle-p)
    (subed--set-subtitle-loop)))

(defun subed-decrease-start-time (&optional arg)
  "Subtract `subed-milliseconds-adjust' milliseconds from start time.

Return new start time in milliseconds or nil if it didn't change.

See `subed-increase-start-time' about ARG."
  (interactive "P")
  (subed-adjust-subtitle-time-start (* -1 (subed-get-milliseconds-adjust arg)))
  (when (subed-loop-over-current-subtitle-p)
    (subed--set-subtitle-loop)))

(defun subed-increase-stop-time (&optional arg)
  "Add `subed-milliseconds-adjust' milliseconds to stop time.

Return new stop time in milliseconds or nil if it didn't change.

See `subed-increase-start-time' about ARG."
  (interactive "P")
  (subed-adjust-subtitle-time-stop (subed-get-milliseconds-adjust arg))
  (when (subed-loop-over-current-subtitle-p)
    (subed--set-subtitle-loop)))

(defun subed-decrease-stop-time (&optional arg)
  "Subtract `subed-milliseconds-adjust' milliseconds from stop time.

Return new stop time in milliseconds or nil if it didn't change.

See `subed-increase-start-time' about ARG."
  (interactive "P")
  (subed-adjust-subtitle-time-stop (* -1 (subed-get-milliseconds-adjust arg)))
  (when (subed-loop-over-current-subtitle-p)
    (subed--set-subtitle-loop)))

(defun subed-copy-player-pos-to-start-time ()
  "Replace current subtitle's start time with current playback time."
  (interactive)
  (when (and subed-mpv-playback-position
           (subed-subtitle-msecs-start))
    (subed-set-subtitle-time-start subed-mpv-playback-position)
    (subed--run-subtitle-time-adjusted-hook)
    subed-mpv-playback-position))

(defun subed-copy-player-pos-to-stop-time ()
  "Replace current subtitle's stop time with current playback time."
  (interactive)
  (when (and subed-mpv-playback-position
           (subed-subtitle-msecs-stop))
    (subed-set-subtitle-time-stop subed-mpv-playback-position)
    (subed--run-subtitle-time-adjusted-hook)
    subed-mpv-playback-position))


;;; Moving subtitles
;;; (adjusting start and stop time by the same amount)

(defun subed--get-move-subtitle-func (msecs)
  "Return subtitle moving function.

When moving subtitles forward (MSECS > 0), we must adjust the
stop time first and adjust the start time by the same amount the
stop time was adjusted.  This ensures that subtitle length
doesn't change if we can't move MSECS milliseconds forward
because we'd overlap with the next subtitle.

When moving subtitles backward (MSECS < 0), it's the same thing
but we move the start time first."
  (if (> msecs 0)
      ;; Moving forward
      (lambda (msecs &optional ignore-overlap)
        (let ((msecs (subed-adjust-subtitle-time-stop msecs
                                                      :ignore-negative-duration
                                                      ignore-overlap)))
          (when msecs (subed-adjust-subtitle-time-start msecs
                                                        :ignore-negative-duration
                                                        ignore-overlap))))
    ;; Moving backward
    (lambda (msecs &optional ignore-overlap)
      (let ((msecs (subed-adjust-subtitle-time-start msecs
                                                     :ignore-negative-duration
                                                     ignore-overlap)))
        (when msecs (subed-adjust-subtitle-time-stop msecs
                                                     :ignore-negative-duration
                                                     ignore-overlap))))))

(defun subed--move-current-subtitle (msecs)
  "Move subtitle on point by MSECS milliseconds."
  (unless (= msecs 0)
    (subed-with-subtitle-replay-disabled
      (cl-flet ((move-subtitle (subed--get-move-subtitle-func msecs)))
        (move-subtitle msecs)))))

(defun subed--scale-subtitles-in-region (msecs beg end)
  "Scale subtitles between BEG and END after moving END milliseconds.
BEG and END specify a region.  This stretches the subtitles so
that they cover the new timespan.  If you want to shift all the
subtitles by the same offset, use `subed-move-subtitles' instead."
  (let* ((beg-point (save-excursion ; normalized to fixed location over BEG
                      (goto-char beg)
                      (subed-jump-to-subtitle-end)
                      (point)))
         (beg-next-point (save-excursion
                           (goto-char beg-point)
                           (subed-forward-subtitle-end)
                           (point)))
         (end-point (save-excursion     ; normalized to fixed location over END
                      (goto-char end)
                      (subed-jump-to-subtitle-end)
                      (point)))
         (end-prev-point (save-excursion
                           (goto-char end-point)
                           (subed-backward-subtitle-end)
                           (point)))
         (beg-start-msecs (save-excursion
                            (goto-char beg-point)
                            (subed-subtitle-msecs-start)))
         (old-end-start-msecs (save-excursion
                                (goto-char end-point)
                                (subed-subtitle-msecs-start))))
    ;; check for improper range (BEG after END)
    (unless (<= beg end)
      (user-error "Can't scale with improper range"))
    ;; check for 0 or 1 subtitle scenario
    (unless (/= beg-point end-point)
      (user-error "Can't scale with fewer than 3 subtitles"))
    ;; check for 2 subtitle scenario
    (unless (/= beg-point end-prev-point)
      (user-error "Can't scale with only 2 subtitles"))
    ;; check for missing timestamps
    (unless beg-start-msecs
      (user-error "Can't scale when first subtitle timestamp missing"))
    (unless old-end-start-msecs
      (user-error "Can't scale when last subtitle timestamp missing"))
    ;; check for range with 0 time interval
    (unless (/= beg-start-msecs old-end-start-msecs)
      (user-error "Can't scale subtitle range with 0 time interval"))
    ;; check for overlap
    (when (and (> msecs 0) (eq subed-enforce-time-boundaries 'error))
      (subed-save-excursion
       (goto-char end-point)
       (let ((old-stop (subed-subtitle-msecs-stop)))
         (when (and (subed-forward-subtitle-time-start)
                    (> (+ old-stop msecs subed-subtitle-spacing)
                       (subed-subtitle-msecs-start)))
           (user-error "Can't scale when extension would overlap subsequent subtitles")))))
    ;; check for non-chronological: last will start before previous subtitle stops
    (let ((list
           (subed-subtitle-list beg end)))
      ;; make sure each subtitle ends before the next subtitle starts
      (while list
        (when (and (cdr list)
                   (> (elt (car list) 2) (elt (cadr list) 1)))
          (user-error "Can't scale when nonchronological subtitles exist"))
        (setq list (cdr list))))
    (unless (= msecs 0)
      (atomic-change-group
        (subed-with-subtitle-replay-disabled
         (cl-flet ((move-subtitle (subed--get-move-subtitle-func msecs)))
          (let* ((new-end-start-msecs (+ old-end-start-msecs msecs))
                 (scale-factor (/ (float (- new-end-start-msecs beg-start-msecs))
                                  (float (- old-end-start-msecs beg-start-msecs))))
                 (scale-subtitles
                  (lambda (&optional reverse)
                    (subed-for-each-subtitle beg-next-point end-prev-point reverse
                      (let ((old-start-msecs (subed-subtitle-msecs-start)))
                        (unless old-start-msecs
                          (user-error "Can't scale when subtitle timestamp missing"))
                        (let* ((new-start-msecs
                                (+ beg-start-msecs
                                   (round (* (- old-start-msecs beg-start-msecs) scale-factor))))
                               (delta-msecs (- new-start-msecs old-start-msecs)))
                          (unless (and (<= beg-start-msecs old-start-msecs)
                                       (>= old-end-start-msecs old-start-msecs))
                            (user-error "Can't scale when nonchronological subtitles exist"))
                          (move-subtitle delta-msecs :ignore-negative-duration)))))))
            (atomic-change-group
              (if (> msecs 0)
                  (save-excursion
                    ;; Moving forward - Start on last subtitle to see if we
                    ;; can move forward.
                    (goto-char end)
                    (move-subtitle msecs)
                    (funcall scale-subtitles :reverse))
                (save-excursion
                  ;; Moving backward - Make sure the last subtitle will not
                  ;; precede the first subtitle.
                  (unless (> new-end-start-msecs beg-start-msecs)
                    (user-error "Can't scale when contraction would eliminate region"))
                  (goto-char end)
                  (move-subtitle msecs :ignore-negative-duration)
                  (funcall scale-subtitles)))))))))))

(defun subed--move-subtitles-in-region (msecs beg end)
  "Move subtitles in region specified by BEG and END by MSECS milliseconds."
  (unless (= msecs 0)
    (atomic-change-group
      (subed-with-subtitle-replay-disabled
        (cl-flet ((move-subtitle (subed--get-move-subtitle-func msecs)))
					;; When moving subtitles forward, the first step is to move the last
					;; subtitle because:
					;;     a) We need to check if we can move at all and abort if not.
					;;     b) We may have to reduce MSECS if we can move but not by the full
					;;        amount. The goal is that all subtitles are moved by the same
					;;        amount and the spacing between subtitles doesn't change.
					;; All other subtitles must be moved without any checks because we only
					;; ensure that the active region as a whole can be moved, not it's
					;; individual parts, which may be too close together or even overlap.
					;; Moving subtitles backward is basically the same thing but vice versa.
					(catch 'bumped-into-subtitle
						(if (> msecs 0)
								(save-excursion
									;; Moving forward - Start on last subtitle to see if/how far
									;; we can move forward.
									(goto-char end)
									(unless (setq msecs (move-subtitle msecs))
										(throw 'bumped-into-subtitle t))
									(subed-backward-subtitle-id)
									(subed-for-each-subtitle beg (point) :reverse
										(move-subtitle msecs :ignore-negative-duration)))
							;; Start on first subtitle to see if/how far we can move backward.
							(save-excursion
								(goto-char beg)
								(unless (setq msecs (move-subtitle msecs))
									(throw 'bumped-into-subtitle t))
								(subed-forward-subtitle-id)
								(subed-for-each-subtitle (point) end nil
									(move-subtitle msecs :ignore-negative-duration))))))))))

(defun subed-scale-subtitles (msecs &optional beg end)
  "Scale subtitles between BEG and END after moving END MSECS.
Use a negative MSECS value to move END backward.
If END is nil, END will be the last subtitle in the buffer.
If BEG is nil, BEG will be the first subtitle in the buffer.

If you want to shift all the subtitles by the same offset, use
`subed-move-subtitles' instead."
  (interactive (list (if current-prefix-arg
                         (prefix-numeric-value current-prefix-arg)
                       (read-number "Milliseconds: "))
                     (when (region-active-p) (point))
                     (when (region-active-p) (mark))))
  (let ((beg (or beg (point-min)))
        (end (or end (point-max))))
    (subed--scale-subtitles-in-region msecs beg end)
    (when (subed-replay-adjusted-subtitle-p)
      (save-excursion
        (goto-char end)
        (subed-jump-to-subtitle-id)
        (subed-mpv-jump (subed-subtitle-msecs-start))))))

(defun subed-scale-subtitles-forward (&optional arg)
  "Scale subtitles after region is extended `subed-milliseconds-adjust'.

Scaling adjusts start and stop by the same amount, preserving
subtitle duration.

All subtitles that are fully or partially in the active region
are moved so they are placed proportionally in the new range.

If prefix argument ARG is given, it is used to extend the end of the region
`subed-milliseconds-adjust' before proportionally adjusting subtitles.  If the
prefix argument is given but not numerical,
`subed-milliseconds-adjust' is reset to its default value.

Example usage:

\\[universal-argument] 1000 \\[subed-scale-subtitles-forward]
Extend region 1000ms forward in time and scale subtitles in region.

\\[subed-scale-subtitles-forward]
Extend region another 1000ms forward in time and scale subtitles again.

\\[universal-argument] 500 \\[subed-scale-subtitles-forward]
Extend region 500ms forward in time and scale subtitles in region.

\\[subed-scale-subtitles-forward]
Extend region another 500ms forward in time and scale subtitles again.

\\[universal-argument] \\[subed-scale-subtitles-forward]
Extend region 100ms (the default) forward in time and scale subtitles in region.

\\[subed-scale-subtitles-forward]
Extend region another 100ms (the default) forward in time
and scale subtitles again.

If you want to shift all the subtitles by the same offset, use
`subed-move-subtitles' instead."
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (subed-get-milliseconds-adjust arg))
        (beg (when mark-active (region-beginning)))
        (end (when mark-active (region-end))))
    (subed-scale-subtitles msecs beg end)))

(defun subed-scale-subtitles-backward (&optional arg)
  "Scale subtitles after region is shortened `subed-milliseconds-adjust'.

See `subed-scale-subtitles-forward' about ARG.

If you want to shift all the subtitles by the same offset, use
`subed-move-subtitles' instead."
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (* -1 (subed-get-milliseconds-adjust arg)))
        (beg (when mark-active (region-beginning)))
        (end (when mark-active (region-end))))
    (subed-scale-subtitles msecs beg end)))

(defun subed-move-subtitles (msecs &optional beg end)
  "Move subtitles between BEG and END MSECS milliseconds forward.
Use a negative MSECS value to move subtitles backward.
If END is nil, move all subtitles from BEG to end of buffer.
If BEG is nil, move only the current subtitle.
After subtitles are moved, replay the first moved subtitle if
replaying is enabled.

To move to a specific timestamp, use
`subed-move-subtitles-to-start-at-timestamp'.

To move the current subtitle and following subtitles by default,
use `subed-shift-subtitles', `subed-shift-subtitle-forward',
or `subed-shift-subtitle-backward'."
  (interactive (list (if current-prefix-arg
                         (prefix-numeric-value current-prefix-arg)
                       (read-number "Milliseconds: "))
                     (when (region-active-p) (point))
                     (when (region-active-p) (mark))))
  (cond ((and beg end) (subed--move-subtitles-in-region msecs beg end))
        (beg (subed--move-subtitles-in-region msecs beg (point-max)))
        (t (subed--move-current-subtitle msecs)))
  (when (subed-replay-adjusted-subtitle-p)
    (save-excursion
      (when beg (goto-char beg))
      (subed-mpv-jump (subed-subtitle-msecs-start)))))

(defun subed-move-subtitles-to-start-at-timestamp (timestamp &optional beg end)
  "Move subtitles between BEG and END to start at TIMESTAMP.
If END is nil, move all subtitles from BEG to end of buffer.
If BEG is nil, move only the current subtitle.
After subtitles are moved, replay the first moved subtitle if
replaying is enabled.

To move the current subtitle and following subtitles by default,
use `subed-shift-subtitles', `subed-shift-subtitles-to-start-at-timestamp',
`subed-shift-subtitle-forward', or `subed-shift-subtitle-backward'."
  (interactive (list (read-string "New start: ")
                     (when (region-active-p) (point))
                     (when (region-active-p) (mark))))
  (subed-move-subtitles (- (subed-timestamp-to-msecs timestamp) (subed-subtitle-msecs-start))
                        beg end))

(defun subed-move-subtitle-forward (&optional arg)
  "Move subtitle `subed-milliseconds-adjust' forward.

Moving adjusts start and stop time by the same amount, preserving
subtitle duration.

All subtitles that are fully or partially in the active region
are moved.

If prefix argument ARG is given, it is used to set
`subed-milliseconds-adjust' before moving subtitles.  If the
prefix argument is given but not numerical,
`subed-milliseconds-adjust' is reset to its default value.

Example usage:
  \\[universal-argument] 1000 \\[subed-move-subtitle-forward]  Move subtitle 1000ms forward in time
           \\[subed-move-subtitle-forward]  Move subtitle 1000ms forward in time again
   \\[universal-argument] 500 \\[subed-move-subtitle-forward]  Move subtitle 500ms forward in time
           \\[subed-move-subtitle-forward]  Move subtitle 500ms forward in time again
       \\[universal-argument] \\[subed-move-subtitle-forward]  Move subtitle 100ms (the default) forward in time
           \\[subed-move-subtitle-forward]  Move subtitle 100ms (the default) forward in time again"
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (subed-get-milliseconds-adjust arg))
        (beg (when mark-active (region-beginning)))
        (end (when mark-active (region-end))))
    (subed-move-subtitles msecs beg end)))

(defun subed-move-subtitle-backward (&optional arg)
  "Move subtitle `subed-milliseconds-adjust' backward.

See `subed-move-subtitle-forward' about ARG."
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (* -1 (subed-get-milliseconds-adjust arg)))
        (beg (when mark-active (region-beginning)))
        (end (when mark-active (region-end))))
    (subed-move-subtitles msecs beg end)))


;;; Shifting subtitles
;;; (same as moving, but follow-up subtitles are also moved)

(defun subed-shift-subtitles (&optional arg)
  "Move this and following subtitles by ARG milliseconds.
To shift to a specific timestamp, use
`subed-shift-subtitles-to-start-at-timestamp'."
  (interactive (list (if current-prefix-arg
                         (prefix-numeric-value current-prefix-arg)
                       (read-number "Milliseconds: "))))
  (let ((deactivate-mark nil)
        (msecs (subed-get-milliseconds-adjust arg)))
    (subed-move-subtitles msecs (point))))

(defun subed-shift-subtitles-to-start-at-timestamp (timestamp)
  "Move this and following subtitles to  starts at TIMESTAMP.
To shift by a millisecond offset, use `subed-shift-subtitles'.  If
TIMESTAMP is a number or a numeric string, treat it as the time in
milliseconds."
  (interactive (list (read-string "New start: ")))
  (subed-shift-subtitles (- (subed-to-msecs timestamp) (subed-subtitle-msecs-start))))

(defun subed-shift-subtitle-forward (&optional arg)
  "Shift subtitle `subed-milliseconds-adjust' forward.

Shifting is like moving, but it always moves the subtitles
between point and the end of the buffer.

See `subed-move-subtitle-forward' about ARG."
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (subed-get-milliseconds-adjust arg)))
    (subed-move-subtitles msecs (point))))

(defun subed-shift-subtitle-backward (&optional arg)
  "Shift subtitle `subed-milliseconds-adjust' backward.

Shifting is like moving, but it always moves the subtitles
between point and the end of the buffer.

See `subed-move-subtitle-forward' about ARG."
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (* -1 (subed-get-milliseconds-adjust arg))))
    (subed-move-subtitles msecs (point))))


;;; Inserting

(defun subed--insert-subtitle-info (arg)
  "Provide information for inserting subtitles.
ARG is the user-given argument.
Return a list of values in the following order:
  number-of-subs insert-before-current (t or nil) buffer-is-empty
  msecs-min msecs-max msecs-avail msecs-per-sub msecs-between
  insert-subtitle-func"
  (let* ((number-of-subs (cond ((not arg) 1)         ;; M-i
                               ((integerp arg) arg)  ;; C-u N M-i  /  C-u - N M-i
                               ;; C-u [C-u ...] M-i  /  C-u - [C-u ...] M-i
                               ((consp arg) (* (truncate (log (abs (car arg)) 4)) ;; ([-]64) -> 3
                                               (/ (car arg) (abs (car arg)))))    ;; Restore sign
                               (t 1)))  ;; C-u - M-i
         (insert-before-current (or (< number-of-subs 0)  ;; C-u - N M-i
                                    (eq arg '-)           ;; C-u - M-i
                                    (consp arg)))         ;; C-u [C-u ...] M-i
         ;; Ensure number-of-subs is positive, now that we figured out `insert-before-current'
         (number-of-subs (abs number-of-subs))
         (buffer-is-empty (if (subed-subtitle-id) nil t))
         ;; Find out how much time there is available
         (msecs-min (save-excursion (if insert-before-current
                                        (if (subed-backward-subtitle-id)
                                          (subed-subtitle-msecs-stop) 0)
                                      (subed-subtitle-msecs-stop))))
         (msecs-max (save-excursion (if insert-before-current
                                        (subed-subtitle-msecs-start)
                                      (when (subed-forward-subtitle-id)
                                        (subed-subtitle-msecs-start)))))
         (msecs-avail (cond ((and msecs-min msecs-max) (- msecs-max msecs-min))
                            (msecs-max msecs-max)
                            (t nil)))  ;; Unlimited
         (msecs-per-sub (if msecs-avail
                            (min subed-default-subtitle-length
                                 (max 0 (/ (- msecs-avail (* (1+ number-of-subs) subed-subtitle-spacing))
                                           number-of-subs)))
                          subed-default-subtitle-length))
         (msecs-between (if (or (not msecs-avail)
                                (>= msecs-avail (* (1+ number-of-subs) subed-subtitle-spacing)))
                            subed-subtitle-spacing
                          0))
         (insert-subtitle-func (if insert-before-current
                                   #'subed-prepend-subtitle
                                 #'subed-append-subtitle)))
    (subed-debug "Inserting %s subtitle(s) %s the current in %sempty buffer"
                 number-of-subs
                 (if insert-before-current "before" "after")
                 (if buffer-is-empty "" "non-"))
    (subed-debug "  Available time: min=%S max=%S avail=%S sublen=%S/%S"
                 msecs-min msecs-max msecs-avail msecs-per-sub subed-default-subtitle-length)
    (list number-of-subs insert-before-current buffer-is-empty
          msecs-min msecs-max msecs-avail msecs-per-sub msecs-between
          insert-subtitle-func)))

(subed-define-generic-function insert-subtitle (&optional arg)
  "Insert subtitle(s) evenly spaced.

The inserted subtitles are `subed-default-subtitle-length'
milliseconds long.

Subtitles are spread out evenly over the available time.

ARG, usually provided by `universal-argument', is used in the
following manner:
          \\[subed-insert-subtitle]   Insert 1 subtitle after the current subtitle
      \\[universal-argument] \\[subed-insert-subtitle]   Insert 1 subtitle before the current subtitle
    \\[universal-argument] 5 \\[subed-insert-subtitle]   Insert 5 subtitles after the current subtitle
  \\[universal-argument] - 5 \\[subed-insert-subtitle]   Insert 5 subtitles before the current subtitle
  \\[universal-argument] \\[universal-argument] \\[subed-insert-subtitle]   Insert 2 subtitles before the current subtitle"
  (interactive "P")
  (atomic-change-group
    (cl-multiple-value-bind (number-of-subs insert-before-current _buffer-is-empty
                             msecs-min msecs-max msecs-avail msecs-per-sub msecs-between
                             insert-subtitle-func)
                            (subed--insert-subtitle-info arg)
      (dotimes (i number-of-subs)
        ;; Value constellations:
        ;;     empty buffer, append           : min=0       max=nil     avail=nil
        ;;     empty buffer, prepend          : min=0       max=nil     avail=nil
        ;; non-empty buffer, prepend, betwixt : min=non-nil max=non-nil avail=non-nil
        ;; non-empty buffer, append,  betwixt : min=non-nil max=non-nil avail=non-nil
        ;; non-empty buffer, prepend to first : min=0       max=non-nil avail=non-nil
        ;; non-empty buffer, append  to last  : min=non-nil max=nil     avail=nil
        (let* ((multiplier (if insert-before-current
                               (- number-of-subs i)
                             (1+ i)))
               (msecs-start (if msecs-avail
                                ;; Inserting anywhere before the last subtitle
                                (+ msecs-min
                                   (if (< msecs-per-sub subed-default-subtitle-length)
                                       ;; Use all available space between subtitles
                                       (+ msecs-between
                                          (* (1- multiplier) (+ msecs-between msecs-per-sub)))
                                     ;; Leave extra space between subtitles
                                     (* multiplier (/ msecs-avail (1+ number-of-subs)))))
                              (if (and msecs-min (not msecs-max))
                                  ;; Appending to last subtitle
                                  (+ msecs-min
                                     ;; If buffer is empty, start first subtitle at 0
                                     (if (> msecs-min 0) msecs-between 0)
                                     (* (1- multiplier) (+ msecs-per-sub msecs-between)))
                                ;; Appending in empty buffer
                                (* i (+ msecs-per-sub msecs-between)))))
               (msecs-stop (+ msecs-start msecs-per-sub)))
          (subed-debug "  Inserting new subtitle at %S - %S" msecs-start msecs-stop)
          (funcall insert-subtitle-func nil msecs-start msecs-stop nil)))
      (unless insert-before-current
        (dotimes (_ (1- number-of-subs))
          (subed-backward-subtitle-text)))))
  (point))

(subed-define-generic-function insert-subtitle-adjacent (&optional arg)
  "Insert subtitle(s) close to each other.

The inserted subtitles are `subed-default-subtitle-length'
milliseconds long.

Subtitles are inserted `subed-subtitle-spacing' milliseconds
before or after the current subtitle.  When inserting multiple
subtitles, the gap between them is also `subed-subtitle-spacing'
milliseconds long.

ARG, usually provided by `universal-argument', is used in the
following manner:
          \\[subed-insert-subtitle]   Insert 1 subtitle after the current subtitle
      \\[universal-argument] \\[subed-insert-subtitle]   Insert 1 subtitle before the current subtitle
    \\[universal-argument] 5 \\[subed-insert-subtitle]   Insert 5 subtitles after the current subtitle
  \\[universal-argument] - 5 \\[subed-insert-subtitle]   Insert 5 subtitles before the current subtitle
  \\[universal-argument] \\[universal-argument] \\[subed-insert-subtitle]   Insert 2 subtitles before the current subtitle"
  (interactive "P")
  (atomic-change-group
    (cl-multiple-value-bind (number-of-subs insert-before-current buffer-is-empty
                             msecs-min msecs-max _msecs-avail msecs-per-sub msecs-between
                             insert-subtitle-func)
                            (subed--insert-subtitle-info arg)
      (dotimes (i number-of-subs)
        ;; Value constellations:
        ;;     empty buffer, append           : min=0       max=nil     avail=nil
        ;;     empty buffer, prepend          : min=0       max=nil     avail=nil
        ;; non-empty buffer, prepend, betwixt : min=non-nil max=non-nil avail=non-nil
        ;; non-empty buffer, append,  betwixt : min=non-nil max=non-nil avail=non-nil
        ;; non-empty buffer, prepend to first : min=0       max=non-nil avail=non-nil
        ;; non-empty buffer, append  to last  : min=non-nil max=nil     avail=nil
        (let* ((multiplier (if insert-before-current
                               (- number-of-subs i 1)
                             i))
               (msecs-start (if buffer-is-empty
                                (* multiplier (+ msecs-between msecs-per-sub))
                              (if insert-before-current
                                  (- msecs-max (* (1+ i) (+ msecs-between msecs-per-sub)))
                                (+ msecs-min
                                   msecs-between
                                   (* i (+ msecs-per-sub msecs-between))))))
               (msecs-stop (+ msecs-start msecs-per-sub)))
          (subed-debug "  Inserting new subtitle at %S - %S" msecs-start msecs-stop)
          (funcall insert-subtitle-func nil msecs-start msecs-stop nil)))
      (unless insert-before-current
        (dotimes (_ (1- number-of-subs))
          (subed-backward-subtitle-text)))))
  (point))

(defvar subed-split-subtitle-timestamp-functions
  '(subed-split-subtitle-based-on-mpv-playback-position
    subed-split-subtitle-based-on-point-ratio)
  "Functions to call to get the timestamp to split at.
Functions will be called with one argument.
They should return a timestamp in milliseconds.
The first non-nil value will be used for the split.
Functions should preserve the point.")

(defun subed-split-subtitle-based-on-mpv-playback-position ()
  "Return a timestamp based on the current MPV position.
Do this only if the position is within the start and end of the
current subtitle."
  (when (and
         subed-mpv-playback-position
         (>= subed-mpv-playback-position (subed-subtitle-msecs-start))
         (<= subed-mpv-playback-position (subed-subtitle-msecs-stop)))
    subed-mpv-playback-position))

(defun subed-split-subtitle-based-on-point-ratio ()
  "Return a timestamp based on the relative position in the subtitle text."
  (let* ((pos (point))
         (text-beg (or (save-excursion (subed-jump-to-subtitle-text)) pos))
         (text-end (or (save-excursion (subed-jump-to-subtitle-end)) pos)))
    ;; Ensure point is on subtitle text
    (when (and (>= pos text-beg)
               (<= pos text-end))
      (let* ((orig-start (subed-subtitle-msecs-start))
             (orig-end (subed-subtitle-msecs-stop))
             (text-fraction (if (= text-beg text-end) 1
                              (/ (* 1.0 (- (point) text-beg)) (- text-end text-beg))))
             (time-fraction (floor (* text-fraction (- orig-end orig-start)))))
        (+ orig-start time-fraction)))))

(subed-define-generic-function split-subtitle (&optional offset)
  "Split current subtitle at point.

The subtitle text after point is moved to a new subtitle that is
inserted after the current subtitle.

If OFFSET is a number, it is used as the offset in milliseconds
from the starting timestamp if positive or from the ending
timestamp if negative.  If OFFSET is a timestamp, it is used
as the starting timestamp of the second subtitle.  Otherwise, if
`subed-mpv-playback-position' is within the current subtitle, it
is used as the new stop time of the current subtitle.  Otherwise,
the timestamp proportional to the point's position between start
and stop timestamp of the current subtitle is used.

If called interactively, calling it with one prefix
argument (e.g. \\[universal-argument] \\[subed-split-subtitle])
prompts for the offset in milliseconds.  Calling it with two
prefix arguments (e.g. \\[universal-argument]
\\[universal-argument] \\[subed-split-subtitle]) uses the
relative position of the point even if the media is playing in
MPV.

The newly inserted subtitle starts `subed-subtitle-spacing'
milliseconds after the current subtitle's new end timestamp.

Move to the beginning of the new subtitle's text and return the
position of the point."
  (interactive (list
                (cond
                 ((equal current-prefix-arg '(4))
                  (read-string "Offset (ms or timestamp): "))
                 ((equal current-prefix-arg '(16)) t))))
  (let ((text-beg (save-excursion (subed-jump-to-subtitle-text)))
        (text-end (save-excursion (or (subed-jump-to-subtitle-end) (point)))))
    ;; Ensure point is on subtitle text
    (unless (and text-beg (>= (point) text-beg))
      (subed-jump-to-subtitle-text))
    (let* ((orig-end (subed-subtitle-msecs-stop))
           (offset-ms
            (cond
             ((null offset) nil)
             ((numberp offset) offset)
             ((and (stringp offset) (string-match "^-?[0-9]*\\.[0-9]*" offset))
              (string-to-number offset))))
           (split-timestamp
            (cond
             (offset-ms
              (if (> offset-ms 0)
                  (+ (subed-subtitle-msecs-start) offset-ms)
                (+ orig-end offset-ms)))
             ((stringp offset)
              (- (subed-timestamp-to-msecs offset)
                 subed-subtitle-spacing))
             (t (run-hook-with-args-until-success 'subed-split-subtitle-timestamp-functions))))
           (new-text (string-trim (buffer-substring (point) text-end)))
           (new-start-timestamp (and split-timestamp (+ split-timestamp subed-subtitle-spacing))))
      (if split-timestamp
          (progn
            (subed-set-subtitle-time-stop split-timestamp)
            (skip-chars-backward "\r\n")
            (delete-region (point) (progn (subed-jump-to-subtitle-end) (skip-chars-forward " \t") (point)))
            (when (looking-at "[ \t]+") (replace-match ""))
            (subed-append-subtitle nil new-start-timestamp orig-end (string-trim new-text)))
        (error "Could not determine timestamp for splitting")))
    (point)))

;;; Merging

(defun subed-merge-dwim ()
  "Merge the subtitles in the region if the region is active.
If the region is not active, merge the current subtitle with the next one."
  (interactive)
  (if (region-active-p)
      (subed-merge-region (min (point) (mark))
                          (max (point) (mark)))
    (subed-merge-with-next)))

(subed-define-generic-function merge-with-next ()
  "Merge the current subtitle with the next subtitle.
Update the end timestamp accordingly."
  (interactive))

(subed-define-generic-function merge-with-previous ()
  "Merge the current subtitle with the previous subtitle.
Update the end timestamp accordingly."
  (interactive)
  (if (subed-backward-subtitle-id)
      (subed-merge-with-next)
    (error "No previous subtitle to merge into")))

(subed-define-generic-function merge-region (beg end)
  "Merge the subtitles in the region defined by BEG and END."
  (interactive "r")
  (save-restriction
    (narrow-to-region (progn (goto-char beg) (or (subed-jump-to-subtitle-id) (point)))
                      (progn (goto-char end)
                             (if (= (point) (subed-subtitle-start-pos))
                                 (point)
                               (or (subed-jump-to-subtitle-end) (point)))))
    (goto-char beg)
    (while (save-excursion (subed-forward-subtitle-id))
      (subed-merge-with-next))))

(subed-define-generic-function merge-region-and-set-text (beg end text)
  "Merge the subtitles in the region defined by BEG and END.
Replace the subtitle text with TEXT.  If the region is not
specified, set the current subtitle's text."
  (interactive (list (when (region-active-p) (min (point) (mark)))
                     (when (region-active-p) (max (point) (mark)))
                     (read-string "Text: ")))
  (when (and beg end)
    (subed-merge-region beg end))
  (subed-set-subtitle-text text))

;;; Replay time-adjusted subtitle

(defun subed-replay-adjusted-subtitle-p ()
  "Whether the player jumps to start time when start or stop time is adjusted."
  (member #'subed--replay-adjusted-subtitle subed-subtitle-time-adjusted-hook))

(defun subed-enable-replay-adjusted-subtitle (&optional quiet)
  "Automatically replay a subtitle when its start/stop time is adjusted.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (unless (subed-replay-adjusted-subtitle-p)
    (add-hook 'subed-subtitle-time-adjusted-hook #'subed--replay-adjusted-subtitle :append :local)
    (subed-debug "Enabled replaying adjusted subtitle: %s" subed-subtitle-time-adjusted-hook)
    (unless quiet
      (message "Enabled replaying adjusted subtitle"))))

(defun subed-disable-replay-adjusted-subtitle (&optional quiet)
  "Do not replay a subtitle automatically when its start/stop time is adjusted.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (when (subed-replay-adjusted-subtitle-p)
    (remove-hook 'subed-subtitle-time-adjusted-hook #'subed--replay-adjusted-subtitle :local)
    (subed-debug "Disabled replaying adjusted subtitle: %s" subed-subtitle-time-adjusted-hook)
    (unless quiet
      (message "Disabled replaying adjusted subtitle"))))

(defun subed-toggle-replay-adjusted-subtitle ()
  "Enable/disable subtitle replay when start/stop time is adjusted."
  (interactive)
  (if (subed-replay-adjusted-subtitle-p)
      (subed-disable-replay-adjusted-subtitle)
    (subed-enable-replay-adjusted-subtitle)))

(defun subed--replay-adjusted-subtitle (msecs-start)
  "Seek player to MSECS-START."
  (subed-debug "Replaying subtitle at: %s" (subed-msecs-to-timestamp msecs-start))
  (subed-mpv-jump msecs-start))

;;; Sync point-to-player

(defun subed-sync-point-to-player-p ()
  "Whether point is automatically moved to currently playing subtitle."
  (member #'subed--sync-point-to-player subed-mpv-playback-position-hook))

(defun subed-enable-sync-point-to-player (&optional quiet)
  "Automatically move point to the currently playing subtitle.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  ;; If looping over a subtitle, don't immediately enable it. Set it to be enabled after the loop ends.
  (if (subed-loop-over-current-subtitle-p)
      (setq subed--enable-point-to-player-sync-after-disabling-loop t)
    (unless (subed-sync-point-to-player-p)
      (add-hook 'subed-mpv-playback-position-hook #'subed--sync-point-to-player :append :local)
      (subed-debug "Enabled syncing point to playback position: %s" subed-mpv-playback-position-hook)
      (unless quiet
        (message "Enabled syncing point to playback position")))))

(defun subed-disable-sync-point-to-player (&optional quiet)
  "Do not move point automatically to the currently playing subtitle.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (when (subed-loop-over-current-subtitle-p)
    (setq subed--enable-point-to-player-sync-after-disabling-loop nil))
  (when (subed-sync-point-to-player-p)
    (remove-hook 'subed-mpv-playback-position-hook #'subed--sync-point-to-player :local)
    (subed-debug "Disabled syncing point to playback position: %s" subed-mpv-playback-position-hook)
    (unless quiet
      (message "Disabled syncing point to playback position"))))

(defun subed-toggle-sync-point-to-player ()
  "Enable/disable moving point to the currently playing subtitle."
  (interactive)
  (if (subed-sync-point-to-player-p)
      (subed-disable-sync-point-to-player)
    (subed-enable-sync-point-to-player)))

(defun subed--sync-point-to-player (msecs)
  "Move point to subtitle at MSECS."
  (when (and (not (use-region-p)) ;; Don't sync with active-mark in transient-mark-mode
             (subed-jump-to-subtitle-text-at-msecs msecs))
    (subed-debug "Synchronized point to playback position: %s -> #%s"
                 (subed-msecs-to-timestamp msecs) (subed-subtitle-id))
    ;; post-command-hook is not triggered because we didn't move interactively,
    ;; but there shouldn't be a difference between automatic movement and manual
    ;; movement.  E.g. the minor mode `hl-line' breaks because its post-command
    ;; function is not called.
    ;; But it's also important NOT to call our own post-command function because
    ;; that causes player-to-point syncing, which would get hairy.
    (remove-hook 'post-command-hook #'subed--post-command-handler)
    (run-hooks 'post-command-hook)
    (add-hook 'post-command-hook #'subed--post-command-handler :append :local)))

(defvar-local subed--point-sync-delay-after-motion-timer nil)
(defun subed-disable-sync-point-to-player-temporarily ()
  "Temporarily disable syncing point to player.

After `subed-point-sync-delay-after-motion' seconds point is re-synced."
  (if subed--point-sync-delay-after-motion-timer
      (when (timerp subed--point-sync-delay-after-motion-timer)
        (cancel-timer subed--point-sync-delay-after-motion-timer))
    (setq subed--point-was-synced (subed-sync-point-to-player-p)))
  (when subed--point-was-synced
    (subed-disable-sync-point-to-player :quiet))
  (when subed--point-was-synced
    (setq subed--point-sync-delay-after-motion-timer
          (run-at-time subed-point-sync-delay-after-motion nil
                       (lambda ()
                         (setq subed--point-sync-delay-after-motion-timer nil)
                         (subed-enable-sync-point-to-player :quiet))))))


;;; Sync player-to-point

(defun subed-sync-player-to-point-p ()
  "Whether playback position jumps to subtitle at point."
  (member #'subed--sync-player-to-point subed-subtitle-motion-hook))

(defun subed-enable-sync-player-to-point (&optional quiet)
  "Automatically seek player to subtitle at point.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (unless (subed-sync-player-to-point-p)
    (subed--sync-player-to-point)
    (add-hook 'subed-subtitle-motion-hook #'subed--sync-player-to-point :append :local)
    (subed-debug "Enabled syncing playback position to point: %s" subed-subtitle-motion-hook)
    (unless quiet
      (message "Enabled syncing playback position to point"))))

(defun subed-disable-sync-player-to-point (&optional quiet)
  "Do not automatically seek player to subtitle at point.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (when (subed-sync-player-to-point-p)
    (remove-hook 'subed-subtitle-motion-hook #'subed--sync-player-to-point :local)
    (subed-debug "Disabled syncing playback position to point: %s" subed-subtitle-motion-hook)
    (unless quiet
      (message "Disabled syncing playback position to point"))))

(defun subed-toggle-sync-player-to-point ()
  "Enable or disable automatically seeking player to subtitle at point."
  (interactive)
  (if (subed-sync-player-to-point-p)
      (subed-disable-sync-player-to-point)
    (subed-enable-sync-player-to-point)))

(defun subed--sync-player-to-point ()
  "Seek player to currently focused subtitle."
  (subed-debug "Seeking player to subtitle at point %s" (point))
  (let ((cur-sub-start (subed-subtitle-msecs-start))
        (cur-sub-stop (subed-subtitle-msecs-stop)))
    (when (and subed-mpv-playback-position cur-sub-start cur-sub-stop
               (or (< subed-mpv-playback-position cur-sub-start)
                   (> subed-mpv-playback-position cur-sub-stop)))
      (subed-mpv-jump cur-sub-start)
      (subed-debug "Synchronized playback position to point: #%s -> %s"
                   (subed-subtitle-id) cur-sub-start))))


;;; Loop over single subtitle

(defun subed-loop-over-current-subtitle-p ()
  "Whether the player is looping over the current subtitle."
  (or subed--subtitle-loop-start subed--subtitle-loop-stop))

(defun subed-enable-loop-over-current-subtitle (&optional quiet)
  "Enable looping over the current subtitle in the player.

If enabled, point-to-player synchronization is disabled and
re-enabled again when `subed-disable-loop-over-current-subtitle'
is called.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (unless (subed-loop-over-current-subtitle-p)
    (subed--set-subtitle-loop (subed-subtitle-id))
    (add-hook 'subed-mpv-playback-position-hook #'subed--ensure-subtitle-loop :append :local)
    (add-hook 'subed-subtitle-motion-hook #'subed--set-subtitle-loop :append :local)
    (subed-debug "Enabling loop: %s - %s" subed--subtitle-loop-start subed--subtitle-loop-stop)
    (when (subed-sync-point-to-player-p)
      (subed-disable-sync-point-to-player quiet)
      (setq subed--enable-point-to-player-sync-after-disabling-loop t))
    (unless quiet
      (message "Enabled looping over current subtitle"))))

(defun subed-disable-loop-over-current-subtitle (&optional quiet)
  "Disable looping over the current subtitle in the player.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (when (subed-loop-over-current-subtitle-p)
    (remove-hook 'subed-mpv-playback-position-hook #'subed--ensure-subtitle-loop :local)
    (remove-hook 'subed-subtitle-motion-hook #'subed--set-subtitle-loop :local)
    (setq subed--subtitle-loop-start nil
          subed--subtitle-loop-stop nil)
    (subed-debug "Disabling loop: %s - %s" subed--subtitle-loop-start subed--subtitle-loop-stop)
    (when subed--enable-point-to-player-sync-after-disabling-loop
      (subed-enable-sync-point-to-player)
      (setq subed--enable-point-to-player-sync-after-disabling-loop nil))
    (unless quiet
      (message "Disabled looping over current subtitle"))))

(defun subed-toggle-loop-over-current-subtitle (&optional quiet)
  "Enable or disable looping over the current subtitle in the player.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (if (subed-loop-over-current-subtitle-p)
      (subed-disable-loop-over-current-subtitle quiet)
    (subed-enable-loop-over-current-subtitle quiet)))

(defun subed--set-subtitle-loop (&optional sub-id)
  "Set loop positions to start/stop time of SUB-ID or current subtitle."
  (let ((msecs-start (subed-subtitle-msecs-start sub-id))
        (msecs-stop (subed-subtitle-msecs-stop sub-id)))
    (when (and msecs-start msecs-stop)
      (setq subed--subtitle-loop-start (- msecs-start (* subed-loop-seconds-before 1000))
            subed--subtitle-loop-stop (+ msecs-stop (* subed-loop-seconds-after 1000)))
      (subed-debug "Set loop: %s - %s"
                   (subed-msecs-to-timestamp subed--subtitle-loop-start)
                   (subed-msecs-to-timestamp subed--subtitle-loop-stop))
      (message "Looping over %s - %s"
               (subed-msecs-to-timestamp subed--subtitle-loop-start)
               (subed-msecs-to-timestamp subed--subtitle-loop-stop)))))

(defun subed--ensure-subtitle-loop (cur-msecs)
  "Jump to current subtitle start time if CUR-MSECS is after stop time."
  (when (and subed--subtitle-loop-start subed--subtitle-loop-stop
             subed-mpv-is-playing)
    (when (or (< cur-msecs subed--subtitle-loop-start)
              (> cur-msecs subed--subtitle-loop-stop))
      (subed-debug "%s -> Looping over %s - %s"
                   (subed-msecs-to-timestamp cur-msecs)
                   (subed-msecs-to-timestamp subed--subtitle-loop-start)
                   (subed-msecs-to-timestamp subed--subtitle-loop-stop))
      (subed-mpv-jump subed--subtitle-loop-start))))


;;; Pause player while the user is editing

(defun subed-pause-while-typing-p ()
  "Whether player is automatically paused or slowed down during editing.

See `subed-playback-speed-while-typing' and
`subed-playback-speed-while-not-typing'."
  (member #'subed--pause-while-typing post-command-hook))

(defun subed-enable-pause-while-typing (&optional quiet)
  "Pause player while the user is editing a subtitle.

After `subed-unpause-after-typing-delay' seconds, playback is
resumed automatically unless the player was paused already.

If QUIET is non-nil, do not display a message in the minibuffer."
  (unless (subed-pause-while-typing-p)
    (add-hook 'post-command-hook #'subed--pause-while-typing :append :local)
    (unless quiet
      (subed-debug "%S" subed-playback-speed-while-typing)
      (if (<= subed-playback-speed-while-typing 0)
          (message "Playback will pause while subtitle texts are edited")
        (message "Playback will slow down by %s while subtitle texts are edited"
                 subed-playback-speed-while-typing)))))

(defun subed-disable-pause-while-typing (&optional quiet)
  "Do not automatically pause player while the user is editing the buffer.

If QUIET is non-nil, do not display a message in the minibuffer."
  (when (subed-pause-while-typing-p)
    (remove-hook 'post-command-hook #'subed--pause-while-typing :local)
    (unless quiet
      (message "Playback speed will not change while subtitle texts are edited"))))

(defun subed-toggle-pause-while-typing ()
  "Enable or disable auto-pausing while the user is editing the buffer."
  (interactive)
  (if (subed-pause-while-typing-p)
      (subed-disable-pause-while-typing)
    (subed-enable-pause-while-typing)))

(defvar-local subed--unpause-after-typing-timer nil)
(defun subed--pause-while-typing (&rest _args)
  "Pause or slow down playback for `subed-unpause-after-typing-delay' seconds.

This function is meant to be an item in `after-change-functions'
and therefore gets ARGS, which is ignored."
  (when subed--unpause-after-typing-timer
    (cancel-timer subed--unpause-after-typing-timer))
  (when (or subed-mpv-is-playing subed--player-is-auto-paused)
    (if (<= subed-playback-speed-while-typing 0)
        ;; Pause playback
        (progn
          (subed-mpv-pause)
          (setq subed--player-is-auto-paused t)
          (setq subed--unpause-after-typing-timer
                (run-at-time subed-unpause-after-typing-delay nil
                             (lambda ()
                               (setq subed--player-is-auto-paused nil)
                               (subed-mpv-unpause)))))
      ;; Slow down playback
      (progn
        (subed-mpv-playback-speed subed-playback-speed-while-typing)
        (setq subed--player-is-auto-paused t)
        (setq subed--unpause-after-typing-timer
              (run-at-time subed-unpause-after-typing-delay nil
                           (lambda ()
                             (setq subed--player-is-auto-paused nil)
                             (subed-mpv-playback-speed subed-playback-speed-while-not-typing))))))))

(defvar subed-media-file-functions '(subed-media-file-from-cache subed-guess-media-file)
  "Functions to use for getting the media file.")

(defun subed-media-file ()
  "Return the current media file.
Uses the functions listed in `subed-media-file-functions'."
  (run-hook-with-args-until-success 'subed-media-file-functions))

(defun subed-media-file-from-cache ()
  "Return the media file from the variable."
  subed-mpv-media-file)

(defun subed-guess-media-file (&optional extensions)
  "Find media file with same base name as the opened file in the buffer.

The optional EXTENSIONS argument can be a list of extensions to
look for. If not, check against the extensions in
`subed-video-extensions' and `subed-audio-extensions'.  The file
extension of the function `buffer-file-name' is replaced with each item in
the extension list and the first existing file is returned.

Language codes are also handled; e.g. \"foo.en.srt\" or
\"foo.estonian.srt\" -> \"foo.{mkv,mp4,...}\" (this actually
simply removes the extension from the extension-stripped file
name).

Return nil if function `buffer-file-name' returns nil."
  (when (buffer-file-name)
    (catch 'found-file
      (let* ((file-base (file-name-sans-extension (buffer-file-name)))
             (file-stem (file-name-sans-extension file-base)))
        (dolist (extension
                 (or extensions (append subed-video-extensions subed-audio-extensions)))
          (let ((file-base-media (format "%s.%s" file-base extension))
                (file-stem-media (format "%s.%s" file-stem extension)))
            (when (file-exists-p file-base-media)
              (throw 'found-file file-base-media))
            (when (file-exists-p file-stem-media)
              (throw 'found-file file-stem-media))))))))
(define-obsolete-function-alias 'subed-guess-video-file 'subed-guess-media-file "1.0.20")

;;; Inserting HTML-like tags

(defvar subed--html-tag-history nil
  "History of HTML-like tags in subtitles.")
(defvar subed--html-attr-history nil
  "History of HTML-like attributes in subtitles.")

(defun subed-insert-html-tag (begin end tag &optional attributes)
  "Insert a pair of HTML-like tags around the region using TAG.
BEGIN and END specify the start of the region.
If region is not active, insert a pair of tags and put the point
between them.  If called with a prefix argument, also ask for
ATTRIBUTE(s)."
  (interactive (let* ((region-p (use-region-p))
                      (begin (if region-p (region-beginning) (point)))
                      (end (if region-p (region-end) (point)))
                      (tag (read-string "Tag: " nil 'subed--html-tag-history))
                      (attributes (when current-prefix-arg
                                    (read-string "Attribute(s): " nil 'subed--html-attr-history))))
                 (list begin end tag attributes)))
  (save-excursion
    (push (point) buffer-undo-list)
    (goto-char end)
    (insert "</" tag ">")
    (goto-char begin)
    (insert-before-markers "<" tag)
    (when attributes (insert-before-markers " " attributes))
    (insert-before-markers ">")))

(defun subed-insert-html-tag-italic (begin end)
  "Insert a pair of <i> tags at point or around the region.
The region is defined by BEGIN and END."
  (interactive (let* ((region-p (use-region-p))
                      (begin (if region-p (region-beginning) (point)))
                      (end (if region-p (region-end) (point))))
                 (list begin end)))
  (subed-insert-html-tag begin end "i"))

(defun subed-insert-html-tag-bold (begin end)
  "Insert a pair of <b> tags at point or around the region.
The region is defined by BEGIN and END."
  (interactive (let* ((region-p (use-region-p))
                      (begin (if region-p (region-beginning) (point)))
                      (end (if region-p (region-end) (point))))
                 (list begin end)))
  (subed-insert-html-tag begin end "b"))

;;; Characters per second computation

(defvar-local subed--cps-overlay nil)

(defun subed-show-cps-p ()
  "Whether CPS is shown for the current subtitle."
  (member #'subed--update-cps-overlay post-command-hook))

(defun subed-enable-show-cps (&optional quiet)
  "Enable showing CPS next to the subtitle heading.
If QUIET is nil, show a message."
  (interactive "p")
  ;; FIXME: Consider displaying CPS on all cues (via jit-lock) rather than the current one?
  (add-hook 'post-command-hook #'subed--update-cps-overlay nil t)
  (add-hook 'subed-subtitle-motion-hook #'subed--move-cps-overlay-to-current-subtitle nil t)
  (add-hook 'after-save-hook #'subed--move-cps-overlay-to-current-subtitle nil t)
  (unless quiet
    (message "Enabled showing characters per second")))

(defun subed-disable-show-cps (&optional quiet)
  "Disable showing CPS next to the subtitle heading.
If QUIET is nil, show a message."
  (interactive)
  (remove-hook 'post-command-hook #'subed--update-cps-overlay t)
  (remove-hook 'subed-subtitle-motion-hook #'subed--move-cps-overlay-to-current-subtitle t)
  (remove-hook 'after-save-hook #'subed--move-cps-overlay-to-current-subtitle t)
  (when subed--cps-overlay
    (remove-overlays (point-min) (point-max) 'subed 'cps)
    (setq subed--cps-overlay nil))
  (unless quiet
    (message "Disabled showing characters per second")))

(defun subed-toggle-show-cps ()
  "Enable or disable showing CPS next to the subtitle heading."
  (interactive)
  (if (subed-show-cps-p)
      (subed-disable-show-cps)
    (subed-enable-show-cps)))

(defvar subed-transform-for-cps #'subed--strip-tags)

(defun subed--strip-tags (string)
  "Strip HTML-like tags from STRING."
  (with-temp-buffer
    (insert string)
    (goto-char 1)
    (while (re-search-forward "</?[^>]+>" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (buffer-string)))

(defun subed-calculate-cps (&optional print-message)
  "Calculate characters per second of the current subtitle.
if PRINT-MESSAGE is non-nil, display a message."
  (interactive "p")
  (let* ((msecs-start (ignore-errors (subed-subtitle-msecs-start)))
         (msecs-stop (ignore-errors (subed-subtitle-msecs-stop)))
         (text (if (fboundp subed-transform-for-cps)
                   (funcall subed-transform-for-cps (subed-subtitle-text))
                 (subed-subtitle-text)))
         (length (length text))
         (cps (when (and (numberp msecs-stop)
                         (numberp msecs-start))
                (/ length 0.001 (- msecs-stop msecs-start)))))
    (if (and print-message cps)
        (message "%.1f characters per second" cps)
      cps)))

(defun subed--move-cps-overlay-to-current-subtitle ()
  "Move the CPS overlay to the current subtitle."
  (unless subed--batch-editing
    (let* ((begin (save-excursion
                    (subed-jump-to-subtitle-time-start)
                    (point)))
           (end (save-excursion
                  (goto-char begin)
                  (line-end-position))))
      (if subed--cps-overlay
          (move-overlay subed--cps-overlay begin end (current-buffer))
        (setq subed--cps-overlay (make-overlay begin end))
        (overlay-put subed--cps-overlay 'subed 'cps))
      (subed--update-cps-overlay))))

(defun subed--update-cps-overlay (&rest _rest)
  "Update the CPS overlay."
  (when subed--cps-overlay
    (let ((cps (subed-calculate-cps)))
      (when cps
        (overlay-put
         subed--cps-overlay
         'after-string
         (propertize (format " %.1f CPS" cps) 'face 'shadow 'display '(height 0.9)))))))

(defun subed-wpm (&optional subtitles)
  "Display words per minute.
Use SUBTITLES if specified."
  (interactive)
  (setq subtitles (or subtitles (subed-subtitle-list)))
  (let (word-count
        (minutes
         (/ (apply '+ (mapcar (lambda (o) (- (elt o 2) (elt o 1))) subtitles))
            60000.0)))
    (with-temp-buffer
      (insert
       (mapconcat (lambda (o)
                    (replace-regexp-in-string "</?[^>]+>" "" (elt o 3)))
                  subtitles
                  " "))
      (setq word-count (count-words (point-min) (point-max))))
    (if (called-interactively-p 'any)
        (message
         "%d wpm (%d words / %.1f minutes)"
         (/ (* 1.0 word-count) minutes)
         word-count
         minutes)
      (list (/ (* 1.0 word-count) minutes)
            word-count minutes))))

;;; Trimming overlaps

(defun subed--identify-overlaps ()
  "Return a list of IDs for subtitles that overlap the next one.
This observes `subed-subtitle-spacing'."
  (let (overlap-ids next-sub-start-time)
    (save-excursion
      (subed-for-each-subtitle (point-min) (point-max) t
        (when (and next-sub-start-time
                   (> (+ (subed-subtitle-msecs-stop)
                         subed-subtitle-spacing)
                      next-sub-start-time))
          (push (subed-subtitle-id) overlap-ids))
        (setq next-sub-start-time (subed-subtitle-msecs-start)))
      overlap-ids)))

;; NB: runs in a hook, so this version cannot send prefix arg to
;; subed-trim-overlaps. Doesn't actually use the whole list of
;; overlaps, so it may be more efficient to just find the first overlap.
(defun subed-trim-overlap-check ()
  "Test all subtitles for overlapping timecodes.

Creates a list of the ids of overlapping subtitles, moves point
to the end time of the first one, and prompts to trim
them. Designed to be run in `subed-mode-hook'."
  (interactive)
  (let ((overlap-ids (subed--identify-overlaps)))
    (when overlap-ids
      (subed-jump-to-subtitle-time-stop (car overlap-ids))
      (when (yes-or-no-p "Overlapping subtitles found. Trim them? ")
        (subed-trim-overlaps)))))

(defun subed-trim-overlaps (&optional msecs)
  "Adjust all overlapping times in current file.

Change the stop times to start MSECS before the next subtitle, or
`subed-subtitle-spacing' if not specified.  If
`subed-trim-overlap-use-start' is non-nil, change
the start times instead."
  (interactive "P")
  (subed-batch-edit
   (save-excursion
     (subed-for-each-subtitle (point-min) (point-max) nil
       (if subed-trim-overlap-use-start
           (subed-trim-overlap-next-start msecs)
         (subed-trim-overlap-stop msecs))))))

(defun subed-trim-overlap-maybe-sanitize ()
  "Trim overlaps if specified by `subed-trim-overlap-on-save'."
  (when subed-trim-overlap-on-save
    (subed-trim-overlaps)))

(defun subed-trim-overlap-maybe-check ()
  "Check overlaps if specified by `subed-trim-overlap-check-on-save'."
  (when subed-trim-overlap-check-on-save
    (subed-trim-overlap-check)))

(defun subed-trim-overlap-stop (&optional msecs ignore-negative-duration)
  "Trim the current subtitle so that it stops before the next one.

Trim the end time of the current subtitle to MSECS or
`subed-subtitle-spacing' less than the start time of the next
subtitle, if needed.  Unless either IGNORE-NEGATIVE-DURATION or
`subed-enforce-time-boundaries' are non-nil, adjust MSECS so that
the stop time isn't smaller than the start time.

Return the new stop time."
  (interactive "P")
  (let ((next-sub-start-time (save-excursion
                               (and
                                (subed-forward-subtitle-time-start)
                                (subed-subtitle-msecs-start))))
        (this-sub-stop-time (subed-subtitle-msecs-stop)))
    (when (and next-sub-start-time this-sub-stop-time
               (> this-sub-stop-time
                  (- next-sub-start-time
                     (if (numberp msecs) msecs subed-subtitle-spacing))))
      (subed-set-subtitle-time-stop
       (- next-sub-start-time
          (if (numberp msecs) msecs subed-subtitle-spacing))
       nil
       ignore-negative-duration
       t))))

(defun subed-trim-overlap-next-start (&optional msecs ignore-negative-duration)
  "Trim the next subtitle to start after the current one.

Trim the start time of the next subtitle to MSECS or
`subed-subtitle-spacing' greater than the end time of the current
subtitle.  Unless either IGNORE-NEGATIVE-DURATION or
`subed-enforce-time-boundaries' are non-nil, adjust MSECS so that
the stop time isn't smaller than the start time.

Return the new start time."
  (interactive "P")
  (save-excursion
    (let ((this-sub-stop-time (subed-subtitle-msecs-stop))
          (next-sub-start-time (and
                                (subed-forward-subtitle-time-start)
                                (subed-subtitle-msecs-start))))
      (when (and this-sub-stop-time
                 next-sub-start-time
                 (or msecs subed-subtitle-spacing)
                 (< next-sub-start-time
                    (+ this-sub-stop-time
                       (if (numberp msecs) msecs subed-subtitle-spacing))))
        (subed-set-subtitle-time-start
         (+ this-sub-stop-time (or msecs subed-subtitle-spacing))
         nil
         ignore-negative-duration
         t)))))

;;; Sorting and sanitizing

(defun subed-prepare-to-save ()
  "Sanitize and validate this buffer."
  (interactive)
  (atomic-change-group
    (subed-sanitize)
    (subed-validate)))

(defun subed--sorted-p (&optional list)
  "Return non-nil if LIST is sorted by start time.
If LIST is nil, use the subtitles in the current buffer."
  (let ((subtitles (or list (subed-subtitle-list)))
        (sorted t))
    (while (cdr subtitles)
      (if (and
           (numberp (elt (car subtitles) 1))
           (numberp (elt (cadr subtitles) 1))
           (> (elt (car subtitles) 1)
              (elt (cadr subtitles) 1))) ; starts later than the next one
          (setq sorted nil
                subtitles nil)
        (setq subtitles (cdr subtitles))))
    sorted))

(subed-define-generic-function sort ()
  "Sort subtitles."
  (interactive)
  (subed-sanitize-format)
  (subed-validate-format)
  (unless (subed--sorted-p)
    (subed-batch-edit
      (atomic-change-group
        (subed-save-excursion
         (goto-char (point-min))
         (unless (subed-jump-to-subtitle-id)
           (subed-forward-subtitle-id))
         (sort-subr nil
                    ;; nextrecfun (move to next record/subtitle or to end-of-buffer
                    ;; if there are no more records)
                    (lambda () (unless (subed-forward-subtitle-id)
                                 (goto-char (point-max))))
                    ;; endrecfun (move to end of current record/subtitle)
                    #'subed-jump-to-subtitle-end
                    ;; startkeyfun (return sort value of current record/subtitle)
                    #'subed-subtitle-msecs-start))
        (subed-regenerate-ids)
        (run-hooks 'subed-subtitles-sorted-hook)))))

;;; Conversion

(subed-define-generic-function auto-insert ()
  "Add header text for the current format."
  (interactive)
  nil)

(defun subed-create-file (filename subtitles &optional ok-if-exists init-func)
  "Create FILENAME and prepopulate it with SUBTITLES.
If OK-IF-EXISTS is non-nil, overwrite existing files.
If INIT-FUNC is non-nil, call that function to initialize."
  (when (and (file-exists-p filename) (not ok-if-exists))
    (error "File %s already exists" filename))
  (let ((subed-auto-play-media nil))
    (with-temp-file filename
      (subed-guess-format filename)
      (if init-func (funcall init-func))
      (subed-auto-insert)
      (subed-append-subtitle-list subtitles)
      (subed-regenerate-ids))))

(defun subed-convert (format &optional include-comments)
  "Create a buffer with the current subtitles converted to FORMAT.
You may need to add some extra information to the buffer.  If
INCLUDE-COMMENTS is non-nil or `subed-convert' is called with a
prefix argument, include comments in TXT output."
  (interactive (list (completing-read "To format: " '("VTT" "SRT" "ASS" "TSV" "TXT"))
                     current-prefix-arg))
  (let* ((subtitles
          (mapcar (lambda (sub) (cons nil (cdr sub))) ; remove ID
                  (subed-subtitle-list)))
         (new-filename (concat (file-name-base (or (buffer-file-name) (buffer-name))) "."
                               (downcase format)))
         (mode-func (pcase format
                      ("VTT" (require 'subed-vtt) 'subed-vtt-mode)
                      ("SRT" (require 'subed-srt) 'subed-srt-mode)
                      ("ASS" (require 'subed-ass) 'subed-ass-mode)
                      ("TSV" (require 'subed-tsv) 'subed-tsv-mode))))
    (if (buffer-file-name)
        ;; Create a new file
        (when (or (not (file-exists-p new-filename))
                  (yes-or-no-p (format "%s exists. Overwrite? " new-filename)))
          (if (string= format "TXT")
              (progn
                (with-temp-file new-filename
                  (insert (subed-subtitle-list-text subtitles include-comments)))
                (find-file new-filename))
            (subed-create-file new-filename subtitles t mode-func))
          (current-buffer))
      ;; Create a temporary buffer
      (switch-to-buffer (get-buffer-create new-filename))
      (erase-buffer)
      (when (functionp mode-func) (funcall mode-func) (subed-auto-insert))
      (if (string= format "TXT")
          (insert (subed-subtitle-list-text subtitles include-comments))
        (mapc (lambda (sub) (apply #'subed-append-subtitle nil (cdr sub)))
              subtitles))
      (current-buffer))))

;;; Wdiff
;;;###autoload
(defvar subed-wdiff-executable "wdiff" "Command for word-based diffs.")

(defun subed-wdiff-subtitle-text-with-file (script-file)
	"Use wdiff to compare the captions with SCRIPT-FILE by word.
The wdiff program must be installed.  Set
`subed-wdiff-executable' if needed."
	(interactive (list (read-file-name "Script: ")))
	(let ((subtitle-text (subed-subtitle-list-text (subed-subtitle-list)))
				(subtitle-text-filename (make-temp-file "subed-wdiff-subtitles" nil ".txt"))
				(one-line-script-filename (make-temp-file "subed-wdiff-script" nil ".txt"))
				result)
		(with-temp-file subtitle-text-filename
			(insert (replace-regexp-in-string
			         "[ \n]+" " "
			         subtitle-text)))
		(with-temp-file one-line-script-filename
			(if (member (file-name-extension script-file) '("vtt" "srt" "tsv" "ass"))
					(insert (mapconcat (lambda (o) (concat (elt o 3) " ")) (subed-parse-file script-file)))
				(insert-file-contents script-file))
			(goto-char (point-min))
			(while (re-search-forward "[ \n]+" nil t)
				(replace-match " ")))
		(setq result
					(shell-command-to-string (format "wdiff %s %s"
																					 subtitle-text-filename
																					 one-line-script-filename)))
		(delete-file subtitle-text-filename)
		(delete-file one-line-script-filename)
		(with-current-buffer (get-buffer-create "*wdiff*")
			(erase-buffer)
			(insert result)
			(goto-char (point-min))
			(while (re-search-forward "\\(\\[-\\|{\\+\\)\\(.*?\\)\\(-\\]\\|\\+}\\)" nil t)
				(add-text-properties (match-beginning 0) (match-end 0)
														 (list 'face (if (string= (match-string 1) "[-")
																						 'diff-removed
																					 'diff-added))))
			(display-buffer (current-buffer)))
		result))

;;; Misc

(defun subed-sum-time (&optional beg end)
  "Display the total time of the subtitles.
Does not yet take overlapping subtitles into account."
  (interactive (list (and (region-active-p) (min (point) (mark)))
                     (and (region-active-p) (max (point) (mark)))))
  (let ((sum
         (seq-reduce
          ;; TODO: Handle overlapping subtitles
          (lambda (prev val)
            (+ prev (- (elt val 2) (elt val 1))))
          (subed-subtitle-list beg end)
          0)))
    (when (called-interactively-p 'any)
      (message "%s" (subed-msecs-to-timestamp sum)))
    sum))

;;; Experimental retiming workflow

(defvar subed-retime-subtitles-adjustment-msecs 100
  "Number of msecs to adjust the MPV playback position.
This accounts for reaction time.")

(defun subed-retime-set-stop-and-move-forward ()
  "Set the current subtitle's stop time and the next subtitle's start time.
Move to the next subtitle.
Take into account `subed-subtitle-spacing' and
`subed-retime-subtitles-adjustment-msecs'."
  (interactive)
  (subed-set-subtitle-time-stop
   (- subed-mpv-playback-position subed-subtitle-spacing subed-retime-subtitles-adjustment-msecs))
  (subed-forward-subtitle-text)
  (subed-set-subtitle-time-start
   (- subed-mpv-playback-position subed-retime-subtitles-adjustment-msecs)))

(defun subed-retime-play-previous ()
  "Go backward one subtitle and replay."
  (interactive)
  (subed-backward-subtitle-text)
  (subed-mpv-jump-to-current-subtitle))

(defun subed-retime-play-next ()
  "Go backward one subtitle and replay."
  (interactive)
  (subed-forward-subtitle-text)
  (subed-mpv-jump-to-current-subtitle))

(defvar subed-retime-subtitles-map
  (define-keymap
    "SPC" #'subed-retime-set-stop-and-move-forward
    "<left>" #'subed-mpv-jump-to-current-subtitle
    "j" #'subed-mpv-jump-to-current-subtitle
    "<right>" #'subed-retime-play-next
    "b" #'subed-retime-play-previous
    "f" #'subed-retime-play-next
    "n" #'subed-retime-play-next
    "p" #'subed-mpv-toggle-pause)
  "Some shortcuts for subtitle retiming.")

;;;###autoload
(defun subed-retime-subtitles ()
  "Set new stop times for subtitles by pressing SPC when the next subtitle starts."
  (interactive)
  (subed-disable-loop-over-current-subtitle)
  (subed-mpv-unpause)
  (subed-mpv-jump-to-current-subtitle)
  (set-transient-map
   subed-retime-subtitles-map t
   nil
   ;; todo: support substitute-command-keys
   "SPC: set new stop, <left>: replay current, <right>: forward, (b)ack, (f)orward, (p)ause"))

;;; ffprobe

(defvar-local subed-file-duration-ms-cache nil
  "If non-nil, duration of current file in milliseconds.")

(defun subed-convert-ffprobe-tags-duration-to-ms (duration)
  "Return milliseconds as an integer for DURATION.

DURATION must be a string of the format HH:MM:SS.MMMM.

Example:

00:00:03.003000000 -> 3003
00:00:03.00370000 -> 3004"
  (unless (string-match "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\.\\([0-9]+\\)" duration)
    (error "The duration is not well formatted."))
  (let ((hour (match-string 1 duration))
        (minute (match-string 2 duration))
        (seconds (match-string 3 duration))
        (milliseconds (match-string 4 duration)))
    (+
     (* (string-to-number hour) 3600000)
     (* (string-to-number minute) 60000)
     (* (string-to-number seconds) 1000)
     (* (string-to-number (concat "0." milliseconds)) 1000))))

(defun subed-ffprobe-duration-ms (filename)
  "Use ffprobe to get duration of audio stream in milliseconds of FILENAME."
  (let ((json
         (json-read-from-string
          (with-temp-buffer
            (call-process
             subed-ffprobe-executable nil t nil
             "-v" "error"
             "-print_format" "json"
             "-show_streams"
             "-show_format"
             filename)
            (buffer-string)))))
    ;; Check that the file has at least one audio stream.
    (when (eq (seq-find
               (lambda (stream)
                 (equal (alist-get 'codec_type stream) "audio"))
               (alist-get 'streams json))
              0)
      (error "The provided file doesn't have an audio stream."))
    (cond
     ;; If the file has one stream and it is an audio stream, we can
     ;; get the duration from format=duration
     ;;
     ;; nb_streams equals the number of streams in the media file.
     ((and (eq (alist-get 'nb_streams (alist-get 'format json)) 1)
           (equal (alist-get
                   'codec_type
                   (seq-first (alist-get 'streams json)))
                  "audio"))
      (* 1000 (string-to-number
               (alist-get 'duration (alist-get 'format json)))))
     ;; If the file has more than one stream and only one audio
     ;; stream, return the duration of the audio stream.
     ((and (> (alist-get 'nb_streams (alist-get 'format json)) 1)
           (eq (length (seq-filter
                        (lambda (stream)
                          (equal (alist-get 'codec_type stream) "audio"))
                        (alist-get 'streams json)))
               1))
      (cond
       ((or
         (string-match "\\.mkv\\'" filename)
         (string-match "\\.webm\\'" filename))
        (subed-convert-ffprobe-tags-duration-to-ms
         (alist-get
          'DURATION
          (alist-get
           'tags
           (seq-find
            (lambda (stream)
              (equal (alist-get 'codec_type stream) "audio"))
            (alist-get 'streams json))))))
       (t
        (* 1000
           (string-to-number
            (alist-get
             'duration
             (seq-find
              (lambda (stream)
                (equal (alist-get 'codec_type stream) "audio"))
              (alist-get 'streams json))))))))
     ;; TODO: Some media files might have multiple audio streams
     ;; (e.g. multiple languages). When the media file has multiple
     ;; audio streams, prompt the user for the audio stream. The audio
     ;; stream selected by the user must be stored in a buffer-local
     ;; variable so that ffmpeg knows the audio stream from which the
     ;; waveforms are created.
     )))

(defun subed-clear-file-duration-ms-cache (&rest _)
  "Clear `subed-file-duration-ms-cache'."
  (setq subed-file-duration-ms-cache nil))

(defun subed-file-duration-ms (&optional filename refresh-cache)
  "Return the duration of FILENAME in milliseconds."
  (setq filename (or filename (subed-media-file)))
  (if refresh-cache (setq subed-file-duration-ms-cache nil))
  (cond
   ((numberp subed-file-duration-ms-cache)
    (when (> subed-file-duration-ms-cache 0)
      subed-file-duration-ms-cache))
   (subed-ffprobe-executable
    (setq subed-file-duration-ms-cache
          (subed-ffprobe-duration-ms
           filename))
    (if (and (numberp subed-file-duration-ms-cache)
             (> subed-file-duration-ms-cache 0))
        subed-file-duration-ms-cache
      ;; mark as invalid
      (warn "Could not get file duration for %s" filename)
      (setq subed-file-duration-ms-cache -1)
      nil))))

(defun subed-insert-subtitle-for-whole-file ()
  "Insert a subtitle that starts at 0 until the end of the current file.

This might make it easier to type subtitles from scratch.  Use this
function to start with a subtitle for the whole duration.  It may be a
good idea to enable pausing while typing with
`subed-toggle-pause-while-typing'.

As you type each subtitle's worth of text, use `subed-split-subtitle'
to start a new subtitle at the current playback position.

If there is an error running `subed-ffprobe-executable' points to ffprobe,
use one day as the duration instead."
  (interactive)
  (when (string= (string-trim (buffer-string)) "")
    (subed-auto-insert))
  (subed-append-subtitle
   nil
   0
   (condition-case nil
       (and (subed-media-file)
            (subed-file-duration-ms (subed-media-file)))
     (error (* 24 60 60 1000)))))

(provide 'subed-common)
;;; subed-common.el ends here
