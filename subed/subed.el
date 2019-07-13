;;; subed.el --- A major mode for editing subtitles  -*- lexical-binding: t; -*-

;; Version: 0.0
;; Keywords: convenience, files, hypermedia, multimedia
;; URL: https://github.com/rndusr/subed
;; Package-Requires: ((emacs "25"))

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
;; subed is a major mode for editing subtitles with Emacs and mpv.  See
;; README.org or https://github.com/rndusr/subed for more information.

;;; Code:

(require 'subed-srt)
(require 'subed-mpv)

(defgroup subed nil
  "Major mode for editing subtitles."
  :group 'languages
  :group 'hypermedia
  :prefix "subed-")

(defvar subed-mode-map
  (let ((subed-mode-map (make-keymap)))
    (define-key subed-mode-map (kbd "M-n") #'subed-forward-subtitle-text)
    (define-key subed-mode-map (kbd "M-p") #'subed-backward-subtitle-text)
    (define-key subed-mode-map (kbd "C-M-a") #'subed-jump-to-subtitle-text)
    (define-key subed-mode-map (kbd "C-M-e") #'subed-jump-to-subtitle-end)
    (define-key subed-mode-map (kbd "M-[") #'subed-decrease-start-time)
    (define-key subed-mode-map (kbd "M-]") #'subed-increase-start-time)
    (define-key subed-mode-map (kbd "M-{") #'subed-decrease-stop-time)
    (define-key subed-mode-map (kbd "M-}") #'subed-increase-stop-time)
    (define-key subed-mode-map (kbd "C-M-n") #'subed-move-subtitle-forward)
    (define-key subed-mode-map (kbd "C-M-p") #'subed-move-subtitle-backward)
    (define-key subed-mode-map (kbd "C-M-f") #'subed-shift-subtitle-forward)
    (define-key subed-mode-map (kbd "C-M-b") #'subed-shift-subtitle-backward)
    (define-key subed-mode-map (kbd "M-i") #'subed-subtitle-insert)
    (define-key subed-mode-map (kbd "M-k") #'subed-subtitle-kill)
    (define-key subed-mode-map (kbd "M-s") #'subed-sort)
    (define-key subed-mode-map (kbd "M-SPC") #'subed-mpv-toggle-pause)
    (define-key subed-mode-map (kbd "C-c C-d") #'subed-toggle-debugging)
    (define-key subed-mode-map (kbd "C-c C-v") #'subed-mpv-find-video)
    (define-key subed-mode-map (kbd "C-c C-p") #'subed-toggle-pause-while-typing)
    (define-key subed-mode-map (kbd "C-c C-l") #'subed-toggle-subtitle-loop)
    (define-key subed-mode-map (kbd "C-c C-r") #'subed-toggle-replay-adjusted-subtitle)
    (define-key subed-mode-map (kbd "C-c [") #'subed-copy-player-pos-to-start-time)
    (define-key subed-mode-map (kbd "C-c ]") #'subed-copy-player-pos-to-stop-time)
    (define-key subed-mode-map (kbd "C-c .") #'subed-toggle-sync-point-to-player)
    (define-key subed-mode-map (kbd "C-c ,") #'subed-toggle-sync-player-to-point)
    subed-mode-map)
  "Keymap for ‘subed-mode’.")

;; Syntax highlighting

(defface subed-srt-id-face
  '((t (:foreground "sandybrown")))
  "Each subtitle's consecutive number")

(defface subed-srt-time-face
  '((t (:foreground "skyblue")))
  "Start and stop times of subtitles")

(defface subed-srt-time-separator-face
  '((t (:foreground "dimgray")))
  "Separator between the start and stop time (\" --> \")")

(defface subed-srt-text-face
  '((t (:foreground "brightyellow")))
  "Text of the subtitle")

;; Variables

(defvar-local subed-mode--enabled-p nil
  "Whether `subed-mode' is enabled.
This is set by `subed-mode-enable' and `subed-mode-disable'.")

(defvar-local subed-debugging-enabled-p nil
  "Whether debugging messages are displayed.")

(defcustom subed-debug-buffer "*subed-debug*"
  "Name of the buffer that contains debugging messages."
  :type 'string
  :group 'subed)

(defcustom subed-mode-hook nil
  "Functions to call when entering subed mode."
  :type 'hook
  :group 'subed)

(defcustom subed-video-extensions '("mkv" "mp4" "webm" "avi" "ts" "ogv")
  "Video file name extensions."
  :type 'list
  :group 'subed)

(defcustom subed-auto-find-video t
  "Whether to open the video automatically when opening a subtitle file."
  :type 'boolean
  :group 'subed)

(defcustom subed-milliseconds-adjust 100
  "Milliseconds to add or subtract from start/stop time.

This variable is used when adjusting, moving or shifting
subtitles without a prefix argument.

This variable is set when adjusting, moving or shifting subtitles
with a prefix argument.  See `subed-increase-start-time' for
details.

Use `setq-default' to change the default value of this variable."
  :type 'float
  :group 'subed)

(defun subed-get-milliseconds-adjust (arg)
  "Set `subed-milliseconds-adjust' to ARG if it's a number.

If ARG is non-nil, reset `subed-milliseconds-adjust' to its
default.

Return new `subed-milliseconds-adjust' value."
  (cond ((integerp arg)
          (setq subed-milliseconds-adjust arg)) ; Custom adjustment
        (arg
         (custom-reevaluate-setting 'subed-milliseconds-adjust))) ; Reset to default
  subed-milliseconds-adjust)

(defcustom subed-playback-speed-while-typing 0
  "Video playback speed while the user is editing the buffer.
If set to zero or smaller, playback is paused."
  :type 'float
  :group 'subed)

(defcustom subed-playback-speed-while-not-typing 1.0
  "Video playback speed while the user is not editing the buffer."
  :type 'float
  :group 'subed)

(defcustom subed-unpause-after-typing-delay 1.0
  "Number of seconds to wait after typing stopped before unpausing the player."
  :type 'float
  :group 'subed)

(defvar-local subed--player-is-auto-paused nil
  "Whether the player was paused by the user or automatically.")

(defcustom subed-subtitle-spacing 100
  "Milliseconds between subtitles when adjusting or inserting subtitles."
  :type 'integer
  :group 'subed)

(defcustom subed-default-subtitle-length 1.0
  "How long to make subtitles in seconds when inserted after the last subtitle."
  :type 'float
  :group 'subed)

(defcustom subed-loop-seconds-before 0
  "Prelude in seconds when looping over subtitle(s)."
  :type 'float
  :group 'subed)

(defcustom subed-loop-seconds-after 0
  "Addendum in seconds when looping over subtitle(s)."
  :type 'float
  :group 'subed)

(defvar-local subed--subtitle-loop-start nil
  "Start position of loop in player in milliseconds.")

(defvar-local subed--subtitle-loop-stop nil
  "Stop position of loop in player in milliseconds.")

(defcustom subed-point-sync-delay-after-motion 1.0
  "Number of seconds the player can't adjust point after point was moved by the user."
  :type 'float
  :group 'subed)

(defvar-local subed--point-was-synced nil
  "When temporarily disabling point-to-player sync, this variable
remembers whether it was originally enabled by the user.")

(defcustom subed-mpv-socket-dir (concat (temporary-file-directory) "subed-mpv-socket")
  "Path to Unix IPC socket that is passed to mpv's --input-ipc-server option."
  :type 'file
  :group 'subed)

(defcustom subed-mpv-executable "mpv"
  "Path or filename of mpv executable."
  :type 'file
  :group 'subed)

(defcustom subed-mpv-arguments '("--osd-level" "2" "--osd-fractions")
  "Additional arguments for \"mpv\".
The options --input-ipc-server=SRTEDIT-MPV-SOCKET and --idle are
hardcoded."
  :type '(repeat string)
  :group 'subed)

(defun subed--buffer-file-name ()
  "Return base name of buffer file name or a default name."
  (file-name-nondirectory (or (buffer-file-name) "unnamed")))

;; Hooks

(defvar subed-subtitle-time-adjusted-hook ()
  "Functions to call when a subtitle's start or stop time has changed.
The functions are called with the subtitle's start time.")

(defun subed--run-subtitle-time-adjusted-hook ()
  "Run `subed-subtitle-time-adjusted-hook' functions.
The functions are called with the subtitle's start time."
  (when subed-subtitle-time-adjusted-hook
    (run-hook-with-args 'subed-subtitle-time-adjusted-hook
                        (subed-subtitle-msecs-start))))

(defvar-local subed-point-motion-hook nil
  "Functions to call after point changed.")

(defvar-local subed-subtitle-motion-hook nil
  "Functions to call after current subtitle changed.")

(defvar-local subed--status-point 1
  "Keeps track of `(point)' to detect changes.")

(defvar-local subed--status-subtitle-id 1
  "Keeps track of `(subed-subtitle-id)' to detect changes.")

(defun subed--post-command-handler ()
  "Detect point motion and user entering text and signal hooks."
  ;; Check for point motion first; skip checking for other changes if it didn't
  (let ((new-point (point)))
    (when (and new-point subed--status-point
               (not (= new-point subed--status-point)))

      ;; If point is synced to playback position, temporarily prevent unexpected
      ;; movement of the cursor.
      (subed-disable-sync-point-to-player-temporarily)

      (setq subed--status-point new-point)
      ;; Signal point motion
      (run-hooks 'subed-point-motion-hook)
      (let ((new-sub-id (subed-subtitle-id)))
        (when (and new-sub-id subed--status-subtitle-id
                   (not (= subed--status-subtitle-id new-sub-id)))
          (setq subed--status-subtitle-id new-sub-id)
          ;; Signal motion between subtitles
          (run-hooks 'subed-subtitle-motion-hook))))))

;; Abstraction layer to allow support for other subtitle formats
(defcustom subed-font-lock-keywords subed-srt-font-lock-keywords
  "The particular font-lock keywords in use by subed."
  :type 'list
  :group 'subed)

(defalias 'subed-subtitle-id #'subed-srt--subtitle-id)
(defalias 'subed-subtitle-id-max #'subed-srt--subtitle-id-max)
(defalias 'subed-subtitle-msecs-start #'subed-srt--subtitle-msecs-start)
(defalias 'subed-subtitle-msecs-stop #'subed-srt--subtitle-msecs-stop)
(defalias 'subed-subtitle-text #'subed-srt--subtitle-text)
(defalias 'subed-subtitle-relative-point #'subed-srt--subtitle-relative-point)
(defalias 'subed-set-subtitle-time-start #'subed-srt--set-subtitle-time-start)
(defalias 'subed-set-subtitle-time-stop #'subed-srt--set-subtitle-time-stop)

(defalias 'subed-jump-to-subtitle-id #'subed-srt--jump-to-subtitle-id)
(defalias 'subed-jump-to-subtitle-time-start #'subed-srt--jump-to-subtitle-time-start)
(defalias 'subed-jump-to-subtitle-time-stop #'subed-srt--jump-to-subtitle-time-stop)
(defalias 'subed-jump-to-subtitle-text-at-msecs #'subed-srt--jump-to-subtitle-text-at-msecs)
(defalias 'subed-jump-to-subtitle-text #'subed-srt--jump-to-subtitle-text)
(defalias 'subed-jump-to-subtitle-end #'subed-srt--jump-to-subtitle-end)

(defalias 'subed-forward-subtitle-id #'subed-srt--forward-subtitle-id)
(defalias 'subed-backward-subtitle-id #'subed-srt--backward-subtitle-id)
(defalias 'subed-forward-subtitle-text #'subed-srt--forward-subtitle-text)
(defalias 'subed-backward-subtitle-text #'subed-srt--backward-subtitle-text)
(defalias 'subed-forward-subtitle-time-start #'subed-srt--forward-subtitle-time-start)
(defalias 'subed-backward-subtitle-time-start #'subed-srt--backward-subtitle-time-start)
(defalias 'subed-forward-subtitle-time-stop #'subed-srt--forward-subtitle-time-stop)
(defalias 'subed-backward-subtitle-time-stop #'subed-srt--backward-subtitle-time-stop)

(defalias 'subed-subtitle-insert #'subed-srt--subtitle-insert)
(defalias 'subed-subtitle-kill #'subed-srt--subtitle-kill)
(defalias 'subed-sanitize #'subed-srt--sanitize)
(defalias 'subed-sort #'subed-srt--sort)


;;; Debugging

(defun subed-enable-debugging ()
  "Hide debugging messages and set `debug-on-error' to nil."
  (interactive)
  (unless subed-debugging-enabled-p
    (setq subed-debugging-enabled-p t
          debug-on-error t)
    (let ((debug-buffer (get-buffer-create subed-debug-buffer))
          (debug-window (or (get-buffer-window subed-debug-buffer)
                            (split-window-horizontally (max 35 (floor (* 0.3 (window-width))))))))
      (set-window-buffer debug-window debug-buffer)
      (with-current-buffer debug-buffer
        (buffer-disable-undo)
        (setq-local buffer-read-only t)))
    (add-hook 'kill-buffer-hook #'subed-disable-debugging :append :local)))

(defun subed-disable-debugging ()
  "Display debugging messages in separate window and set `debug-on-error' to t."
  (interactive)
  (when subed-debugging-enabled-p
    (setq subed-debugging-enabled-p nil
          debug-on-error nil)
    (let ((debug-window (get-buffer-window subed-debug-buffer)))
      (when debug-window
        (delete-window debug-window)))
    (remove-hook 'kill-buffer-hook #'subed-disable-debugging :local)))

(defun subed-toggle-debugging ()
  "Display or hide debugging messages in separate window and set `debug-on-error' to t or nil."
  (interactive)
  (if subed-debugging-enabled-p
      (subed-disable-debugging)
    (subed-enable-debugging)))

(defun subed-debug (msg &rest args)
  "Pass MSG and ARGS to `format' and show the result in debugging buffer if it exists."
  (when (get-buffer subed-debug-buffer)
    (with-current-buffer (get-buffer-create subed-debug-buffer)
      (setq-local buffer-read-only nil)
      (insert (apply #'format (concat msg "\n") args))
      (setq-local buffer-read-only t)
      (let ((debug-window (get-buffer-window subed-debug-buffer)))
        (when debug-window
          (set-window-point debug-window (goto-char (point-max))))))))


;;; Utilities

(defmacro subed-save-excursion (&rest body)
  "Restore relative point within current subtitle after executing BODY.
This also works if the buffer changes (e.g. when sorting
subtitles) as long the subtitle IDs don't change."
  (save-excursion
    `(let ((sub-id (subed-subtitle-id))
           (sub-pos (subed-subtitle-relative-point)))
       (progn ,@body)
       (subed-jump-to-subtitle-id sub-id)
       ;; Subtitle text may have changed and we may not be able to move to the
       ;; exact original position
       (condition-case nil
           (forward-char sub-pos)
         ('beginning-of-buffer nil)
         ('end-of-buffer nil)))))

(defmacro subed-for-each-subtitle (&optional beg end reverse &rest body)
  "Run BODY for each subtitle between the region specified by BEG and END.
If END is nil, it defaults to `point-max'.
If BEG and END are both nil, run BODY only on the subtitle at point.
If REVERSE is non-nil, start on the subtitle at END and move backwards.
Before BODY is run, point is placed on the subtitle's ID."
  (declare (indent defun))
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
             (save-excursion (goto-char endm)
                             (subed-jump-to-subtitle-id)
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
           (save-excursion (goto-char begm)
                           (subed-jump-to-subtitle-id)
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

(defun subed--right-pad (string length fillchar)
  "Use FILLCHAR to make STRING LENGTH characters long."
  (concat string (make-string (- length (length string)) fillchar)))


;;; Adjusting start/stop time individually

(defun subed-adjust-subtitle-time-start (msecs &optional
                                               ignore-negative-duration
                                               ignore-spacing)
  "Add MSECS milliseconds to start time (use negative value to subtract).

Unless IGNORE-NEGATIVE-DURATION is non-nil, reduce MSECS so that
the start time isn't larger than the stop time.  Zero-length
subtiltes are always allowed.

Unless IGNORE-SPACING is non-nil, if the adjustment would result
in gaps between subtitles being smaller than
`subed-subtitle-spacing', reduce MSECS so that this doesn't
happen.

Return the number of milliseconds the start time was adjusted or
nil if nothing changed."
  (subed-disable-sync-point-to-player-temporarily)
  (let* ((msecs-start (subed-subtitle-msecs-start))
         (msecs-new (when msecs-start (+ msecs-start msecs))))
    (when msecs-new
      (if (> msecs 0)
          ;; Adding to start time
          (unless ignore-negative-duration
            (let ((msecs-stop (subed-subtitle-msecs-stop)))
              (setq msecs-new (min msecs-new msecs-stop))))
        ;; Subtracting from start time
        (unless ignore-spacing
          (let* ((msecs-prev-stop (save-excursion (when (subed-backward-subtitle-id)
                                                    (subed-subtitle-msecs-stop))))
                 (msecs-min (if msecs-prev-stop
                                (+ msecs-prev-stop subed-subtitle-spacing) 0)))
            (when msecs-min
              (setq msecs-new (max msecs-new msecs-min))))))
      ;; msecs-new must be bigger than the current start time if we are adding
      ;; or smaller if we are subtracting.
      (when (or (and (> msecs 0) (> msecs-new msecs-start))   ;; Adding
                (and (< msecs 0) (< msecs-new msecs-start)))  ;; Subtracting
        (subed-set-subtitle-time-start msecs-new)
        (subed--run-subtitle-time-adjusted-hook)
        (- msecs-new msecs-start)))))

(defun subed-adjust-subtitle-time-stop (msecs &optional
                                              ignore-negative-duration
                                              ignore-spacing)
  "Add MSECS milliseconds to stop time (use negative value to subtract).

Unless IGNORE-NEGATIVE-DURATION is non-nil, increase MSECS so
that the stop time isn't smaller than the start time.
Zero-length subtiltes are always allowed.

Unless IGNORE-SPACING is non-nil, if the adjustment would result
in gaps between subtitles being smaller than
`subed-subtitle-spacing', reduce MSECS so that this doesn't
happen.

Return the number of milliseconds the stop time was adjusted or
nil if nothing changed."
  (subed-disable-sync-point-to-player-temporarily)
  (let* ((msecs-stop (subed-subtitle-msecs-stop))
         (msecs-new (when msecs-stop (+ msecs-stop msecs))))
    (when msecs-new
      (if (> msecs 0)
          ;; Adding to stop time
          (unless ignore-spacing
            (let* ((msecs-next-start (save-excursion (when (subed-forward-subtitle-id)
                                                       (subed-subtitle-msecs-start))))
                   (msecs-max (when msecs-next-start
                                (- msecs-next-start subed-subtitle-spacing))))
              (when msecs-max
                (setq msecs-new (min msecs-new msecs-max)))))
        ;; Subtracting from stop time
        (unless ignore-negative-duration
          (let ((msecs-start (subed-subtitle-msecs-start)))
            (setq msecs-new (max msecs-new msecs-start)))))
      ;; msecs-new must be bigger than the current stop time if we are adding or
      ;; smaller if we are subtracting.
      (when (or (and (> msecs 0) (> msecs-new msecs-stop))   ;; Adding
                (and (< msecs 0) (< msecs-new msecs-stop)))  ;; Subtracting
        (subed-set-subtitle-time-stop msecs-new)
        (subed--run-subtitle-time-adjusted-hook)
        (- msecs-new msecs-stop)))))

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
  (subed-adjust-subtitle-time-start (subed-get-milliseconds-adjust arg)))

(defun subed-decrease-start-time (&optional arg)
  "Subtract `subed-milliseconds-adjust' milliseconds from start time.

Return new start time in milliseconds or nil if it didn't change.

See `subed-increase-start-time' about ARG."
  (interactive "P")
  (subed-adjust-subtitle-time-start (* -1 (subed-get-milliseconds-adjust arg))))

(defun subed-increase-stop-time (&optional arg)
  "Add `subed-milliseconds-adjust' milliseconds to stop time.

Return new stop time in milliseconds or nil if it didn't change.

See `subed-increase-start-time' about ARG."
  (interactive "P")
  (subed-adjust-subtitle-time-stop (subed-get-milliseconds-adjust arg)))

(defun subed-decrease-stop-time (&optional arg)
  "Subtract `subed-milliseconds-adjust' milliseconds from stop time.

Return new stop time in milliseconds or nil if it didn't change.

See `subed-increase-start-time' about ARG."
  (interactive "P")
  (subed-adjust-subtitle-time-stop (* -1 (subed-get-milliseconds-adjust arg))))

(defun subed-copy-player-pos-to-start-time ()
  "Replace current subtitle's start timestamp with mpv player's current timestamp."
  (interactive)
  (subed-srt--jump-to-subtitle-time-start)
  (when (and subed-mpv-playback-position
	         (looking-at subed-srt--regexp-timestamp))
    (replace-match (subed-srt--msecs-to-timestamp subed-mpv-playback-position))
    (subed--run-subtitle-time-adjusted-hook)))

(defun subed-copy-player-pos-to-stop-time ()
  "Replace current subtitle's stop timestamp with mpv player's current timestamp."
  (interactive)
  (subed-srt--jump-to-subtitle-time-stop)
  (when (and subed-mpv-playback-position
	         (looking-at subed-srt--regexp-timestamp))
    (replace-match (subed-srt--msecs-to-timestamp subed-mpv-playback-position))
    (subed--run-subtitle-time-adjusted-hook)))


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
      (lambda (msecs &optional ignore-limits)
        (let ((msecs (subed-adjust-subtitle-time-stop msecs
                                                      :ignore-negative-duration
                                                      ignore-limits)))
          (when msecs (subed-adjust-subtitle-time-start msecs
                                                        :ignore-negative-duration
                                                        ignore-limits))))
    (lambda (msecs &optional ignore-limits)
      (let ((msecs (subed-adjust-subtitle-time-start msecs
                                                     :ignore-negative-duration
                                                     ignore-limits)))
        (when msecs (subed-adjust-subtitle-time-stop msecs
                                                     :ignore-negative-duration
                                                     ignore-limits))))))

(defun subed--move-current-subtitle (msecs)
  "Move subtitle on point by MSECS milliseconds."
  (unless (= msecs 0)
    (subed-with-subtitle-replay-disabled
      (cl-flet ((move-subtitle (subed--get-move-subtitle-func msecs)))
        (move-subtitle msecs)))))

(defun subed--move-subtitles-in-region (msecs beg end)
  "Move subtitles in region specified by BEG and END by MSECS milliseconds."
  (unless (= msecs 0)
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
                  (move-subtitle msecs :ignore-spacing)))
            ;; Start on first subtitle to see if/how far we can move backward.
            (save-excursion
              (goto-char beg)
              (unless (setq msecs (move-subtitle msecs))
                (throw 'bumped-into-subtitle t))
              (subed-forward-subtitle-id)
              (subed-for-each-subtitle (point) end
                (move-subtitle msecs :ignore-spacing)))))))))

(defun subed-move-subtitles (msecs &optional beg end)
  "Move subtitles between BEG and END MSECS milliseconds forward.
Use a negative MSECS value to move subtitles backward.
If END is nil, move all subtitles from BEG to end of buffer.
If BEG is nil, move only the current subtitle.
After subtitles are moved, replay the first moved subtitle if
replaying is enabled."
  (cond ((and beg end) (subed--move-subtitles-in-region msecs beg end))
        (beg (subed--move-subtitles-in-region msecs beg (point-max)))
        (t (subed--move-current-subtitle msecs)))
  (when (subed-replay-adjusted-subtitle-p)
    (save-excursion
      (when beg (goto-char beg))
      (subed-mpv-jump (subed-subtitle-msecs-start)))))

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
        (beg (when (use-region-p) (region-beginning)))
        (end (when (use-region-p) (region-end))))
    (subed-move-subtitles msecs beg end)))

(defun subed-move-subtitle-backward (&optional arg)
  "Move subtitle `subed-milliseconds-adjust' backward.

See `subed-move-subtitle-forward' about ARG."
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (* -1 (subed-get-milliseconds-adjust arg)))
        (beg (when (use-region-p) (region-beginning)))
        (end (when (use-region-p) (region-end))))
    (subed-move-subtitles msecs beg end)))


;;; Shifting subtitles
;;; (same as moving, but follow-up subtitles are also moved)

(defun subed-shift-subtitle-forward (&optional arg)
  "Shift subtitle `subed-milliseconds-adjust' backward.

Shifting is like moving, but it always moves the subtitles
between point and the end of the buffer.

See `subed-move-subtitle-forward' about ARG."
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (subed-get-milliseconds-adjust arg))
        (beg (if (use-region-p) (region-beginning) (point))))
    (subed-move-subtitles msecs beg)))

(defun subed-shift-subtitle-backward (&optional arg)
  "Shift subtitle `subed-milliseconds-adjust' backward.

Shifting is like moving, but it always moves the subtitles
between point and the end of the buffer.

See `subed-move-subtitle-forward' about ARG."
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (* -1 (subed-get-milliseconds-adjust arg)))
        (beg (if (use-region-p) (region-beginning) (point))))
    (subed-move-subtitles msecs beg)))


;;; Replay time-adjusted subtitle

(defun subed-replay-adjusted-subtitle-p ()
  "Whether the player jumps to start time if start or stop time is adjusted."
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
  (subed-debug "Replaying subtitle at: %s" (subed-srt--msecs-to-timestamp msecs-start))
  (subed-mpv-jump msecs-start))


;;; Sync point-to-player

(defun subed-sync-point-to-player-p ()
  "Whether point is automatically moved to currently playing subtitle."
  (member #'subed--sync-point-to-player subed-mpv-playback-position-hook))

(defun subed-enable-sync-point-to-player (&optional quiet)
  "Automatically move point to the currently playing subtitle.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (unless (subed-sync-point-to-player-p)
    (add-hook 'subed-mpv-playback-position-hook #'subed--sync-point-to-player :append :local)
    (subed-debug "Enabled syncing point to playback position: %s" subed-mpv-playback-position-hook)
    (unless quiet
      (message "Enabled syncing point to playback position"))))

(defun subed-disable-sync-point-to-player (&optional quiet)
  "Do not move point automatically to the currently playing subtitle.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
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
  (when (and (not (use-region-p))
             (subed-jump-to-subtitle-text-at-msecs msecs))
    (subed-debug "Synchronized point to playback position: %s -> #%s"
                 (subed-srt--msecs-to-timestamp msecs) (subed-subtitle-id))
    ;; post-command-hook is not triggered because we didn't move interactively.
    ;; But there's not really a difference, e.g. the minor mode `hl-line' breaks
    ;; unless we call its post-command function, so we do it manually.
    ;; It's also important NOT to call our own post-command function because
    ;; that causes player-to-point syncing, which would get hairy.
    (remove-hook 'post-command-hook #'subed--post-command-handler)
    (run-hooks 'post-command-hook)
    (add-hook 'post-command-hook #'subed--post-command-handler :append :local)))

(defvar-local subed--point-sync-delay-after-motion-timer nil)
(defun subed-disable-sync-point-to-player-temporarily ()
  "Temporarily disable syncing point to player.

After `subed-point-sync-delay-after-motion' seconds point is re-synced."
  (if subed--point-sync-delay-after-motion-timer
      (cancel-timer subed--point-sync-delay-after-motion-timer)
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

(defun subed-subtitle-loop-p ()
  "Whether the player is looping over the current subtitle."
  (or subed--subtitle-loop-start subed--subtitle-loop-stop))

(defun subed-toggle-subtitle-loop (&optional quiet)
  "Enable or disable looping in player over the current subtitle.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (if (subed-subtitle-loop-p)
      (progn
        (remove-hook 'subed-mpv-playback-position-hook #'subed--ensure-subtitle-loop :local)
        (remove-hook 'subed-subtitle-motion-hook #'subed--set-subtitle-loop :local)
        (setq subed--subtitle-loop-start nil
              subed--subtitle-loop-stop nil)
        (subed-debug "Disabling loop: %s - %s" subed--subtitle-loop-start subed--subtitle-loop-stop)
        (unless quiet
          (message "Disabled looping")))
    (subed--set-subtitle-loop (subed-subtitle-id))
    (add-hook 'subed-mpv-playback-position-hook #'subed--ensure-subtitle-loop :append :local)
    (add-hook 'subed-subtitle-motion-hook #'subed--set-subtitle-loop :append :local)
    (subed-debug "Enabling loop: %s - %s" subed--subtitle-loop-start subed--subtitle-loop-stop)))

(defun subed--set-subtitle-loop (&optional sub-id)
  "Set loop positions to start/stop time of SUB-ID or current subtitle."
  (setq subed--subtitle-loop-start (- (subed-subtitle-msecs-start sub-id)
                                      (* subed-loop-seconds-before 1000))
        subed--subtitle-loop-stop (+ (subed-subtitle-msecs-stop sub-id)
                                     (* subed-loop-seconds-after 1000)))
  (subed-debug "Set loop: %s - %s"
               (subed-srt--msecs-to-timestamp subed--subtitle-loop-start)
               (subed-srt--msecs-to-timestamp subed--subtitle-loop-stop))
  (message "Looping over %s - %s"
           (subed-srt--msecs-to-timestamp subed--subtitle-loop-start)
           (subed-srt--msecs-to-timestamp subed--subtitle-loop-stop)))

(defun subed--ensure-subtitle-loop (cur-msecs)
  "Jump to current subtitle start time if CUR-MSECS is after stop time."
  (when (and subed--subtitle-loop-start subed--subtitle-loop-stop
             subed-mpv-is-playing)
    (when (or (< cur-msecs subed--subtitle-loop-start)
              (> cur-msecs subed--subtitle-loop-stop))
      (subed-debug "%s -> Looping over %s - %s"
                   (subed-srt--msecs-to-timestamp cur-msecs)
                   (subed-srt--msecs-to-timestamp subed--subtitle-loop-start)
                   (subed-srt--msecs-to-timestamp subed--subtitle-loop-stop))
      (subed-mpv-jump subed--subtitle-loop-start))))


;;; Pause player while the user is editing

(defun subed-pause-while-typing-p ()
  "Whether player is automatically paused or slowed down during editing.

See `subed-playback-speed-while-typing' and
`subed-playback-speed-while-not-typing'."
  (member #'subed--pause-while-typing after-change-functions))

(defun subed-enable-pause-while-typing (&optional quiet)
  "Pause player while the user is editing a subtitle.

After `subed-unpause-after-typing-delay' seconds, playback is
resumed automatically unless the player was paused already.

If QUIET is non-nil, do not display a message in the minibuffer."
  (unless (subed-pause-while-typing-p)
    (add-hook 'after-change-functions #'subed--pause-while-typing :append :local)
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
    (remove-hook 'after-change-functions #'subed--pause-while-typing :local)
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


(defun subed-guess-video-file ()
  "Find video file with same base name as the opened file in the buffer.

The file extension of the return value of the function
`buffer-file-name' is replaced with each item in
`subed-video-extensions' and the first existing file is returned.

Language codes are also handled; e.g. \"foo.en.srt\" or
\"foo.estonian.srt\" -> \"foo.{mkv,mp4,...}\" (this actually
simply removes the extension from the extension-stripped file
name).

Return nil if function `buffer-file-name' returns nil."
  (when (buffer-file-name)
    (catch 'found-videofile
      (let* ((file-base (file-name-sans-extension (buffer-file-name)))
	         (file-stem (file-name-sans-extension file-base)))
	    (dolist (extension subed-video-extensions)
	      (let ((file-base-video (format "%s.%s" file-base extension))
		        (file-stem-video (format "%s.%s" file-stem extension)))
	        (when (file-exists-p file-base-video)
	          (throw 'found-videofile file-base-video))
	        (when (file-exists-p file-stem-video)
	          (throw 'found-videofile file-stem-video))))))))


;;;###autoload
(defun subed-mode-enable ()
  "Enable subed mode."
  (interactive)
  (kill-all-local-variables)
  (setq-local font-lock-defaults '(subed-font-lock-keywords))
  (setq-local paragraph-start "^[[:alnum:]\n]+")
  (setq-local paragraph-separate "\n\n")
  (use-local-map subed-mode-map)
  (add-hook 'post-command-hook #'subed--post-command-handler :append :local)
  (add-hook 'before-save-hook #'subed-sort :append :local)
  (add-hook 'after-save-hook #'subed-mpv-reload-subtitles :append :local)
  (add-hook 'kill-buffer-hook #'subed-mpv-kill :append :local)
  (add-hook 'kill-emacs-hook #'subed-mpv-kill :append :local)
  (when subed-auto-find-video
    (let ((video-file (subed-guess-video-file)))
      (when video-file
        (subed-debug "Auto-discovered video file: %s" video-file)
        (condition-case err
            (subed-mpv-find-video video-file)
          (error (message "%s -- Set subed-auto-find-video to nil to avoid this error."
                          (car (cdr err))))))))
  (subed-enable-pause-while-typing :quiet)
  (subed-enable-sync-point-to-player :quiet)
  (subed-enable-sync-player-to-point :quiet)
  (subed-enable-replay-adjusted-subtitle :quiet)
  (setq major-mode 'subed-mode
        mode-name "subed")
  (setq subed-mode--enabled-p t)
  (run-mode-hooks 'subed-mode-hook))

(defun subed-mode-disable ()
  "Disable subed mode."
  (interactive)
  (subed-disable-pause-while-typing :quiet)
  (subed-disable-sync-point-to-player :quiet)
  (subed-disable-sync-player-to-point :quiet)
  (subed-disable-replay-adjusted-subtitle :quiet)
  (subed-mpv-kill)
  (subed-disable-debugging)
  (kill-all-local-variables)
  (remove-hook 'post-command-hook #'subed--post-command-handler :local)
  (remove-hook 'before-save-hook #'subed-sort :local)
  (remove-hook 'after-save-hook #'subed-mpv-reload-subtitles :local)
  (remove-hook 'kill-buffer-hook #'subed-mpv-kill :local)
  (setq subed-mode--enabled-p nil))

;;;###autoload
(defun subed-mode ()
  "Major mode for editing subtitles.

This function enables or disables `subed-mode'.  See also
`subed-mode-enable' and `subed-mode-disable'.

Key bindings:
\\{subed-mode-map}"
  (interactive)
  ;; Use 'enabled property of this function to store enabled/disabled status
  (if subed-mode--enabled-p
      (subed-mode-disable)
    (subed-mode-enable)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.srt\\'" . subed-mode-enable))

(provide 'subed)
;;; subed.el ends here
