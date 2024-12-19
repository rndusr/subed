;;; subed.el --- A major mode for editing subtitles  -*- lexical-binding: t; -*-

;; Version: 1.2.23
;; Maintainer: Sacha Chua <sacha@sachachua.com>
;; Author: Random User
;; Keywords: convenience, files, hypermedia, multimedia
;; URL: https://github.com/sachac/subed
;; Package-Requires: ((emacs "25.1"))

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
;; subed is a major mode for editing subtitles with Emacs and mpv.  See C-h f
;; subed-mode, the README.org file or https://github.com/rndusr/subed for more
;; information.

;;; Code:

(require 'subed-config)
(require 'subed-debug)
(require 'subed-common)
(require 'subed-mpv)
(require 'subed-menu)

(declare-function tramp-tramp-file-p "tramp")

(define-obsolete-variable-alias 'subed-mpv-frame-step-map 'subed-mpv-control-map "2024-12-18")

(defvar-keymap subed-mpv-control-map
	:doc "Shortcuts for focusing on controlling MPV."
  :name "MPV"
	"." #'subed-mpv-frame-step
	"," #'subed-mpv-frame-back-step
	"<left>" #'subed-mpv-back-large-step
	"S-<left>" #'subed-mpv-back-small-step
	"<right>" #'subed-mpv-large-step
	"S-<right>" #'subed-mpv-small-step
	"SPC" #'subed-mpv-toggle-pause
	"u" #'subed-mpv-undo-seek
	"S-<backspace>" #'subed-mpv-undo-seek
	"j" #'subed-mpv-jump-to-current-subtitle
	"J" #'subed-mpv-jump-to-current-subtitle-near-end
	"s" #'subed-mpv-seek
	"S" #'subed-mpv-jump
  "l" #'subed-toggle-loop-over-current-subtitle
  ;; hmm, should we change these to work with the playback speed instead?
	"[" #'subed-copy-player-pos-to-start-time
	"]" #'subed-copy-player-pos-to-stop-time
	"{" #'subed-copy-player-pos-to-start-time-and-copy-to-previous
	"}" #'subed-copy-player-pos-to-stop-time-and-copy-to-next
	"b" #'subed-backward-subtitle-text
	"f" #'subed-forward-subtitle-text
	"p" #'subed-backward-subtitle-text
	"n" #'subed-forward-subtitle-text
	"i" #'subed-mpv-screenshot
	"I" #'subed-mpv-screenshot-with-subtitles ; note that this is the opposite of MPV's s/S
  "t" #'subed-mpv-copy-position-as-timestamp
  "T" #'subed-mpv-copy-position-as-seconds
  "M-t" #'subed-mpv-copy-position-as-msecs
  "<up>" #'subed-backward-subtitle-text
	"<down>" #'subed-forward-subtitle-text
  "S-<up>" #'subed-mpv-backward-subtitle-and-jump
	"S-<down>" #'subed-mpv-forward-subtitle-and-jump
	;; aegisub-inspired keyboard shortcuts
	"q" #'subed-mpv-jump-to-before-current-subtitle
	"d" #'subed-mpv-jump-to-current-subtitle-near-end
	"e" #'subed-mpv-jump-to-current-subtitle
	"w" #'subed-mpv-jump-to-end-of-current-subtitle
	"x" #'subed-backward-subtitle-text
	"z" #'subed-forward-subtitle-text
  "X" #'subed-mpv-backward-subtitle-and-jump
	"Z" #'subed-mpv-forward-subtitle-and-jump)

(defconst subed-mode-map
  (let ((subed-mode-map (make-keymap)))
    (define-key subed-mode-map (kbd "M-n") #'subed-forward-subtitle-text)
    (define-key subed-mode-map (kbd "M-p") #'subed-backward-subtitle-text)
    (define-key subed-mode-map (kbd "C-M-a") #'subed-jump-to-subtitle-text)
    (define-key subed-mode-map (kbd "C-M-e") #'subed-jump-to-subtitle-end)
    ;; Binding M-[ when Emacs runs in a terminal emulator inserts "O" and "I"
    ;; every time the terminal window looses/gains focus.
    ;; https://emacs.stackexchange.com/questions/48738
    ;; https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h3-FocusIn_FocusOut
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Input-Focus.html
    (if (display-graphic-p)
        (progn
          (define-key subed-mode-map (kbd "M-[") #'subed-decrease-start-time)
          (define-key subed-mode-map (kbd "M-]") #'subed-increase-start-time))
      (progn
        (define-key subed-mode-map (kbd "C-M-[") #'subed-decrease-start-time)
        (define-key subed-mode-map (kbd "C-M-]") #'subed-increase-start-time)))
    (define-key subed-mode-map (kbd "M-{") #'subed-decrease-stop-time)
    (define-key subed-mode-map (kbd "M-}") #'subed-increase-stop-time)
    (define-key subed-mode-map (kbd "C-M-n") #'subed-move-subtitle-forward)
    (define-key subed-mode-map (kbd "C-M-p") #'subed-move-subtitle-backward)
    (define-key subed-mode-map (kbd "C-M-f") #'subed-shift-subtitle-forward)
    (define-key subed-mode-map (kbd "C-M-b") #'subed-shift-subtitle-backward)
    (define-key subed-mode-map (kbd "C-M-x") #'subed-scale-subtitles-forward)
    (define-key subed-mode-map (kbd "C-M-S-x") #'subed-scale-subtitles-backward)
    (define-key subed-mode-map (kbd "M-i") #'subed-insert-subtitle)
    (define-key subed-mode-map (kbd "C-M-i") #'subed-insert-subtitle-adjacent)
    (define-key subed-mode-map (kbd "M-k") #'subed-kill-subtitle)
    (define-key subed-mode-map (kbd "M-m") #'subed-merge-dwim)
    (define-key subed-mode-map (kbd "M-M") #'subed-merge-with-previous)
    (define-key subed-mode-map (kbd "M-.") #'subed-split-subtitle)
    (define-key subed-mode-map (kbd "M-s") #'subed-sort)
    (define-key subed-mode-map (kbd "M-SPC") #'subed-mpv-toggle-pause)
    (define-key subed-mode-map (kbd "M-j") #'subed-mpv-jump-to-current-subtitle)
    (define-key subed-mode-map (kbd "M-J") #'subed-mpv-jump-to-current-subtitle-near-end)
    (define-key subed-mode-map (kbd "C-c C-d") #'subed-toggle-debugging)
    (define-key subed-mode-map (kbd "C-c C-v") #'subed-mpv-play-from-file)
    (define-key subed-mode-map (kbd "C-c C-u") #'subed-mpv-play-from-url)
    (define-key subed-mode-map (kbd "C-c C-f") #'subed-mpv-control)
    (define-key subed-mode-map (kbd "C-c C-p") #'subed-toggle-pause-while-typing)
    (define-key subed-mode-map (kbd "C-c C-l") #'subed-toggle-loop-over-current-subtitle)
    (define-key subed-mode-map (kbd "C-c C-r") #'subed-toggle-replay-adjusted-subtitle)
    (define-key subed-mode-map (kbd "C-c [") #'subed-copy-player-pos-to-start-time)
    (define-key subed-mode-map (kbd "C-c ]") #'subed-copy-player-pos-to-stop-time)
    (define-key subed-mode-map (kbd "C-c .") #'subed-toggle-sync-point-to-player)
    (define-key subed-mode-map (kbd "C-c ,") #'subed-toggle-sync-player-to-point)
    (define-key subed-mode-map (kbd "C-c C-t") (let ((html-tag-keymap (make-sparse-keymap)))
						                                (define-key html-tag-keymap (kbd "C-t") #'subed-insert-html-tag)
						                                (define-key html-tag-keymap (kbd "C-i") #'subed-insert-html-tag-italic)
						                                (define-key html-tag-keymap (kbd "C-b") #'subed-insert-html-tag-bold)
						                                html-tag-keymap))
    subed-mode-map)
  "A keymap for editing subtitles.")

(defun subed-auto-play-media-maybe ()
  "Load media file associated with this subtitle file.
Do not autoplay media over TRAMP."
	(unless (and (featurep 'tramp)
							 (buffer-file-name)
							 (tramp-tramp-file-p (buffer-file-name)))
		(let ((file (subed-guess-media-file)))
			(when file
				(subed-debug "Auto-discovered media file: %s" file)
				(condition-case err
						(subed-mpv-play-from-file file)
					(error (message "%s -- Set subed-auto-find-media to nil to avoid this error."
													(car (cdr err)))))))))
(define-obsolete-function-alias 'subed-auto-find-video-maybe 'subed-auto-play-media-maybe "1.20")

;; TODO: Make these more configurable.
(defun subed-set-up-defaults ()
  "Quietly enable some recommended defaults."
  (subed-enable-pause-while-typing :quiet)
  (subed-enable-sync-point-to-player :quiet)
  (subed-enable-sync-player-to-point :quiet)
  (subed-enable-replay-adjusted-subtitle :quiet)
  (subed-enable-loop-over-current-subtitle :quiet)
  (subed-enable-show-cps :quiet))

;;;###autoload
(define-derived-mode subed-mode text-mode "subed"
  "Major mode for editing subtitles.

subed uses the following terminology when it comes to changes in
subtitles' timestamps:

Adjust - Increase or decrease start or stop time of a subtitle
  Move - Increase or decrease start and stop time of a subtitle
         by the same amount
 Shift - Increase or decrease start and stop time of the current
         and all following subtitles by the same amount

Key bindings:
\\{subed-mode-map}"
  :group 'subed
  (add-hook 'post-command-hook #'subed--post-command-handler :append :local)
  (add-hook 'before-save-hook #'subed-prepare-to-save :append :local)
  (add-hook 'after-save-hook #'subed-mpv-reload-subtitles :append :local)
  (add-hook 'kill-buffer-hook #'subed-mpv-kill :append :local)
  (add-hook 'kill-emacs-hook #'subed-mpv-kill :append :local)
  (add-hook 'after-change-major-mode-hook #'subed-guess-format :append :local)
  (when subed-trim-overlap-check-on-load
		(subed-trim-overlap-check))
  (when subed-auto-play-media
		(subed-auto-play-media-maybe)))

(declare-function subed-ass-mode "subed-ass" (&optional arg))
(declare-function subed-vtt-mode "subed-vtt" (&optional arg))
(declare-function subed-srt-mode "subed-srt" (&optional arg))

(defun subed-guess-format (&optional filename)
  "Set this buffer's format to a more specific subed mode format.
This is a workaround for the transition to using format-specific
modes such as `subed-srt-mode' while `auto-mode-alist' might
still refer to `subed-mode'.  It will also switch to the
format-specific mode if `subed-mode' is called directly.

If FILENAME is specified, use that."
  (when (or filename
            (and (eq major-mode 'subed-mode)
                 (buffer-file-name)))
    (pcase (file-name-extension (or filename (buffer-file-name)))
      ("vtt" (subed-vtt-mode))
      ("srt" (subed-srt-mode))
      ("ass" (subed-ass-mode)))))

(provide 'subed)
;;; subed.el ends here
