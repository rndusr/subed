;;; subed-menu.el --- Menu for subed  -*- lexical-binding: t; -*-

;;; License:

;; Copyright (C) 2024  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: multimedia
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
;; Thanks to Mohsen Banan for the nudge.

;;; Code:

(when (< emacs-major-version 28) ; preloaded in Emacs 28
  (require 'easymenu))

(easy-menu-define subed-menu subed-mode-map "Subed menu."
  `("Subed"
    ("MPV"
     ["Load file" subed-mpv-play-from-file t]
     ["Load URL" subed-mpv-play-from-url t]
     ["Jump to current subtitle" subed-mpv-jump-to-current-subtitle t]
     ["Control" subed-mpv-control t])
    ("Navigate"
     ("Backward"
      ["Start pos" subed-backward-subtitle-start-pos t]
      ["Comment" subed-backward-subtitle-comment t
       :visible (and (featurep 'subed-vtt) subed-vtt-mode)]
      ["Start time" subed-backward-subtitle-start t]
      ["Stop time" subed-backward-subtitle-stop t]
      ["Text" subed-backward-subtitle-text t]
      ["End of subtitle" subed-backward-subtitle-end t])
     ("Forward"
      ["Start pos" subed-forward-subtitle-start-pos t]
      ["Comment" subed-forward-subtitle-comment t
       :visible (and (featurep 'subed-vtt) subed-vtt-mode)]
      ["Start time" subed-forward-subtitle-start t]
      ["Stop time" subed-forward-subtitle-stop t]
      ["Text" subed-forward-subtitle-text t]
      ["End of subtitle" subed-forward-subtitle-end t])
     ("Jump"
      ["Start pos" subed-jump-to-subtitle-start-pos t]
      ["Comment" subed-jump-to-subtitle-comment t
       :visible (and (featurep 'subed-vtt) subed-vtt-mode)]
      ["Start time" subed-jump-to-subtitle-start t]
      ["Stop time" subed-jump-to-subtitle-stop t]
      ["Text" subed-jump-to-subtitle-text t]
      ["End of subtitle" subed-jump-to-subtitle-end t]))
    ["Insert" subed-insert-subtitle t]
    ["Insert adjacent" subed-insert-subtitle-adjacent t]
    ["Prepend" subed-prepend-subtitle t]
    ["Delete current" subed-kill-subtitle]
    ("Change time"
     ("Current subtitle"
      ("Copy player pos to..."
       ["Start time" subed-copy-player-pos-to-start-time]
       ["Start time and previous subtitle" subed-copy-player-pos-to-start-time-and-copy-to-previous]
       ["Stop time" subed-copy-player-pos-to-stop-time]
       ["Stop time and next subtitle" subed-copy-player-pos-to-stop-time-and-copy-to-next])
      ["Set start" subed-set-subtitle-time-start t]
      ["Set stop" subed-set-subtitle-time-stop t]
      ["Decrease start time" subed-decrease-start-time t]
      ["Increase start time" subed-increase-start-time t]
      ["Decrease stop time" subed-decrease-stop-time t]
      ["Increase stop time" subed-increase-stop-time t]
      ["Move backward" subed-move-subtitle-backward t]
      ["Move forward" subed-move-subtitle-forward t])
     ("Current and following subtitles"
      ["Shift to start at time" subed-shift-subtitles-to-start-at-timestamp t]
      ["Shift by msecs" subed-shift-subtitles t]
      ["Shift backward" subed-shift-subtitle-backward t]
      ["Shift forward" subed-shift-subtitle-forward t]
      ["Scale backward" subed-scale-subtitles-backward t]
      ["Scale forward" subed-scale-subtitles-forward t])
     ["Trim overlaps" subed-trim-overlaps t]
     ["Retime" subed-retime-subtitles t])
    ["Set text" subed-set-subtitle-text t]
    ["Set comment" subed-set-subtitle-comment
     :visible (and (featurep 'subed-vtt) subed-vtt-mode)]
    ["Split" subed-split-subtitle t]
    ["Merge" subed-merge-dwim t]
    ["Merge with previous" subed-merge-with-previous t]
    ["Copy region text" subed-copy-region-text t]
    ["Calculate WPM" subed-wpm t]
    ["Sort" subed-sort t]
    ["Convert" subed-convert t]
    ("Crop"
     ["Subtitles" subed-crop-subtitles t]
     ["Media file" subed-crop-media-file t])
    ("Waveforms"
     ["Show for all subtitles" subed-waveform-show-all t]
     ["Show for current" subed-waveform-show-current t]
     ["Hide" subed-waveform-minor-mode
      :active subed-waveform-minor-mode
      :visible subed-waveform-minor-mode])
    ("Align with aeneas"
     ["Region" subed-align-region t]
     ["Buffer" subed-align t])
    ["Load word data from file" subed-word-data-load-from-file t]
    ["Adjust timing using word data" subed-word-data-fix-subtitle-timing t]
    ["Word diff" subed-wdiff-subtitle-text-with-file t]
    ("Options"
     ["Loop over current subtitle" subed-toggle-loop-over-current-subtitle
      :style toggle
      :selected (subed-loop-over-current-subtitle-p)]
     ["Pause when typing" subed-toggle-pause-while-typing
      :style toggle
      :selected (subed-pause-while-typing-p)]
     ["Replay adjusted subtitle" subed-toggle-replay-adjusted-subtitle
      :style toggle
      :selected (subed-replay-adjusted-subtitle-p)]
     ["Show characters per second" subed-toggle-show-cps
      :style toggle
      :selected (subed-show-cps-p)]
     ["Sync player -> point" subed-toggle-sync-player-to-point
      :style toggle
      :selected (subed-sync-player-to-point-p)]
     ["Sync point -> player" subed-toggle-sync-point-to-player
      :style toggle
      :selected (subed-sync-point-to-player-p)]
     ))
  )
;;; Code:

(provide 'subed-menu)
;;; subed-menu.el ends here
