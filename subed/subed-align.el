;;; subed-align.el --- use forced alignment tools like aeneas  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This has some extra support for using Aeneas for forced alignment
;; in order to get VTT or SRT timestamps from a plain text file and an
;; audio file.
;;
;; You will also need aeneas and its dependencies: https://github.com/readbeyond/aeneas
;;
;;; Code:

(require 'subed)

(defvar subed-align-command '("python3" "-m" "aeneas.tools.execute_task")
  "Command to run aeneas.")

(defvar subed-align-language "eng"
  "Language code.")

(defvar subed-align-options nil
  "Other options to include in the aeneas invocation.
Ex: task_adjust_boundary_nonspeech_min=0.500|task_adjust_boundary_nonspeech_string=REMOVE
will remove silence and other non-speech spans.")

;;;###autoload
(defun subed-align-region (audio-file beg end)
  "Align just the given section."
  (interactive
   (list
    (or
     (subed-media-file)
     (subed-guess-media-file subed-audio-extensions)
     (read-file-name "Audio file: "))
    (if (region-active-p) (min (point) (mark)) (point-min))
    (if (region-active-p) (max (point) (mark)) (point-max))))
  (let* ((format (cond
									((derived-mode-p 'subed-vtt-mode) "VTT")
									((derived-mode-p 'subed-srt-mode) "SRT")))
         (input-mode major-mode)
         (input-subtitles (subed-subtitle-list beg end))
         (temp-input-file
          (make-temp-file "subed-align" nil ".txt"
                          (mapconcat (lambda (o) (elt o 3)) input-subtitles "\n\n")))
				 (temp-file
          (concat (make-temp-name "subed-align")
                  "."
                  (if (buffer-file-name)
											(file-name-extension (buffer-file-name))
										(downcase format))))
				 (ignore-before (save-excursion
													(goto-char beg)
													(unless (subed-subtitle-msecs-start)
														(subed-forward-subtitle-text))
													(/ (subed-subtitle-msecs-start) 1000.0)))
				 (process-length (save-excursion
													 (goto-char end)
													 (- (/ (subed-subtitle-msecs-stop) 1000.0)
															ignore-before)))
         results)
    (unwind-protect
        (progn
          (apply
           #'call-process
           (car subed-align-command)
           nil
           (get-buffer-create "*subed-aeneas*")
           t
           (append (cdr subed-align-command)
                   (list (expand-file-name audio-file)
                         temp-input-file
                         (format "is_audio_file_head_length=%.3f|is_audio_file_process_length=%.3f|task_language=%s|os_task_file_format=%s|is_text_type=%s%s"
                                 ignore-before
                                 process-length
                                 subed-align-language
                                 (downcase format)
                                 "subtitles"
                                 (if subed-align-options (concat "|" subed-align-options) ""))
                         temp-file)))
          ;; parse the subtitles from the resulting output
          (setq results (subed-parse-file temp-file))
          (save-excursion
            (subed-for-each-subtitle beg end nil
              (let ((current (pop results)))
                (subed-set-subtitle-time-start (elt current 1))
                (subed-set-subtitle-time-stop (elt current 2)))))
          (run-hook-with-args 'subed-region-adjusted-hook beg end))
      (delete-file temp-input-file)
      (delete-file temp-file))))

;;;###autoload
(defun subed-align (audio-file text-file format)
  "Align AUDIO-FILE with TEXT-FILE to get timestamps in FORMAT.
Return the new filename."
  (interactive
   (list
    (or
     (subed-media-file)
     (subed-guess-media-file subed-audio-extensions)
     (read-file-name "Audio file: "))
    (buffer-file-name)
    (completing-read "Format: "
                     '("AUD" "CSV" "EAF" "JSON" "SMIL" "SRT"
                       "SSV" "SUB" "TEXTGRID" "TSV" "TTML" "TXT" "VTT" "XML"))))
  (let ((new-file
         (and (buffer-file-name)
              (expand-file-name
               (concat (file-name-sans-extension (buffer-file-name)) "." (downcase format)))))
        temp-file subtitles)
    (when (or (null (file-exists-p new-file))
              (yes-or-no-p (format "%s exists. Overwrite? " (file-name-nondirectory new-file))))
      (when (derived-mode-p 'subed-mode)
        (setq subtitles (subed-subtitle-list))
        (setq temp-file (make-temp-file "subed-align" nil ".txt"))
        (with-temp-file temp-file
          (insert (mapconcat (lambda (o) (elt o 3)) subtitles "\n\n"))))
      (apply
       #'call-process
       (car subed-align-command)
       nil
       (get-buffer-create "*subed-aeneas*")
       t
       (append (cdr subed-align-command)
               (list (expand-file-name audio-file)
                     (or temp-file (expand-file-name text-file))
                     (format "task_language=%s|os_task_file_format=%s|is_text_type=%s%s"
                             subed-align-language
                             (downcase format)
                             (if temp-file
                                 "subtitles"
                               "plain")
                             (if subed-align-options (concat "|" subed-align-options) ""))
                     new-file)))
      (when temp-file (delete-file temp-file))
      (with-temp-file new-file
        (insert-file-contents new-file)
        (subed-guess-format new-file)
        (when (derived-mode-p 'subed-mode)
          (subed-trim-overlaps))
        (when (derived-mode-p 'subed-vtt-mode)
          (goto-char (point-min))
          (flush-lines "^[0-9]+$")
          ;; reinsert comments
          (subed-align-reinsert-comments subtitles)))
      (when (called-interactively-p 'any)
        (find-file new-file))
      new-file)))

(defun subed-align-reinsert-comments (subtitles)
  "Reinsert the comments from SUBTITLES.
Assume that the subtitles are still in the same sequence."
  (goto-char (point-min))
  (mapc
   (lambda (sub)
     (subed-forward-subtitle-time-start)
     (when (elt sub 4)
       (subed-set-subtitle-comment (elt sub 4))))
   subtitles))

(provide 'subed-align)
;;; subed-align.el ends here
