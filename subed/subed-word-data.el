;;; subed-word-data.el --- Use word-level timing data when splitting subtitles  -*- lexical-binding: t; -*-

;;; License:
;;
;; Copyright (C) 2022  Sacha Chua

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

;; This file parses timing data such as the ones you get from YouTube
;; .srv2 or WhisperX JSON and tries to match the timing data with the remaining text in
;; the current subtitle in order to determine the word timestamp for
;; splitting the subtitle.

;; To try to automatically load word data from a similarly-named file
;; in the buffer, add this to your configuration:

;; (with-eval-after-load 'subed
;;   (add-hook 'subed-mode-hook 'subed-word-data-load-maybe))

;;; Code:

(require 'xml)
(require 'dom)

(defvar-local subed-word-data--cache nil
  "Word-level timing in the form ((start . ms) (end . ms) (text . ms))")

(defcustom subed-word-data-score-faces '((0.8 . compilation-info)
                               (0.4 . compilation-warning)
                               (0 . compilation-error))
  "Alist of score thresholds and faces to use."
  :type '(alist :key-type float :value-type face))

(defface subed-word-data-face '((((class color) (background light))
                  :foreground "darkgreen")
                 (((class color) (background dark))
                  :foreground "lightgreen"))
  "Face used for words with word data available.")

(defun subed-word-data--extract-words-from-srv2 (data)
  "Extract the timing from DATA in SRV2 format.
Return a list of ((start . ?), (end . ?) (text . ?))."
  (when (stringp data)
    (with-temp-buffer (insert data) (setq data (xml-parse-region))))
  (let* ((text-elements (reverse (dom-by-tag data 'text)))
         (last-start
          (and text-elements
               (+ (string-to-number
                   (alist-get 't (xml-node-attributes (car text-elements))))
                  (string-to-number (alist-get 'd (xml-node-attributes (car text-elements))))))))
    (reverse
     (mapcar #'(lambda (element)
                 (let ((rec (list (cons 'start (string-to-number (alist-get 't (xml-node-attributes element))))
                                  (cons 'end (min (+ (string-to-number (alist-get 't (xml-node-attributes element)))
                                                     (string-to-number (alist-get 'd (xml-node-attributes element))))
                                                  last-start))
                                  (cons 'text
                                         (replace-regexp-in-string "&#39;" "'"
                                                                   (car (xml-node-children element)))
                                         ))))
                   (setq last-start (alist-get 'start rec))
                   rec))
               text-elements))))

(defun subed-word-data--extract-words-from-whisperx-json (file &optional from-string)
  "Extract the timing from file in WhisperX's JSON format.
Return a list of ((start . ?), (end . ?) (text . ?) (score . ?)).
If FROM-STRING is non-nil, treat FILE as the data itself."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (data (if from-string
                   (json-read-from-string file)
                 (json-read-file file)))
         (base (seq-mapcat
								(lambda (segment)
									(seq-map (lambda (info)
														 (let-alist info
															 `((start . ,(and .start (* 1000 .start)))
                                 (end . ,(and .end (* 1000 .end)))
																 (text . ,(identity .word))
                                 (score . ,(identity .score)))))
													 (alist-get 'words segment)))
								(alist-get 'segments data)))
         last-end
         current)
		;; numbers at the end of a sentence sometimes don't end up with times
		;; so we need to fix them
    (while current
			(unless (alist-get 'start (car current)) ; start
				(set-cdr (assoc 1 'start (car current)) (1+ last-end)))
			(unless (alist-get 'end (car current)) ; start
				(set-cdr (assoc 1 'end (car current)) (1- (alist-get 'start (cadr current)))))
			(setq
			 last-end (alist-get 'end (car current))
			 current (cdr current)))
    base))

(defun subed-word-data--load (data)
  "Load word-level timing from DATA.
For now, only SRV2 files are supported."
  (setq-local subed-word-data--cache data)
  (add-hook 'subed-split-subtitle-timestamp-functions #'subed-word-data-split-at-word-timestamp -5 t)
  (subed-word-data-refresh-text-properties))

;;;###autoload
(defun subed-word-data-load-from-file (file)
  "Load word-level timing from FILE.
For now, only SRV2 and JSON files are supported."
  (interactive (list (read-file-name "JSON or srv2: "
                                     nil
                                     nil
                                     nil
                                     nil
                                     (lambda (f)
                                       (string-match
                                        "\\.json\\'\\|\\.srv2\\'"
                                        f)))))
  (subed-word-data--load
   (if (and (stringp file) (string-match "\\.json\\'" file))
       (subed-word-data--extract-words-from-whisperx-json file t)
     (subed-word-data--extract-words-from-srv2 (xml-parse-file file)))))

(defun subed-word-data-load-from-string (string)
  "Load word-level timing from STRING.
For now, only JSON or SRV2 files are supported."
  (subed-word-data--load (if (string-match "^{" string)
              (subed-word-data--extract-words-from-whisperx-json string)
              (subed-word-data--extract-words-from-srv2 string))))

(defvar subed-word-data-extensions '(".en.srv2" ".srv2") "Extensions to search for word data.")

;;;###autoload
(defun subed-word-data-load-maybe ()
  "Load word data if available. Suitable for adding to `subed-mode-hook'."
  (when (buffer-file-name)
    (let (file)
      (catch 'found
        (mapc (lambda (ext)
                (when (file-exists-p (concat (file-name-sans-extension (buffer-file-name)) ext))
                  (setq file (concat (file-name-sans-extension (buffer-file-name)) ext))
                  (throw 'found)))
              subed-word-data-extensions))
      (when file
        (subed-word-data-load-from-file file)
        (message "Word data loaded.")))))

(defvar subed-word-data-normalizing-functions '(subed-word-data-normalize-word-default)
  "Functions to run to normalize words before comparison.")

(defun subed-word-data-normalize-word-default (s)
  "Downcase S and remove non-alphanumeric characters for comparison."
  (replace-regexp-in-string "[^[:alnum:]]" "" (downcase s)))

(defun subed-word-data-normalize-word (word)
  "Normalize WORD to make it easier to compare."
  (mapc (lambda (func)
          (setq word (funcall func word)))
        subed-word-data-normalizing-functions)
  word)

(defun subed-word-data-compare-normalized-string= (word1 word2)
  "Compare two words and return t if they are the same after normalization."
  (string= (subed-word-data-normalize-word word1)
           (subed-word-data-normalize-word word2)))

(defvar subed-word-data-compare-function 'subed-word-data-compare-normalized-string=
  "Function to use to compare.")

(defun subed-word-data-compare (word1 word2)
  "Use the `subed-word-data-compare' function to compare WORD1 and WORD2.
Return non-nil if they are the same after normalization."
  (funcall subed-word-data-compare-function word1 word2))

(defun subed-word-data--look-up-word ()
  "Find the word timing that matches the one at point (approximately)."
  (save-excursion
    (skip-syntax-backward "w")
    (let* ((end (subed-subtitle-msecs-stop))
           (start (subed-subtitle-msecs-start))
           (remaining-words (split-string
                             (buffer-substring
                              (point)
                              (or (subed-jump-to-subtitle-end) (point)))))
           (words (if remaining-words
                      (reverse (seq-filter
                                (lambda (o)
                                  (and (or (not (alist-get 'end o)) (<= (alist-get 'end o) end))
                                       (or (not (alist-get 'start o)) (>= (alist-get 'start o) start))
                                       (not (string-match "^\n*$" (alist-get 'text o)))))
                                subed-word-data--cache))))
           (offset 0)
           (done (null remaining-words))
           candidate)
      (while (not done)
        (setq candidate (elt words (+ (1- (length remaining-words)) offset)))
        (cond
         ((and candidate (subed-word-data-compare
                          (car remaining-words)
                          (alist-get 'text candidate)))
          (setq done t))
         ((> offset (length words)) (setq done t))
         ((> offset 0) (setq offset (- offset)))
         (t (setq offset (1+ (- offset))))))
      candidate)))

(defun subed-word-data-split-at-word-timestamp ()
  "Return the starting timestamp if the word is found."
  (cond
   ((get-text-property (point) 'subed-word-start)
    (- time subed-subtitle-spacing))
   (subed-word-data--cache
    (let ((time (assoc-default 'start (subed-word-data--look-up-word))))
      (when time (- time subed-subtitle-spacing))))))

(defun subed-word-data-subtitle-entries ()
  "Return the entries that start and end within the current subtitle."
  (let ((start (subed-subtitle-msecs-start))
        (stop (+ (subed-subtitle-msecs-stop) subed-subtitle-spacing)))
    (seq-filter
     (lambda (o)
       (and (<= (or (alist-get 'end o) most-positive-fixnum) stop)
            (>= (or (alist-get 'start o) 0) start)
            (not (string-match "^\n*$" (alist-get 'text o)))))
     subed-word-data--cache)))

(defvar subed-word-data-threshold 5
  "Number of words to consider for matching.")
(defun subed-word-data-refresh-text-properties-for-subtitle ()
  "Refresh the text properties for the current subtitle."
  (interactive)
  (remove-text-properties (subed-jump-to-subtitle-text) (subed-jump-to-subtitle-end)
                          '(subed-word-data-start subed-word-data-end font-lock-face))
  (let* ((text-start (progn (subed-jump-to-subtitle-text) (point)))
         pos
         (word-data (reverse (subed-word-data-subtitle-entries)))
         candidate
         cand-count)
    (subed-jump-to-subtitle-end)
    (while (> (point) text-start)
      ;; Work our way backwards, matching against remaining words
      (setq pos (point))
      (backward-word)
      (let ((try-list word-data)
            candidate)
        (setq candidate (car try-list) cand-count 0)
        (setq try-list (cdr try-list))
        (while (and candidate
                    (< cand-count subed-word-data-threshold)
                    (not (subed-word-data-compare (buffer-substring (point) pos)
                                                  (alist-get 'text candidate))))
          (setq candidate (car try-list) cand-count (1+ cand-count))
          (when (> cand-count subed-word-data-threshold)
            (setq candidate nil))
          (setq try-list (cdr try-list)))
        (when (and candidate (subed-word-data-compare (buffer-substring (point) pos)
                                       (alist-get 'text candidate)))
          (subed-word-data--add-word-properties (point) pos candidate)
          (setq word-data try-list))))))

(defsubst subed-word-data--candidate-face (candidate)
  "Return the face to use for CANDIDATE."
  (if (and (alist-get 'score candidate)
           subed-word-data-confidence-faces)
      (cdr (seq-find (lambda (threshold) (>= (alist-get 'score candidate) (car threshold)))
                     subed-word-data-score-faces))
    subed-word-data-face))

(defsubst subed-word-data--add-word-properties (start end candidate)
  "Add properties from START to END for CANDIDATE."
  (let ((face (subed-word-data--candidate-face candidate)))
    (add-text-properties start end
                         (list 'subed-word-data-start
                               (assoc-default 'start candidate)
                               'subed-word-data-end
                               (assoc-default 'end candidate)
                               'subed-word-data-score
                               (assoc-default 'score candidate)
                               'font-lock-face face))
    (add-face-text-property start end face)))

(defun subed-word-data-refresh-text-properties ()
  "Add word data properties and face when available."
  (interactive)
  (save-excursion
    (remove-text-properties (point-min) (point-max) '(subed-word-data-start subed-word-data-end font-lock-face))
    (when subed-word-data--cache
      (goto-char (point-min))
      (unless (subed-jump-to-subtitle-id) (subed-forward-subtitle-id))
      (while (not (eobp))
        (let* ((text-start (progn (subed-jump-to-subtitle-text) (point)))
               pos
               (word-data (reverse (subed-word-data-subtitle-entries)))
               candidate)
          (subed-jump-to-subtitle-end)
          (while (> (point) text-start)
            ;; Work our way backwards, matching against remaining words
            (setq pos (point))
            (backward-word)
            (let ((try-list word-data)
                  candidate)
              (setq candidate (car try-list))
              (setq try-list (cdr try-list))
              (while (and candidate
                          (not (subed-word-data-compare (buffer-substring (point) pos)
                                         (alist-get 'text candidate))))
                (setq candidate (car try-list))
                (setq try-list (cdr try-list)))
              (when (and candidate (subed-word-data-compare (buffer-substring (point) pos)
                                             (alist-get 'text candidate)))
                ( subed-word-data--add-word-properties (point) pos candidate)
                (setq word-data try-list)))))
        (or (subed-forward-subtitle-id)
            (goto-char (point-max)))))))

(defun subed-word-data-pause-msecs ()
  "Return the number of milliseconds between this word and the previous word.
Requires the text properties to be set."
  (let ((current (get-text-property (point) 'subed-word-data-start)))
    (save-excursion
     (skip-syntax-backward "w")
     (backward-word)
     (when (get-text-property (point) 'subed-word-data-end)
       (- current (get-text-property (point) 'subed-word-data-end))))))

(defun subed-word-data-jump-to-longest-pause-in-current-subtitle ()
  "Jump to the word after the longest pause in the current subtitle.
Requires the text properties to be set."
  (interactive)
  (let ((start (or (subed-jump-to-subtitle-text) (point)))
        (end (or (subed-jump-to-subtitle-end) (point)))
        pos last-start-time pause (max-pause 0) max-pos)
    (backward-word)
    (setq max-pos (point))
    (while (> (point) start)
      (setq pos (point) last-start-time (get-text-property (point) 'subed-word-data-start))
      (backward-word)
      (if (get-text-property (point) 'subed-word-data-end)
          (progn
            (setq pause (and last-start-time (- last-start-time
                                                (get-text-property (point) 'subed-word-data-end))))
            (when (and pause (> pause max-pause))
              (setq max-pos pos
                    max-pause pause)))
        (setq last-start-time nil)))
    (goto-char max-pos)))
(provide 'subed-word-data)
;;; subed-word-data.el ends here
