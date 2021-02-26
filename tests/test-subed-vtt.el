;;;; Copyright 2020 Sacha Chua <sacha@sachachua.com>
;;;; Copyright 2020 Random User <rndusr@posteo.de>
;;;; Copyright 2021 Sebastian 'seabass' Crane <seabass-labrax@gmx.com>
;;;;
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(add-to-list 'load-path "./subed")
(require 'subed)

(defvar mock-vtt-data
  "WEBVTT

00:01:01.000 --> 00:01:05.123
Foo.

00:02:02.234 --> 00:02:10.345
Bar.

00:03:03.45 --> 00:03:15.5
Baz.
")

(defmacro with-temp-vtt-buffer (&rest body)
  "Call `subed-vtt--init' in temporary buffer before running BODY."
  `(with-temp-buffer
    (subed-vtt--init)
    (progn ,@body)))

(describe "VTT"
  (describe "Getting"
    (describe "the subtitle start/stop time"
      (it "returns the time in milliseconds."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:02:02.234")
         (expect (subed-vtt--subtitle-msecs-start) :to-equal (+ (* 2 60000) (* 2 1000) 234))
         (expect (subed-vtt--subtitle-msecs-stop) :to-equal (+ (* 2 60000) (* 10 1000) 345))))
      (it "handles lack of digits in milliseconds gracefully."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:03:03.45")
         (expect (save-excursion (subed-jump-to-subtitle-time-start)
                                 (thing-at-point 'line)) :to-equal "00:03:03.45 --> 00:03:15.5\n")
         (expect (subed-vtt--subtitle-msecs-start) :to-equal (+ (* 3 60 1000) (*  3 1000) 450))
         (expect (subed-vtt--subtitle-msecs-stop)  :to-equal (+ (* 3 60 1000) (* 15 1000) 500))))
      (it "returns nil if time can't be found."
        (with-temp-vtt-buffer
         (expect (subed-vtt--subtitle-msecs-start) :to-be nil)
         (expect (subed-vtt--subtitle-msecs-stop) :to-be nil)))
      )
    (describe "the subtitle text"
      (describe "when text is empty"
        (it "and at the beginning with a trailing newline."
          (with-temp-vtt-buffer
           (insert mock-vtt-data)
           (subed-vtt--jump-to-subtitle-text "00:01:01.000")
           (kill-line)
           (expect (subed-vtt--subtitle-text) :to-equal "")))
        (it "and at the beginning without a trailing newline."
          (with-temp-vtt-buffer
           (insert mock-vtt-data)
           (subed-vtt--jump-to-subtitle-text "00:01:01.000")
           (kill-whole-line)
           (expect (subed-vtt--subtitle-text) :to-equal "")))
        (it "and in the middle."
          (with-temp-vtt-buffer
           (insert mock-vtt-data)
           (subed-vtt--jump-to-subtitle-text "00:02:02.234")
           (kill-line)
           (expect (subed-vtt--subtitle-text) :to-equal "")))
        (it "and at the end with a trailing newline."
          (with-temp-vtt-buffer
           (insert mock-vtt-data)
           (subed-vtt--jump-to-subtitle-text "00:03:03.45")
           (kill-line)
           (expect (subed-vtt--subtitle-text) :to-equal "")))
        (it "and at the end without a trailing newline."
          (with-temp-vtt-buffer
           (insert mock-vtt-data)
           (subed-vtt--jump-to-subtitle-text "00:03:03.45")
           (kill-whole-line)
           (expect (subed-vtt--subtitle-text) :to-equal "")))
        )
      (describe "when text is not empty"
        (it "and has no linebreaks."
          (with-temp-vtt-buffer
           (insert mock-vtt-data)
           (subed-vtt--jump-to-subtitle-text "00:02:02.234")
           (expect (subed-vtt--subtitle-text) :to-equal "Bar.")))
        (it "and has linebreaks."
          (with-temp-vtt-buffer
           (insert mock-vtt-data)
           (subed-vtt--jump-to-subtitle-text "00:02:02.234")
           (insert "Bar.\n")
           (expect (subed-vtt--subtitle-text) :to-equal "Bar.\nBar.")))
        )
      )
    (describe "the point within the subtitle"
      (it "returns the relative point if we can find an ID."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:02:02.234")
         (expect (subed-vtt--subtitle-relative-point) :to-equal 0)
         (forward-line)
         (expect (subed-vtt--subtitle-relative-point) :to-equal 30)
         (forward-char)
         (expect (subed-vtt--subtitle-relative-point) :to-equal 31)
         (forward-line)
         (expect (subed-vtt--subtitle-relative-point) :to-equal 35)
         (forward-line)
         (expect (subed-vtt--subtitle-relative-point) :to-equal 0)))
      (it "returns nil if we can't find an ID."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:01:01.000")
         (insert "foo")
         (expect (subed-vtt--subtitle-relative-point) :to-equal nil)))
      )
    )

  (describe "Jumping"
    (describe "to current subtitle timestamp"
      (it "returns timestamp's point when point is already on the timestamp."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (goto-char (point-min))
         (subed-vtt--jump-to-subtitle-id "00:01:01.000")
         (expect (subed-vtt--jump-to-subtitle-time-start) :to-equal (point))
         (expect (looking-at subed-vtt--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:01:01.000")))
      (it "returns timestamp's point when point is on the text."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (search-backward "Baz.")
         (expect (thing-at-point 'word) :to-equal "Baz")
         (expect (subed-vtt--jump-to-subtitle-time-start) :to-equal 81)
         (expect (looking-at subed-vtt--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:03:03.45")))
      (it "returns timestamp's point when point is between subtitles."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (goto-char (point-min))
         (search-forward "Bar.\n")
         (expect (thing-at-point 'line) :to-equal "\n")
         (expect (subed-vtt--jump-to-subtitle-time-start) :to-equal 45)
         (expect (looking-at subed-vtt--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:02:02.234")))
      (it "returns nil if buffer is empty."
        (with-temp-vtt-buffer
         (expect (buffer-string) :to-equal "")
         (expect (subed-vtt--jump-to-subtitle-time-start) :to-equal nil)))
      (it "returns timestamp's point when buffer starts with blank lines."
        (with-temp-vtt-buffer
         (insert (concat "WEBVTT \n \t \n" (replace-regexp-in-string "WEBVTT" "" mock-vtt-data)))
         (search-backward "Foo.")
         (expect (thing-at-point 'line) :to-equal "Foo.\n")
         (expect (subed-vtt--jump-to-subtitle-time-start) :to-equal 15)
         (expect (looking-at subed-vtt--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:01:01.000")))
      (it "returns timestamp's point when subtitles are separated with blank lines."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (goto-char (point-min))
         (search-forward "Foo.\n")
         (insert " \n \t \n")
         (expect (subed-vtt--jump-to-subtitle-time-start) :to-equal 9)
         (expect (looking-at subed-vtt--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:01:01.000")))
      )
    (describe "to specific subtitle by timestamp"
      (it "returns timestamp's point if wanted time exists."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (goto-char (point-max))
         (expect (subed-vtt--jump-to-subtitle-id "00:02:02.234") :to-equal 45)
         (expect (looking-at (regexp-quote "00:02:02.234")) :to-be t)
         (expect (subed-vtt--jump-to-subtitle-id "00:01:01.000") :to-equal 9)
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)))
      (it "returns nil and does not move if wanted ID does not exists."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (goto-char (point-min))
         (search-forward "Foo")
         (let ((stored-point (point)))
           (expect (subed-vtt--jump-to-subtitle-id "0:08:00") :to-equal nil)
           (expect stored-point :to-equal (point)))))
      )
    (describe "to subtitle start time"
      (it "returns start time's point if movement was successful."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (goto-char (point-min))
         (expect (subed-vtt--jump-to-subtitle-time-start) :to-equal 9)
         (expect (looking-at subed-vtt--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:01:01.000")
         (re-search-forward "\n\n")
         (expect (subed-vtt--jump-to-subtitle-time-start) :to-equal 45)
         (expect (looking-at subed-vtt--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:02:02.234")
         (re-search-forward "\n\n")
         (expect (subed-vtt--jump-to-subtitle-time-start) :to-equal 81)
         (expect (looking-at subed-vtt--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:03:03.45")))
      (it "returns nil if movement failed."
        (with-temp-vtt-buffer
         (expect (subed-vtt--jump-to-subtitle-time-start) :to-equal nil)))
      )
    (describe "to subtitle stop time"
      (it "returns stop time's point if movement was successful."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (goto-char (point-min))
         (expect (subed-vtt--jump-to-subtitle-time-stop) :to-equal 26)
         (expect (looking-at subed-vtt--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:01:05.123")
         (re-search-forward "\n\n")
         (expect (subed-vtt--jump-to-subtitle-time-stop) :to-equal 62)
         (expect (looking-at subed-vtt--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:02:10.345")
         (re-search-forward "\n\n")
         (expect (subed-vtt--jump-to-subtitle-time-stop) :to-equal 97)
         (expect (looking-at subed-vtt--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:03:15.5")))
      (it "returns nil if movement failed."
        (with-temp-vtt-buffer
         (expect (subed-vtt--jump-to-subtitle-time-stop) :to-equal nil)))
      )
    (describe "to subtitle text"
      (it "returns subtitle text's point if movement was successful."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (goto-char (point-min))
         (expect (subed-vtt--jump-to-subtitle-text) :to-equal 39)
         (expect (point) :to-equal (save-excursion (goto-char (point-max)) (search-backward "Foo.")))
         (re-search-forward "\n\n")
         (expect (subed-vtt--jump-to-subtitle-text) :to-equal 75)
         (expect (point) :to-equal (save-excursion (goto-char (point-max)) (search-backward "Bar.")))
         (re-search-forward "\n\n")
         (expect (subed-vtt--jump-to-subtitle-text) :to-equal 108)
         (expect (point) :to-equal (save-excursion (goto-char (point-max)) (search-backward "Baz.")))))
      (it "returns nil if movement failed."
        (with-temp-vtt-buffer
         (expect (subed-vtt--jump-to-subtitle-time-stop) :to-equal nil)))
      )
    (describe "to end of subtitle text"
      (it "returns point if subtitle end can be found."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (goto-char (point-min))
         (expect (subed-vtt--jump-to-subtitle-end) :to-be 43)
         (expect (looking-back "^Foo.$") :to-be t)
         (forward-char 2)
         (expect (subed-vtt--jump-to-subtitle-end) :to-be 79)
         (expect (looking-back "^Bar.$") :to-be t)
         (forward-char 2)
         (expect (subed-vtt--jump-to-subtitle-end) :to-be 112)
         (expect (looking-back "^Baz.$") :to-be t)
         (goto-char (point-max))
         (backward-char 2)
         (expect (subed-vtt--jump-to-subtitle-end) :to-be 112)
         (expect (looking-back "^Baz.$") :to-be t)))
      (it "returns nil if subtitle end cannot be found."
        (with-temp-vtt-buffer
         (expect (subed-vtt--jump-to-subtitle-end) :to-be nil)))
      (it "returns nil if point did not move."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-text "00:01:01.000")
         (kill-line)
         (expect (subed-vtt--jump-to-subtitle-end) :to-be nil)))
      (it "works if text is empty with trailing newline."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-text "00:01:01.000")
         (kill-line)
         (backward-char)
         (expect (subed-vtt--jump-to-subtitle-end) :to-be 39)
         (expect (looking-at "^$") :to-be t)
         (subed-vtt--jump-to-subtitle-text "00:02:02.234")
         (kill-line)
         (backward-char)
         (expect (subed-vtt--jump-to-subtitle-end) :to-be 71)
         (expect (looking-at "^$") :to-be t)
         (subed-vtt--jump-to-subtitle-text "00:03:03.45")
         (kill-line)
         (backward-char)
         (expect (subed-vtt--jump-to-subtitle-end) :to-be 100)
         (expect (looking-at "^$") :to-be t)))
      (it "works if text is empty without trailing newline."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-text "00:01:01.000")
         (kill-whole-line)
         (expect (subed-vtt--jump-to-subtitle-end) :to-be nil)
         (expect (looking-at "^$") :to-be t)
         (goto-char (point-min))
         (expect (subed-vtt--jump-to-subtitle-end) :to-be 39)
         (expect (looking-at "^$") :to-be t)
         (subed-vtt--jump-to-subtitle-text "00:02:02.234")
         (kill-whole-line)
         (expect (subed-vtt--jump-to-subtitle-end) :to-be nil)
         (expect (looking-at "^$") :to-be t)
         (backward-char)
         (expect (subed-vtt--jump-to-subtitle-end) :to-be 70)
         (expect (looking-at "^$") :to-be t)
         (subed-vtt--jump-to-subtitle-text "00:03:03.45")
         (kill-whole-line)
         (expect (subed-vtt--jump-to-subtitle-end) :to-be nil)
         (expect (looking-at "^$") :to-be t)
         (backward-char)
         (expect (subed-vtt--jump-to-subtitle-end) :to-be 98)
         (expect (looking-at "^$") :to-be t)))
      )
    (describe "to next subtitle ID"
      (it "returns point when there is a next subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:01:01.000")
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)
         (expect (subed-vtt--forward-subtitle-id) :to-be 45)
         (expect (looking-at (regexp-quote "00:02:02.234")) :to-be t)
         (subed-vtt--jump-to-subtitle-time-start "00:02:02.234")
         (expect (looking-at (regexp-quote "00:02:02.234")) :to-be t)
         (expect (subed-vtt--forward-subtitle-id) :to-be 81)
         (expect (looking-at (regexp-quote "00:03:03.45")) :to-be t)))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-vtt-buffer
         (expect (thing-at-point 'word) :to-equal nil)
         (expect (subed-vtt--forward-subtitle-id) :to-be nil))
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-text "00:01:01.000")
         (expect (thing-at-point 'word) :to-equal "Foo")
         (expect (subed-vtt--forward-subtitle-id) :to-be 45)
         (expect (looking-at (regexp-quote "00:02:02.234")) :to-be t)
         (subed-vtt--jump-to-subtitle-time-stop "00:02:02.234")
         (expect (looking-at (regexp-quote "00:02:10.345")) :to-be t)
         (expect (subed-vtt--forward-subtitle-id) :to-be 81)
         (expect (looking-at (regexp-quote "00:03:03.45")) :to-be t))
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-text "00:03:03.45")
         (expect (thing-at-point 'word) :to-equal "Baz")
         (expect (subed-vtt--forward-subtitle-id) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "Baz"))
        (with-temp-vtt-buffer
         (insert (concat mock-vtt-data "\n\n"))
         (subed-vtt--jump-to-subtitle-time-stop "00:03:03.45")
         (expect (thing-at-point 'word) :to-equal "00")
         (expect (subed-vtt--forward-subtitle-id) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "00")))
      )
    (describe "to previous subtitle ID"
      (it "returns point when there is a previous subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-text "00:02:02.234")
         (expect (thing-at-point 'word) :to-equal "Bar")
         (expect (subed-vtt--backward-subtitle-id) :to-be 9)
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)
         (subed-vtt--jump-to-subtitle-time-stop "00:03:03.45")
         (expect (looking-at (regexp-quote "00:03:15.5")) :to-be t)
         (expect (subed-vtt--backward-subtitle-id) :to-be 45)
         (expect (looking-at (regexp-quote "00:02:02.234")) :to-be t)))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-vtt-buffer
         (expect (subed-vtt--backward-subtitle-id) :to-be nil))
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:01:01.000")
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)
         (expect (subed-vtt--backward-subtitle-id) :to-be nil)
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t))
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-text "00:01:01.000")
         (expect (thing-at-point 'word) :to-equal "Foo")
         (expect (subed-vtt--backward-subtitle-id) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "Foo"))
        (with-temp-vtt-buffer
         (insert (concat "\n\n\n" mock-vtt-data))
         (subed-vtt--jump-to-subtitle-time-stop "00:01:01.000")
         (expect (thing-at-point 'word) :to-equal "00")
         (expect (subed-vtt--backward-subtitle-id) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "00")))
      )
    (describe "to next subtitle text"
      (it "returns point when there is a next subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:01:01.000")
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)
         (expect (subed-vtt--forward-subtitle-text) :to-be 75)
         (expect (thing-at-point 'word) :to-equal "Bar")))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-vtt-buffer
         (goto-char (point-max))
         (insert (concat mock-vtt-data "\n\n"))
         (subed-vtt--jump-to-subtitle-id "00:03:03.45")
         (expect (looking-at (regexp-quote "00:03:03.45")) :to-be t)
         (expect (subed-vtt--forward-subtitle-text) :to-be nil)
         (expect (looking-at (regexp-quote "00:03:03.45")) :to-be t)))
      )
    (describe "to previous subtitle text"
      (it "returns point when there is a previous subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:03:03.45")
         (expect (looking-at (regexp-quote "00:03:03.45")) :to-be t)
         (expect (subed-vtt--backward-subtitle-text) :to-be 75)
         (expect (thing-at-point 'word) :to-equal "Bar")))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (goto-char (point-min))
         (subed-vtt--forward-subtitle-time-start)
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)
         (expect (subed-vtt--backward-subtitle-text) :to-be nil)
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)))
      )
    (describe "to next subtitle end"
      (it "returns point when there is a next subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-text "00:02:02.234")
         (expect (thing-at-point 'word) :to-equal "Bar")
         (expect (subed-vtt--forward-subtitle-end) :to-be 112)
         (expect (thing-at-point 'word) :to-equal nil)))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-vtt-buffer
         (insert (concat mock-vtt-data "\n\n"))
         (subed-vtt--jump-to-subtitle-text "00:03:03.45")
         (end-of-line)
         (expect (thing-at-point 'word) :to-equal nil)
         (expect (subed-vtt--forward-subtitle-end) :to-be nil)
         (expect (thing-at-point 'word) :to-equal nil)))
      )
    (describe "to previous subtitle end"
      (it "returns point when there is a previous subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:03:03.45")
         (expect (looking-at (regexp-quote "00:03:03.45")) :to-be t)
         (expect (subed-vtt--backward-subtitle-text) :to-be 75)
         (expect (thing-at-point 'word) :to-equal "Bar")))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (goto-char (point-min))
         (subed-vtt--forward-subtitle-id)
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)
         (expect (subed-vtt--backward-subtitle-text) :to-be nil)
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)))
      )
    (describe "to next subtitle start time"
      (it "returns point when there is a next subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-text "00:01:01.000")
         (expect (thing-at-point 'word) :to-equal "Foo")
         (expect (subed-vtt--forward-subtitle-time-start) :to-be 45)
         (expect (looking-at (regexp-quote "00:02:02.234")) :to-be t)))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:03:03.45")
         (expect (looking-at (regexp-quote "00:03:03.45")) :to-be t)
         (expect (subed-vtt--forward-subtitle-time-start) :to-be nil)
         (expect (looking-at (regexp-quote "00:03:03.45")) :to-be t)))
      )
    (describe "to previous subtitle start time"
      (it "returns point when there is a previous subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:02:02.234")
         (expect (looking-at (regexp-quote "00:02:02.234")) :to-be t)
         (expect (subed-vtt--backward-subtitle-time-start) :to-be 9)
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:01:01.000")
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)
         (expect (subed-vtt--backward-subtitle-time-start) :to-be nil)
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)))
      )
    (describe "to next subtitle stop time"
      (it "returns point when there is a next subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-text "00:01:01.000")
         (expect (thing-at-point 'word) :to-equal "Foo")
         (expect (subed-vtt--forward-subtitle-time-stop) :to-be 62)
         (expect (looking-at (regexp-quote "00:02:10.345")) :to-be t)))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:03:03.45")
         (expect (looking-at (regexp-quote "00:03:03.45")) :to-be t)
         (expect (subed-vtt--forward-subtitle-time-stop) :to-be nil)
         (expect (looking-at (regexp-quote "00:03:03.45")) :to-be t)))
      )
    (describe "to previous subtitle stop time"
      (it "returns point when there is a previous subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:03:03.45")
         (expect (looking-at (regexp-quote "00:03:03.45")) :to-be t)
         (expect (subed-vtt--backward-subtitle-time-stop) :to-be 62)
         (expect (looking-at (regexp-quote "00:02:10.345")) :to-be t)))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-vtt--jump-to-subtitle-id "00:01:01.000")
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)
         (expect (subed-vtt--backward-subtitle-time-stop) :to-be nil)
         (expect (looking-at (regexp-quote "00:01:01.000")) :to-be t)))
      )
    )

  (describe "Setting start/stop time"
    (it "of current subtitle."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (subed-vtt--jump-to-subtitle-end "00:02:02.234")
       (subed-vtt--set-subtitle-time-start (+ (* 1 60 60 1000) (* 2 60 1000) (* 3 1000) 400))
       (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                 "00:01:01.000 --> 00:01:05.123\n"
                                                 "Foo.\n\n"
                                                 "01:02:03.400 --> 00:02:10.345\n"
                                                 "Bar.\n\n"
                                                 "00:03:03.45 --> 00:03:15.5\n"
                                                 "Baz.\n"))
       (subed-vtt--set-subtitle-time-stop (+ (* 5 60 60 1000) (* 6 60 1000) (* 7 1000) 800))
       (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                 "00:01:01.000 --> 00:01:05.123\n"
                                                 "Foo.\n\n"
                                                 "01:02:03.400 --> 05:06:07.800\n"
                                                 "Bar.\n\n"
                                                 "00:03:03.45 --> 00:03:15.5\n"
                                                 "Baz.\n"))))
    (it "of specific subtitle."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (subed-vtt--jump-to-subtitle-time-stop "00:01:01.000")
       (subed-vtt--set-subtitle-time-start (+ (* 2 60 60 1000) (* 4 60 1000) (* 6 1000) 800) 1)
       (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                 "02:04:06.800 --> 00:01:05.123\n"
                                                 "Foo.\n\n"
                                                 "00:02:02.234 --> 00:02:10.345\n"
                                                 "Bar.\n\n"
                                                 "00:03:03.45 --> 00:03:15.5\n"
                                                 "Baz.\n"))
       (subed-vtt--jump-to-subtitle-text "00:03:03.45")
       (subed-vtt--set-subtitle-time-stop (+ (* 3 60 60 1000) (* 5 60 1000) (* 7 1000) 900) 3)
       (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                 "02:04:06.800 --> 00:01:05.123\n"
                                                 "Foo.\n\n"
                                                 "00:02:02.234 --> 00:02:10.345\n"
                                                 "Bar.\n\n"
                                                 "00:03:03.45 --> 03:05:07.900\n"
                                                 "Baz.\n"))))
    (it "when milliseconds lack digits."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (subed-jump-to-subtitle-id "00:03:03.45")
       (subed-vtt--set-subtitle-time-start (+ (* 1 60 60 1000) (* 2 60 1000) (* 3 1000) 4) 3)
       (expect (save-excursion (subed-jump-to-subtitle-time-start)
                               (thing-at-point 'line)) :to-equal "01:02:03.004 --> 00:03:15.5\n")
       (subed-vtt--set-subtitle-time-stop (+ (* 2 60 60 1000) (* 3 60 1000) (* 4 1000) 60) 3)
       (expect (save-excursion (subed-jump-to-subtitle-time-start)
                               (thing-at-point 'line)) :to-equal "01:02:03.004 --> 02:03:04.060\n")))
    )

  (describe "Inserting a subtitle"
    (describe "in an empty buffer"
      (describe "before"
        (it "passing nothing."
          (with-temp-vtt-buffer
           (expect (subed-vtt--prepend-subtitle) :to-equal 31)
           (expect (buffer-string) :to-equal (concat "00:00:00.000 --> 00:00:01.000\n\n"))
           (expect (point) :to-equal 31)))
        (it "passing start time."
          (with-temp-vtt-buffer
           (expect (subed-vtt--prepend-subtitle nil 60000) :to-equal 31)
           (expect (buffer-string) :to-equal (concat "00:01:00.000 --> 00:01:01.000\n\n"))
           (expect (point) :to-equal 31)))
        (it "passing start time and stop time."
          (with-temp-vtt-buffer
           (expect (subed-vtt--prepend-subtitle nil 60000 65000) :to-equal 31)
           (expect (buffer-string) :to-equal (concat "00:01:00.000 --> 00:01:05.000\n\n"))
           (expect (point) :to-equal 31)))
        (it "passing start time, stop time and text."
          (with-temp-vtt-buffer
           (expect (subed-vtt--prepend-subtitle nil 60000 65000 "Foo. bar\nbaz.") :to-equal 31)
           (expect (buffer-string) :to-equal (concat "00:01:00.000 --> 00:01:05.000\n"
                                                     "Foo. bar\nbaz.\n"))
           (expect (point) :to-equal 31)))
        )
      (describe "after"
        (it "passing nothing."
          (with-temp-vtt-buffer
           (expect (subed-vtt--append-subtitle) :to-equal 31)
           (expect (buffer-string) :to-equal (concat "00:00:00.000 --> 00:00:01.000\n\n"))
           (expect (point) :to-equal 31)))
        (it "passing start time."
          (with-temp-vtt-buffer
           (expect (subed-vtt--append-subtitle nil 60000) :to-equal 31)
           (expect (buffer-string) :to-equal (concat "00:01:00.000 --> 00:01:01.000\n\n"))
           (expect (point) :to-equal 31)))
        (it "passing start time and stop time."
          (with-temp-vtt-buffer
           (expect (subed-vtt--append-subtitle nil 60000 65000) :to-equal 31)
           (expect (buffer-string) :to-equal (concat "00:01:00.000 --> 00:01:05.000\n\n"))
           (expect (point) :to-equal 31)))
        (it "passing start time, stop time and text."
          (with-temp-vtt-buffer
           (expect (subed-vtt--append-subtitle nil 60000 65000 "Foo, bar\nbaz.") :to-equal 31)
           (expect (buffer-string) :to-equal (concat "00:01:00.000 --> 00:01:05.000\n"
                                                     "Foo, bar\nbaz.\n"))
           (expect (point) :to-equal 31)))
        )
      )
    (describe "in a non-empty buffer"
      (describe "before the current subtitle"
        (describe "with point on the first subtitle"
          (it "passing nothing."
            (with-temp-vtt-buffer
             (insert (concat "00:00:05.000 --> 00:00:06.000\n"
                             "Foo.\n"))
             (subed-vtt--jump-to-subtitle-time-stop)
             (expect (subed-vtt--prepend-subtitle) :to-equal 31)
             (expect (buffer-string) :to-equal (concat "00:00:00.000 --> 00:00:01.000\n"
                                                       "\n\n"
                                                       "00:00:05.000 --> 00:00:06.000\n"
                                                       "Foo.\n"))
             (expect (point) :to-equal 31)))
          (it "passing start time."
            (with-temp-vtt-buffer
             (insert (concat "00:00:05.000 --> 00:00:06.000\n"
                             "Foo.\n"))
             (subed-vtt--jump-to-subtitle-time-stop)
             (expect (subed-vtt--prepend-subtitle nil 1500) :to-equal 31)
             (expect (buffer-string) :to-equal (concat "00:00:01.500 --> 00:00:02.500\n"
                                                       "\n\n"
                                                       "00:00:05.000 --> 00:00:06.000\n"
                                                       "Foo.\n"))
             (expect (point) :to-equal 31)))
          (it "passing start time and stop time."
            (with-temp-vtt-buffer
             (insert (concat "00:00:05.000 --> 00:00:06.000\n"
                             "Foo.\n"))
             (subed-vtt--jump-to-subtitle-time-stop)
             (expect (subed-vtt--prepend-subtitle nil 1500 2000) :to-equal 31)
             (expect (buffer-string) :to-equal (concat "00:00:01.500 --> 00:00:02.000\n"
                                                       "\n\n"
                                                       "00:00:05.000 --> 00:00:06.000\n"
                                                       "Foo.\n"))
             (expect (point) :to-equal 31)))
          (it "passing start time, stop time and text."
            (with-temp-vtt-buffer
             (insert (concat "00:00:05.000 --> 00:00:06.000\n"
                             "Foo.\n"))
             (subed-vtt--jump-to-subtitle-time-stop)
             (expect (subed-vtt--prepend-subtitle nil 1500 3000 "Bar.") :to-equal 31)
             (expect (buffer-string) :to-equal (concat "00:00:01.500 --> 00:00:03.000\n"
                                                       "Bar.\n\n"
                                                       "00:00:05.000 --> 00:00:06.000\n"
                                                       "Foo.\n"))
             (expect (point) :to-equal 31)))
          )
        (describe "with point on a non-first subtitle"
          (it "passing nothing."
            (with-temp-vtt-buffer
             (insert (concat "00:00:05.000 --> 00:00:06.000\n"
                             "Foo.\n\n"
                             "00:00:10.000 --> 00:00:12.000\n"
                             "Bar.\n"))
             (subed-vtt--jump-to-subtitle-text "00:00:10.000")
             (expect (subed-vtt--prepend-subtitle) :to-equal 67)
             (expect (buffer-string) :to-equal (concat "00:00:05.000 --> 00:00:06.000\n"
                                                       "Foo.\n\n"
                                                       "00:00:00.000 --> 00:00:01.000\n"
                                                       "\n\n"
                                                       "00:00:10.000 --> 00:00:12.000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 67)))
          (it "passing start time."
            (with-temp-vtt-buffer
             (insert (concat "00:00:05.000 --> 00:00:06.000\n"
                             "Foo.\n\n"
                             "00:00:10.000 --> 00:00:12.000\n"
                             "Bar.\n"))
             (subed-vtt--jump-to-subtitle-text "00:00:10.000")
             (expect (subed-vtt--prepend-subtitle nil 7000) :to-equal 67)
             (expect (buffer-string) :to-equal (concat "00:00:05.000 --> 00:00:06.000\n"
                                                       "Foo.\n\n"
                                                       "00:00:07.000 --> 00:00:08.000\n"
                                                       "\n\n"
                                                       "00:00:10.000 --> 00:00:12.000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 67)))
          (it "passing start time and stop time."
            (with-temp-vtt-buffer
             (insert (concat "00:00:05.000 --> 00:00:06.000\n"
                             "Foo.\n\n"
                             "00:00:10.000 --> 00:00:12.000\n"
                             "Bar.\n"))
             (subed-vtt--jump-to-subtitle-text "00:00:10.000")
             (expect (subed-vtt--prepend-subtitle nil 7000 7123) :to-equal 67)
             (expect (buffer-string) :to-equal (concat "00:00:05.000 --> 00:00:06.000\n"
                                                       "Foo.\n\n"
                                                       "00:00:07.000 --> 00:00:07.123\n"
                                                       "\n\n"
                                                       "00:00:10.000 --> 00:00:12.000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 67)))
          (it "passing start time, stop time and text."
            (with-temp-vtt-buffer
             (insert (concat "00:00:05.000 --> 00:00:06.000\n"
                             "Foo.\n\n"
                             "00:00:10.000 --> 00:00:12.000\n"
                             "Bar.\n"))
             (subed-vtt--jump-to-subtitle-text "00:00:10.000")
             (expect (subed-vtt--prepend-subtitle nil 7000 7123 "Baz.") :to-equal 67)
             (expect (buffer-string) :to-equal (concat "00:00:05.000 --> 00:00:06.000\n"
                                                       "Foo.\n\n"
                                                       "00:00:07.000 --> 00:00:07.123\n"
                                                       "Baz.\n\n"
                                                       "00:00:10.000 --> 00:00:12.000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 67)))
          )
        )
      (describe "after the current subtitle"
        (describe "with point on the last subtitle"
          (it "passing nothing."
            (with-temp-vtt-buffer
             (insert (concat "00:00:05.000 --> 00:00:06.000\n"
                             "Foo.\n"))
             (subed-vtt--jump-to-subtitle-text)
             (expect (subed-vtt--append-subtitle) :to-equal 67)
             (expect (buffer-string) :to-equal (concat "00:00:05.000 --> 00:00:06.000\n"
                                                       "Foo.\n\n"
                                                       "00:00:00.000 --> 00:00:01.000\n"
                                                       "\n"))
             (expect (point) :to-equal 67)))
          (it "passing start time."
            (with-temp-vtt-buffer
             (insert (concat "00:00:05.000 --> 00:00:06.000\n"
                             "Foo.\n"))
             (subed-vtt--jump-to-subtitle-text)
             (expect (subed-vtt--append-subtitle nil 12345) :to-equal 67)
             (expect (buffer-string) :to-equal (concat "00:00:05.000 --> 00:00:06.000\n"
                                                       "Foo.\n\n"
                                                       "00:00:12.345 --> 00:00:13.345\n"
                                                       "\n"))
             (expect (point) :to-equal 67)))
          (it "passing start time and stop time."
            (with-temp-vtt-buffer
             (insert (concat "00:00:05.000 --> 00:00:06.000\n"
                             "Foo.\n"))
             (subed-vtt--jump-to-subtitle-text)
             (expect (subed-vtt--append-subtitle nil 12345 15000) :to-equal 67)
             (expect (buffer-string) :to-equal (concat "00:00:05.000 --> 00:00:06.000\n"
                                                       "Foo.\n\n"
                                                       "00:00:12.345 --> 00:00:15.000\n"
                                                       "\n"))
             (expect (point) :to-equal 67)))
          (it "passing start time, stop time and text."
            (with-temp-vtt-buffer
             (insert (concat "00:00:05.000 --> 00:00:06.000\n"
                             "Foo.\n"))
             (subed-vtt--jump-to-subtitle-text)
             (expect (subed-vtt--append-subtitle nil 12345 15000 "Bar.") :to-equal 67)
             (expect (buffer-string) :to-equal (concat "00:00:05.000 --> 00:00:06.000\n"
                                                       "Foo.\n\n"
                                                       "00:00:12.345 --> 00:00:15.000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 67)))
          )
        (describe "with point on a non-last subtitle"
          (it "passing nothing."
            (with-temp-vtt-buffer
             (insert (concat "00:00:01.000 --> 00:00:02.000\n"
                             "Foo.\n\n"
                             "00:00:05.000 --> 00:00:06.000\n"
                             "Bar.\n"))
             (subed-vtt--jump-to-subtitle-time-start "00:00:01.000")
             (expect (subed-vtt--append-subtitle) :to-equal 67)
             (expect (buffer-string) :to-equal (concat "00:00:01.000 --> 00:00:02.000\n"
                                                       "Foo.\n\n"
                                                       "00:00:00.000 --> 00:00:01.000\n"
                                                       "\n\n"
                                                       "00:00:05.000 --> 00:00:06.000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 67)))
          (it "passing start time."
            (with-temp-vtt-buffer
             (insert (concat "00:00:01.000 --> 00:00:02.000\n"
                             "Foo.\n\n"
                             "00:00:05.000 --> 00:00:06.000\n"
                             "Bar.\n"))
             (subed-vtt--jump-to-subtitle-time-start "00:00:01.000")
             (expect (subed-vtt--append-subtitle nil 2500) :to-equal 67)
             (expect (buffer-string) :to-equal (concat "00:00:01.000 --> 00:00:02.000\n"
                                                       "Foo.\n\n"
                                                       "00:00:02.500 --> 00:00:03.500\n"
                                                       "\n\n"
                                                       "00:00:05.000 --> 00:00:06.000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 67)))
          (it "passing start time and stop time."
            (with-temp-vtt-buffer
             (insert (concat "00:00:01.000 --> 00:00:02.000\n"
                             "Foo.\n\n"
                             "00:00:05.000 --> 00:00:06.000\n"
                             "Bar.\n"))
             (subed-vtt--jump-to-subtitle-time-start "00:00:01.000")
             (expect (subed-vtt--append-subtitle nil 2500 4000) :to-equal 67)
             (expect (buffer-string) :to-equal (concat "00:00:01.000 --> 00:00:02.000\n"
                                                       "Foo.\n\n"
                                                       "00:00:02.500 --> 00:00:04.000\n"
                                                       "\n\n"
                                                       "00:00:05.000 --> 00:00:06.000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 67)))
          (it "passing start time, stop time and text."
            (with-temp-vtt-buffer
             (insert (concat "00:00:01.000 --> 00:00:02.000\n"
                             "Foo.\n\n"
                             "00:00:05.000 --> 00:00:06.000\n"
                             "Bar.\n"))
             (subed-vtt--jump-to-subtitle-time-start "00:00:01.000")
             (expect (subed-vtt--append-subtitle nil 2500 4000 "Baz.") :to-equal 67)
             (expect (buffer-string) :to-equal (concat "00:00:01.000 --> 00:00:02.000\n"
                                                       "Foo.\n\n"
                                                       "00:00:02.500 --> 00:00:04.000\n"
                                                       "Baz.\n\n"
                                                       "00:00:05.000 --> 00:00:06.000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 67)))
          )
        )
      (it "when point is on empty text."
        (with-temp-vtt-buffer
         (insert (concat "00:00:01.000 --> 00:00:02.000\n"
                         "\n"))
         (subed-jump-to-subtitle-text)
         (expect (subed-vtt--append-subtitle) :to-equal 63)
         (expect (buffer-string) :to-equal (concat "00:00:01.000 --> 00:00:02.000\n"
                                                   "\n\n"
                                                   "00:00:00.000 --> 00:00:01.000\n"
                                                   "\n"))
         (expect (point) :to-equal 63)))
      )
    )

  (describe "Killing a subtitle"
    (it "removes the first subtitle."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (subed-vtt--jump-to-subtitle-text "00:01:01.000")
       (subed-vtt--kill-subtitle)
       (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                 "00:02:02.234 --> 00:02:10.345\n"
                                                 "Bar.\n\n"
                                                 "00:03:03.45 --> 00:03:15.5\n"
                                                 "Baz.\n"))))
    (it "removes it in between."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (subed-vtt--jump-to-subtitle-text "00:02:02.234")
       (subed-vtt--kill-subtitle)
       (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                 "00:01:01.000 --> 00:01:05.123\n"
                                                 "Foo.\n\n"
                                                 "00:03:03.45 --> 00:03:15.5\n"
                                                 "Baz.\n"))))
    (it "removes the last subtitle."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (subed-vtt--jump-to-subtitle-text "00:03:03.45")
       (subed-vtt--kill-subtitle)
       (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                 "00:01:01.000 --> 00:01:05.123\n"
                                                 "Foo.\n\n"
                                                 "00:02:02.234 --> 00:02:10.345\n"
                                                 "Bar.\n"))))
    (describe "removes the previous subtitle when point is right above the timestamp"
      (it "of the last subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-jump-to-subtitle-id "00:03:03.45")
         (backward-char)
         (expect (looking-at "^\n00:03:03.45") :to-be t)
         (subed-vtt--kill-subtitle)
         (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                   "00:01:01.000 --> 00:01:05.123\n"
                                                   "Foo.\n\n"
                                                   "00:03:03.45 --> 00:03:15.5\n"
                                                   "Baz.\n"))))
      (it "of a non-last subtitle."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (subed-jump-to-subtitle-id "00:02:02.234")
         (backward-char)
         (expect (looking-at "^\n00:02:02.234") :to-be t)
         (subed-vtt--kill-subtitle)
         (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                   "00:02:02.234 --> 00:02:10.345\n"
                                                   "Bar.\n\n"
                                                   "00:03:03.45 --> 00:03:15.5\n"
                                                   "Baz.\n"))))
      )
    )

  (describe "Validating"
    (it "works in empty buffer."
      (with-temp-vtt-buffer
       (subed-vtt--validate)))
    (it "works in buffer that contains only newlines."
      (with-temp-vtt-buffer
       (cl-loop for _ from 1 to 10 do
                (insert "\n")
                (subed-vtt--validate))))
    (it "works in buffer that contains only spaces."
      (with-temp-vtt-buffer
       (cl-loop for _ from 1 to 10 do
                (insert " ")
                (subed-vtt--validate))))
    (it "works in buffer that contains only spaces and newlines."
      (with-temp-vtt-buffer
       (cl-loop for _ from 1 to 10 do
                (if (eq (random 2) 0)
                    (insert " ")
                  (insert "\n"))
                (subed-vtt--validate))))
    (it "reports invalid stop time."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (subed-vtt--jump-to-subtitle-time-stop "00:01:01.000")
       (forward-char 10)
       (insert "3")
       (expect (subed-vtt--validate) :to-throw
               'error '("Found invalid stop time: \"00:01:01.000 --> 00:01:05.1323\""))
       (expect (point) :to-equal 26)))
    (it "reports invalid time separator."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (subed-vtt--jump-to-subtitle-time-stop "00:01:01.000")
       (delete-char -1)
       (expect (subed-vtt--validate) :to-throw
               'error '("Found invalid separator between start and stop time: \"00:01:01.000 -->00:01:05.123\""))
       (expect (point) :to-equal 21)))
    (it "does not report error when last subtitle text is empty."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (subed-vtt--jump-to-subtitle-text "00:03:03.45")
       (kill-whole-line)
       (forward-char -2)
       (subed-vtt--validate)
       (expect (point) :to-equal 106)))
    (it "preserves point if there is no error."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (subed-vtt--jump-to-subtitle-text "00:02:02.234")
       (forward-char 2)
       (subed-vtt--validate)
       (expect (point) :to-equal 77)))
    )

  (describe "Sanitizing"
    (it "removes trailing tabs and spaces from all lines."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (goto-char (point-min))
       (while (re-search-forward "\n" nil t)
         (replace-match " \n"))
       (expect (buffer-string) :not :to-equal mock-vtt-data)
       (subed-vtt--sanitize)
       (expect (buffer-string) :to-equal mock-vtt-data))
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (goto-char (point-min))
       (while (re-search-forward "\n" nil t)
         (replace-match "\t\n"))
       (expect (buffer-string) :not :to-equal mock-vtt-data)
       (subed-vtt--sanitize)
       (expect (buffer-string) :to-equal mock-vtt-data)))
    (it "removes leading tabs and spaces from all lines."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (goto-char (point-min))
       (while (re-search-forward "\n" nil t)
         (replace-match "\n "))
       (expect (buffer-string) :not :to-equal mock-vtt-data)
       (subed-vtt--sanitize)
       (expect (buffer-string) :to-equal mock-vtt-data))
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (goto-char (point-min))
       (while (re-search-forward "\n" nil t)
         (replace-match "\n\t"))
       (expect (buffer-string) :not :to-equal mock-vtt-data)
       (subed-vtt--sanitize)
       (expect (buffer-string) :to-equal mock-vtt-data)))
    (it "removes excessive empty lines between subtitles."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (goto-char (point-min))
       (forward-line 2)
       (while (re-search-forward "\n\n" nil t)
         (replace-match "\n \n  \t  \t\t  \n\n  \t\n"))
       (expect (buffer-string) :not :to-equal mock-vtt-data)
       (subed-vtt--sanitize)
       (expect (buffer-string) :to-equal mock-vtt-data)))
    (it "ensures double newline between subtitles if text of previous subtitle is empty."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (subed-vtt--jump-to-subtitle-text "00:01:01.000")
       (kill-whole-line)
       (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                 "00:01:01.000 --> 00:01:05.123\n"
                                                 "\n"
                                                 "00:02:02.234 --> 00:02:10.345\n"
                                                 "Bar.\n\n"
                                                 "00:03:03.45 --> 00:03:15.5\n"
                                                 "Baz.\n"))
       (subed-vtt--sanitize)
       (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                 "00:01:01.000 --> 00:01:05.123\n"
                                                 "\n\n"
                                                 "00:02:02.234 --> 00:02:10.345\n"
                                                 "Bar.\n\n"
                                                 "00:03:03.45 --> 00:03:15.5\n"
                                                 "Baz.\n"))))
    (it "removes empty lines from end of buffer."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (goto-char (point-max))
       (insert " \n\t\n\n")
       (expect (buffer-string) :not :to-equal mock-vtt-data)
       (subed-vtt--sanitize)
       (expect (buffer-string) :to-equal mock-vtt-data)))
    (it "ensures a single newline after the last subtitle."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (goto-char (point-max))
       (while (eq (char-before (point-max)) ?\n)
         (delete-backward-char 1))
       (expect (buffer-string) :not :to-equal mock-vtt-data)
       (subed-vtt--sanitize)
       (expect (buffer-string) :to-equal mock-vtt-data)))
    (it "ensures single newline after last subtitle if text is empty."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (subed-vtt--jump-to-subtitle-text "00:03:03.45")
       (kill-whole-line)
       (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                 "00:01:01.000 --> 00:01:05.123\n"
                                                 "Foo.\n\n"
                                                 "00:02:02.234 --> 00:02:10.345\n"
                                                 "Bar.\n\n"
                                                 "00:03:03.45 --> 00:03:15.5\n"
                                                 ""))
       (subed-vtt--sanitize)
       (expect (buffer-string) :to-equal (concat "WEBVTT\n\n"
                                                 "00:01:01.000 --> 00:01:05.123\n"
                                                 "Foo.\n\n"
                                                 "00:02:02.234 --> 00:02:10.345\n"
                                                 "Bar.\n\n"
                                                 "00:03:03.45 --> 00:03:15.5\n"
                                                 "\n"))))
    (it "ensures single space before and after time separators."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (goto-char (point-min))
       (re-search-forward " --> ")
       (replace-match "  --> ")
       (re-search-forward " --> ")
       (replace-match " -->  ")
       (re-search-forward " --> ")
       (replace-match "-->")
       (expect (buffer-string) :not :to-equal mock-vtt-data)
       (subed-vtt--sanitize)
       (expect (buffer-string) :to-equal mock-vtt-data)))
    (it "does not insert newline in empty buffer."
      (with-temp-vtt-buffer
       (expect (buffer-string) :to-equal "")
       (subed-vtt--sanitize)
       (expect (buffer-string) :to-equal "")))
    )


  (describe "Sorting"
    (it "orders subtitles by start time."
      (with-temp-vtt-buffer
       (insert mock-vtt-data)
       (goto-char (point-min))
       (re-search-forward "01:01")
       (replace-match "12:01")
       (goto-char (point-min))
       (re-search-forward "02:02")
       (replace-match "10:02")
       (goto-char (point-min))
       (re-search-forward "03:03")
       (replace-match "11:03")
       (subed-vtt--sort)
       (expect (buffer-string) :to-equal
               (concat
                "WEBVTT\n"
                "\n"
                "00:10:02.234 --> 00:02:10.345\n"
                "Bar.\n"
                "\n"
                "00:11:03.45 --> 00:03:15.5\n"
                "Baz.\n"
                "\n"
                "00:12:01.000 --> 00:01:05.123\n"
                "Foo.\n"))))
    (describe "preserves point in the current subtitle"
      (it "when subtitle text is non-empty."
        (with-temp-vtt-buffer
         (insert mock-vtt-data)
         (goto-char (point-min))
         (re-search-forward "01:01")
         (replace-match "12:01")
         (search-forward "\n")
         (expect (current-word) :to-equal "Foo")
         (subed-vtt--sort)
         (expect (current-word) :to-equal "Foo")))
      (it "when subtitle text is empty."
        (with-temp-vtt-buffer
         (insert "WEBVTT\n\n00:12:01.000 --> 00:01:05.123\n")
         (goto-char (point-max))
         (subed-vtt--sort)
         (expect (point) :to-equal (1- (point-max)))))
      )
    ))

/* Local Variables:             */
/* eval: (buttercup-minor-mode) */
/* End:                         */
