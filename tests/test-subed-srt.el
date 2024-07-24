;; -*- lexical-binding: t; eval: (buttercup-minor-mode) -*-

(load-file "./tests/undercover-init.el")
(require 'subed-srt)
(defvar mock-srt-data
  "1
00:01:01,000 --> 00:01:05,123
Foo.

2
00:02:02,234 --> 00:02:10,345
Bar.

3
00:03:03,45 --> 00:03:15,5
Baz.
")

(defmacro with-temp-srt-buffer (&rest body)
  "Initialize buffer to `subed-srt-mode' and run BODY."
  `(with-temp-buffer
    (subed-srt-mode)
    (progn ,@body)))

(describe "subed-srt"
  (describe "Getting"
    (describe "the subtitle ID"
      (it "returns the subtitle ID if it can be found."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 2)
         (expect (subed-subtitle-id) :to-equal 2)))
      (it "returns nil if no subtitle ID can be found."
        (with-temp-srt-buffer
         (expect (subed-subtitle-id) :to-equal nil)))
      )
    (describe "the subtitle ID at playback time"
      (it "returns subtitle ID if time is equal to start time."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (cl-loop for target-id from 1 to 3 do
                  (let ((msecs (subed-subtitle-msecs-start target-id)))
                    (cl-loop for outset-id from 1 to 3 do
                             (progn
                               (subed-jump-to-subtitle-id outset-id)
                               (expect (subed-subtitle-id-at-msecs msecs) :to-equal target-id)))))))
      (it "returns subtitle ID if time is equal to stop time."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (cl-loop for target-id from 1 to 3 do
                  (let ((msecs (subed-subtitle-msecs-stop target-id)))
                    (cl-loop for outset-id from 1 to 3 do
                             (progn
                               (subed-jump-to-subtitle-id outset-id)
                               (expect (subed-subtitle-id-at-msecs msecs) :to-equal target-id)))))))
      (it "returns subtitle ID if time is between start and stop time."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (cl-loop for target-id from 1 to 3 do
                  (let ((msecs (+ 1 (subed-subtitle-msecs-start target-id))))
                    (cl-loop for outset-id from 1 to 3 do
                             (progn
                               (subed-jump-to-subtitle-id outset-id)
                               (expect (subed-subtitle-id-at-msecs msecs) :to-equal target-id)))))))
      (it "returns nil if time is before the first subtitle's start time."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (let ((msecs (- (save-excursion
                           (goto-char (point-min))
                           (subed-subtitle-msecs-start)) 1)))
           (cl-loop for outset-id from 1 to 3 do
                    (progn
                      (subed-jump-to-subtitle-id outset-id)
                      (expect (subed-subtitle-id-at-msecs msecs) :to-equal nil))))))
      (it "returns nil if time is after the last subtitle's start time."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (let ((msecs (+ (save-excursion
                           (goto-char (point-max))
                           (subed-subtitle-msecs-stop)) 1)))
           (cl-loop for outset-id from 1 to 3 do
                    (progn
                      (subed-jump-to-subtitle-id outset-id)
                      (expect (subed-subtitle-id-at-msecs msecs) :to-equal nil))))))
      (it "returns nil if time is between subtitles."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (cl-loop for target-id from 1 to 2 do
                  (let ((msecs (+ (subed-subtitle-msecs-stop target-id) 1)))
                    (cl-loop for outset-id from 1 to 3 do
                             (progn
                               (subed-jump-to-subtitle-id outset-id)
                               (expect (subed-subtitle-id-at-msecs msecs) :to-equal nil))))
                  (let ((msecs (- (subed-subtitle-msecs-start (+ target-id 1)) 1)))
                    (cl-loop for outset-id from 1 to 3 do
                             (progn
                               (subed-jump-to-subtitle-id outset-id)
                               (expect (subed-subtitle-id-at-msecs msecs) :to-equal nil)))))))
      (it "doesn't fail if start time is invalid."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 2)
         (let ((msecs (- (subed-subtitle-msecs-start) 1)))
           (subed-jump-to-subtitle-time-start)
           (forward-char 8) (delete-char 1)
           (expect (subed-subtitle-id-at-msecs msecs) :to-equal 2))))
      )
    (describe "the subtitle start/stop time"
      (it "returns the time in milliseconds."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 2)
         (expect (subed-subtitle-msecs-start) :to-equal (+ (* 2 60000) (* 2 1000) 234))
         (expect (subed-subtitle-msecs-stop) :to-equal (+ (* 2 60000) (* 10 1000) 345))))
      (it "handles lack of digits in milliseconds gracefully."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 3)
         (expect (save-excursion (subed-jump-to-subtitle-time-start)
                                 (thing-at-point 'line)) :to-equal "00:03:03,45 --> 00:03:15,5\n")
         (expect (subed-subtitle-msecs-start) :to-equal (+ (* 3 60 1000) (*  3 1000) 450))
         (expect (subed-subtitle-msecs-stop)  :to-equal (+ (* 3 60 1000) (* 15 1000) 500))))
      (it "returns nil if time can't be found."
        (with-temp-srt-buffer
         (expect (subed-subtitle-msecs-start) :to-be nil)
         (expect (subed-subtitle-msecs-stop) :to-be nil)))
      )
    (describe "the subtitle text"
      (describe "when text is empty"
        (it "and at the beginning with a trailing newline."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-text 1)
           (kill-line)
           (expect (subed-subtitle-text) :to-equal "")))
        (it "and at the beginning without a trailing newline."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-text 1)
           (kill-whole-line)
           (expect (subed-subtitle-text) :to-equal "")))
        (it "and in the middle."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-text 2)
           (kill-line)
           (expect (subed-subtitle-text) :to-equal "")))
        (it "and at the end with a trailing newline."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-text 3)
           (kill-line)
           (expect (subed-subtitle-text) :to-equal "")))
        (it "and at the end without a trailing newline."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-text 3)
           (kill-whole-line)
           (expect (subed-subtitle-text) :to-equal "")))
        )
      (describe "when text is not empty"
        (it "and has no linebreaks."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-text 2)
           (expect (subed-subtitle-text) :to-equal "Bar.")))
        (it "and has linebreaks."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-text 2)
           (insert "Bar.\n")
           (expect (subed-subtitle-text) :to-equal "Bar.\nBar.")))
        )
      )
    (describe "the point within the subtitle"
      (it "returns the relative point if we can find an ID."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 2)
         (expect (subed-subtitle-relative-point) :to-equal 0)
         (forward-line)
         (expect (subed-subtitle-relative-point) :to-equal 2)
         (forward-line)
         (expect (subed-subtitle-relative-point) :to-equal 32)
         (forward-char)
         (expect (subed-subtitle-relative-point) :to-equal 33)
         (forward-line)
         (expect (subed-subtitle-relative-point) :to-equal 37)
         (forward-line)
         (expect (subed-subtitle-relative-point) :to-equal 0)))
      (it "returns nil if we can't find an ID."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 1)
         (insert "foo")
         (expect (subed-subtitle-relative-point) :to-equal nil)))
      )
    )
  (describe "Converting to msecs"
    (it "works with numbers."
      (expect (with-temp-srt-buffer (subed-to-msecs 5123)) :to-equal 5123))
    (it "works with numbers as strings."
      (expect (with-temp-srt-buffer (subed-to-msecs "5123")) :to-equal 5123))
    (it "works with timestamps."
      (with-temp-srt-buffer
       (expect (subed-to-msecs "00:00:05,124") :to-equal 5124))))
  (describe "Jumping"
    (describe "to current subtitle ID"
      (it "returns ID's point when point is already on the ID."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-min))
         (expect (thing-at-point 'word) :to-equal "1")
         (expect (subed-jump-to-subtitle-id) :to-equal 1)
         (expect (thing-at-point 'word) :to-equal "1")))
      (it "returns ID's point when point is on the duration."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (search-backward ",234")
         (expect (thing-at-point 'word) :to-equal "02")
         (expect (subed-jump-to-subtitle-id) :to-equal 39)
         (expect (thing-at-point 'word) :to-equal "2")))
      (it "returns ID's point when point is on the text."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (search-backward "Baz.")
         (expect (thing-at-point 'word) :to-equal "Baz")
         (expect (subed-jump-to-subtitle-id) :to-equal 77)
         (expect (thing-at-point 'word) :to-equal "3")))
      (it "returns ID's point when point is between subtitles."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-min))
         (search-forward "Bar.\n")
         (expect (thing-at-point 'line) :to-equal "\n")
         (expect (subed-jump-to-subtitle-id) :to-equal 39)
         (expect (thing-at-point 'word) :to-equal "2")))
      (it "returns nil if buffer is empty."
        (with-temp-srt-buffer
         (expect (buffer-string) :to-equal "")
         (expect (subed-jump-to-subtitle-id) :to-equal nil)))
      (it "returns ID's point when buffer starts with blank lines."
        (with-temp-srt-buffer
         (insert (concat " \n \t \n" mock-srt-data))
         (search-backward "Foo.")
         (expect (thing-at-point 'line) :to-equal "Foo.\n")
         (expect (subed-jump-to-subtitle-id) :to-equal 7)
         (expect (thing-at-point 'word) :to-equal "1")))
      (it "returns ID's point when subtitles are separated with blank lines."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-min))
         (search-forward "Foo.\n")
         (insert " \n \t \n")
         (expect (subed-jump-to-subtitle-id) :to-equal 1)
         (expect (thing-at-point 'word) :to-equal "1")))
      )
    (describe "to specific subtitle ID"
      (it "returns ID's point if wanted ID exists."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-max))
         (expect (subed-jump-to-subtitle-id 2) :to-equal 39)
         (expect (thing-at-point 'word) :to-equal "2")
         (expect (subed-jump-to-subtitle-id 1) :to-equal 1)
         (expect (thing-at-point 'word) :to-equal "1")
         (expect (subed-jump-to-subtitle-id 3) :to-equal 77)
         (expect (thing-at-point 'word) :to-equal "3")))
      (it "returns nil and does not move if wanted ID does not exists."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-min))
         (search-forward "Foo")
         (let ((stored-point (point)))
           (expect (subed-jump-to-subtitle-id 4) :to-equal nil)
           (expect stored-point :to-equal (point)))))
      )
    (describe "to subtitle ID at specific time"
      (it "returns ID's point if point changed."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-max))
         (spy-on 'subed-subtitle-id-at-msecs :and-return-value (point-min))
         (expect (subed-jump-to-subtitle-id-at-msecs 123450) :to-equal (point-min))
         (expect (point) :to-equal (point-min))
         (expect 'subed-subtitle-id-at-msecs :to-have-been-called-with 123450)
         (expect 'subed-subtitle-id-at-msecs :to-have-been-called-times 1)))
      (it "returns nil if point didn't change."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char 75)
         (spy-on 'subed-subtitle-id-at-msecs :and-return-value 75)
         (expect (subed-jump-to-subtitle-id-at-msecs 123450) :to-equal nil)
         (expect (point) :to-equal 75)
         (expect 'subed-subtitle-id-at-msecs :to-have-been-called-with 123450)
         (expect 'subed-subtitle-id-at-msecs :to-have-been-called-times 1)))
      )
    (describe "to subtitle start time"
      (it "returns start time's point if movement was successful."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-min))
         (expect (subed-jump-to-subtitle-time-start) :to-equal 3)
         (expect (looking-at subed--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:01:01,000")
         (re-search-forward "\n\n")
         (expect (subed-jump-to-subtitle-time-start) :to-equal 41)
         (expect (looking-at subed--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:02:02,234")
         (re-search-forward "\n\n")
         (expect (subed-jump-to-subtitle-time-start) :to-equal 79)
         (expect (looking-at subed--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:03:03,45")))
      (it "returns nil if movement failed."
        (with-temp-srt-buffer
         (expect (subed-jump-to-subtitle-time-start) :to-equal nil)))
      )
    (describe "to subtitle stop time"
      (it "returns stop time's point if movement was successful."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-min))
         (expect (subed-jump-to-subtitle-time-stop) :to-equal 20)
         (expect (looking-at subed--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:01:05,123")
         (re-search-forward "\n\n")
         (expect (subed-jump-to-subtitle-time-stop) :to-equal 58)
         (expect (looking-at subed--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:02:10,345")
         (re-search-forward "\n\n")
         (expect (subed-jump-to-subtitle-time-stop) :to-equal 95)
         (expect (looking-at subed--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "00:03:15,5")))
      (it "returns nil if movement failed."
        (with-temp-srt-buffer
         (expect (subed-jump-to-subtitle-time-stop) :to-equal nil)))
      )
    (describe "to subtitle text"
      (it "returns subtitle text's point if movement was successful."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-min))
         (expect (subed-jump-to-subtitle-text) :to-equal 33)
         (expect (point) :to-equal (save-excursion (goto-char (point-max)) (search-backward "Foo.")))
         (re-search-forward "\n\n")
         (expect (subed-jump-to-subtitle-text) :to-equal 71)
         (expect (point) :to-equal (save-excursion (goto-char (point-max)) (search-backward "Bar.")))
         (re-search-forward "\n\n")
         (expect (subed-jump-to-subtitle-text) :to-equal 106)
         (expect (point) :to-equal (save-excursion (goto-char (point-max)) (search-backward "Baz.")))))
      (it "returns nil if movement failed."
        (with-temp-srt-buffer
         (expect (subed-jump-to-subtitle-time-stop) :to-equal nil)))
      )
    (describe "to end of subtitle text"
      (it "returns point if subtitle end can be found."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-min))
         (expect (subed-jump-to-subtitle-end) :to-be 37)
         (expect (looking-back "^Foo.$") :to-be t)
         (forward-char 2)
         (expect (subed-jump-to-subtitle-end) :to-be 75)
         (expect (looking-back "^Bar.$") :to-be t)
         (forward-char 2)
         (expect (subed-jump-to-subtitle-end) :to-be 110)
         (expect (looking-back "^Baz.$") :to-be t)
         (goto-char (point-max))
         (backward-char 2)
         (expect (subed-jump-to-subtitle-end) :to-be 110)
         (expect (looking-back "^Baz.$") :to-be t)))
      (it "returns nil if subtitle end cannot be found."
        (with-temp-srt-buffer
         (expect (subed-jump-to-subtitle-end) :to-be nil)))
      (it "returns nil if point did not move."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 1)
         (kill-line)
         (expect (subed-jump-to-subtitle-end) :to-be nil)))
      (it "works if text is empty with trailing newline."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 1)
         (kill-line)
         (backward-char)
         (expect (subed-jump-to-subtitle-end) :to-be 33)
         (expect (looking-at "^$") :to-be t)
         (subed-jump-to-subtitle-text 2)
         (kill-line)
         (backward-char)
         (expect (subed-jump-to-subtitle-end) :to-be 67)
         (expect (looking-at "^$") :to-be t)
         (subed-jump-to-subtitle-text 3)
         (kill-line)
         (backward-char)
         (expect (subed-jump-to-subtitle-end) :to-be 98)
         (expect (looking-at "^$") :to-be t)))
      (it "works if text is empty without trailing newline."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 1)
         (kill-whole-line)
         (expect (subed-jump-to-subtitle-end) :to-be nil)
         (expect (looking-at "^$") :to-be t)
         (goto-char (point-min))
         (expect (subed-jump-to-subtitle-end) :to-be 33)
         (expect (looking-at "^$") :to-be t)
         (subed-jump-to-subtitle-text 2)
         (kill-whole-line)
         (expect (subed-jump-to-subtitle-end) :to-be nil)
         (expect (looking-at "^$") :to-be t)
         (backward-char)
         (expect (subed-jump-to-subtitle-end) :to-be 66)
         (expect (looking-at "^$") :to-be t)
         (subed-jump-to-subtitle-text 3)
         (kill-whole-line)
         (expect (subed-jump-to-subtitle-end) :to-be nil)
         (expect (looking-at "^$") :to-be t)
         (backward-char)
         (expect (subed-jump-to-subtitle-end) :to-be 96)
         (expect (looking-at "^$") :to-be t)))
      (it "works with multi-line cues where a line is all numbers."
        (with-temp-srt-buffer
         (insert "1
00:00:00,000 --> 00:00:01,000
This is first subtitle.
123456789

2
00:00:01,000 --> 00:00:02,000
This is second subtitle.
")
         (goto-char (point-min))
         (expect (subed-jump-to-subtitle-end) :to-be 66))))
    (describe "to next subtitle ID"
      (it "returns point when there is a next subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 1)
         (expect (thing-at-point 'word) :to-equal "1")
         (expect (subed-forward-subtitle-id) :to-be 39)
         (expect (thing-at-point 'word) :to-equal "2")
         (subed-jump-to-subtitle-time-start 2)
         (expect (thing-at-point 'word) :to-equal "00")
         (expect (subed-forward-subtitle-id) :to-be 77)
         (expect (thing-at-point 'word) :to-equal "3")))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-srt-buffer
         (expect (thing-at-point 'word) :to-equal nil)
         (expect (subed-forward-subtitle-id) :to-be nil))
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 1)
         (expect (thing-at-point 'word) :to-equal "Foo")
         (expect (subed-forward-subtitle-id) :to-be 39)
         (expect (thing-at-point 'word) :to-equal "2")
         (subed-jump-to-subtitle-time-stop 2)
         (expect (thing-at-point 'word) :to-equal "00")
         (expect (subed-forward-subtitle-id) :to-be 77)
         (expect (thing-at-point 'word) :to-equal "3"))
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 3)
         (expect (thing-at-point 'word) :to-equal "Baz")
         (expect (subed-forward-subtitle-id) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "Baz"))
        (with-temp-srt-buffer
         (insert (concat mock-srt-data "\n\n"))
         (subed-jump-to-subtitle-time-stop 3)
         (expect (thing-at-point 'word) :to-equal "00")
         (expect (subed-forward-subtitle-id) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "00")))
      )
    (describe "to previous subtitle ID"
      (it "returns point when there is a previous subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 2)
         (expect (thing-at-point 'word) :to-equal "Bar")
         (expect (subed-backward-subtitle-id) :to-be 1)
         (expect (thing-at-point 'word) :to-equal "1")
         (subed-jump-to-subtitle-time-stop 3)
         (expect (thing-at-point 'word) :to-equal "00")
         (expect (subed-backward-subtitle-id) :to-be 39)
         (expect (thing-at-point 'word) :to-equal "2")))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-srt-buffer
         (expect (subed-backward-subtitle-id) :to-be nil))
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 1)
         (expect (thing-at-point 'word) :to-equal "1")
         (expect (subed-backward-subtitle-id) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "1"))
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 1)
         (expect (thing-at-point 'word) :to-equal "Foo")
         (expect (subed-backward-subtitle-id) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "Foo"))
        (with-temp-srt-buffer
         (insert (concat "\n\n\n" mock-srt-data))
         (subed-jump-to-subtitle-time-stop 1)
         (expect (thing-at-point 'word) :to-equal "00")
         (expect (subed-backward-subtitle-id) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "00")))
      )
    (describe "to next subtitle text"
      (it "returns point when there is a next subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 1)
         (expect (thing-at-point 'word) :to-equal "1")
         (expect (subed-forward-subtitle-text) :to-be 71)
         (expect (thing-at-point 'word) :to-equal "Bar")))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-srt-buffer
         (goto-char (point-max))
         (insert (concat mock-srt-data "\n\n"))
         (subed-jump-to-subtitle-id 3)
         (expect (thing-at-point 'word) :to-equal "3")
         (expect (subed-forward-subtitle-text) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "3")))
      )
    (describe "to previous subtitle text"
      (it "returns point when there is a previous subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 3)
         (expect (thing-at-point 'word) :to-equal "3")
         (expect (subed-backward-subtitle-text) :to-be 71)
         (expect (thing-at-point 'word) :to-equal "Bar")))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-min))
         (expect (thing-at-point 'word) :to-equal "1")
         (expect (subed-backward-subtitle-text) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "1")))
      )
    (describe "to next subtitle end"
      (it "returns point when there is a next subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 2)
         (expect (thing-at-point 'word) :to-equal "Bar")
         (expect (subed-forward-subtitle-end) :to-be 110)
         (expect (thing-at-point 'word) :to-equal nil)))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-srt-buffer
         (insert (concat mock-srt-data "\n\n"))
         (subed-jump-to-subtitle-text 3)
         (end-of-line)
         (expect (thing-at-point 'word) :to-equal nil)
         (expect (subed-forward-subtitle-end) :to-be nil)
         (expect (thing-at-point 'word) :to-equal nil)))
      )
    (describe "to previous subtitle end"
      (it "returns point when there is a previous subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 3)
         (expect (thing-at-point 'word) :to-equal "3")
         (expect (subed-backward-subtitle-text) :to-be 71)
         (expect (thing-at-point 'word) :to-equal "Bar")))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-min))
         (expect (thing-at-point 'word) :to-equal "1")
         (expect (subed-backward-subtitle-text) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "1")))
      )
    (describe "to next subtitle start time"
      (it "returns point when there is a next subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 1)
         (expect (thing-at-point 'word) :to-equal "Foo")
         (expect (subed-forward-subtitle-time-start) :to-be 41)
         (expect (thing-at-point 'word) :to-equal "00")))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 3)
         (expect (thing-at-point 'word) :to-equal "3")
         (expect (subed-forward-subtitle-time-start) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "3")))
      )
    (describe "to previous subtitle start time"
      (it "returns point when there is a previous subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 2)
         (expect (thing-at-point 'word) :to-equal "2")
         (expect (subed-backward-subtitle-time-start) :to-be 3)
         (expect (thing-at-point 'word) :to-equal "00")))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 1)
         (expect (thing-at-point 'word) :to-equal "1")
         (expect (subed-backward-subtitle-time-start) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "1")))
      )
    (describe "to next subtitle stop time"
      (it "returns point when there is a next subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 1)
         (expect (thing-at-point 'word) :to-equal "Foo")
         (expect (subed-forward-subtitle-time-stop) :to-be 58)
         (expect (thing-at-point 'word) :to-equal "00")))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 3)
         (expect (thing-at-point 'word) :to-equal "3")
         (expect (subed-forward-subtitle-time-stop) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "3")))
      )
    (describe "to previous subtitle stop time"
      (it "returns point when there is a previous subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 3)
         (expect (thing-at-point 'word) :to-equal "3")
         (expect (subed-backward-subtitle-time-stop) :to-be 58)
         (expect (thing-at-point 'word) :to-equal "00")))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 1)
         (expect (thing-at-point 'word) :to-equal "1")
         (expect (subed-backward-subtitle-time-stop) :to-be nil)
         (expect (thing-at-point 'word) :to-equal "1")))
      )
    )

  (describe "Setting start/stop time"
    (it "of current subtitle."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-end 2)
       (subed-set-subtitle-time-start (subed-timestamp-to-msecs "1:02:03,400") nil t t)
       (expect (buffer-string) :to-equal (concat "1\n"
                                                 "00:01:01,000 --> 00:01:05,123\n"
                                                 "Foo.\n\n"
                                                 "2\n"
                                                 "01:02:03,400 --> 00:02:10,345\n"
                                                 "Bar.\n\n"
                                                 "3\n"
                                                 "00:03:03,45 --> 00:03:15,5\n"
                                                 "Baz.\n"))
       (subed-set-subtitle-time-stop (subed-timestamp-to-msecs "5:06:07,800") nil t t)
       (expect (buffer-string) :to-equal (concat "1\n"
                                                 "00:01:01,000 --> 00:01:05,123\n"
                                                 "Foo.\n\n"
                                                 "2\n"
                                                 "01:02:03,400 --> 05:06:07,800\n"
                                                 "Bar.\n\n"
                                                 "3\n"
                                                 "00:03:03,45 --> 00:03:15,5\n"
                                                 "Baz.\n"))))
    (it "of specific subtitle."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-time-stop 3)
       (subed-set-subtitle-time-start (+ (* 2 60 60 1000) (* 4 60 1000) (* 6 1000) 800) 1 t t)
       (expect (buffer-string) :to-equal (concat "1\n"
                                                 "02:04:06,800 --> 00:01:05,123\n"
                                                 "Foo.\n\n"
                                                 "2\n"
                                                 "00:02:02,234 --> 00:02:10,345\n"
                                                 "Bar.\n\n"
                                                 "3\n"
                                                 "00:03:03,45 --> 00:03:15,5\n"
                                                 "Baz.\n"))
       (subed-jump-to-subtitle-text 1)
       (subed-set-subtitle-time-stop (+ (* 3 60 60 1000) (* 5 60 1000) (* 7 1000) 900) 3 t t)
       (expect (buffer-string) :to-equal (concat "1\n"
                                                 "02:04:06,800 --> 00:01:05,123\n"
                                                 "Foo.\n\n"
                                                 "2\n"
                                                 "00:02:02,234 --> 00:02:10,345\n"
                                                 "Bar.\n\n"
                                                 "3\n"
                                                 "00:03:03,45 --> 03:05:07,900\n"
                                                 "Baz.\n"))))
    (it "when milliseconds lack digits."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-id 3)
       (subed-set-subtitle-time-start (+ (* 1 60 60 1000) (* 2 60 1000) (* 3 1000) 4) 3 t)
       (expect (save-excursion (subed-jump-to-subtitle-time-start)
                               (thing-at-point 'line)) :to-equal "01:02:03,004 --> 00:03:15,5\n")
       (subed-set-subtitle-time-stop (+ (* 2 60 60 1000) (* 3 60 1000) (* 4 1000) 60) 3 t)
       (expect (save-excursion (subed-jump-to-subtitle-time-start)
                               (thing-at-point 'line)) :to-equal "01:02:03,004 --> 02:03:04,060\n")))
    )

  (describe "Inserting a subtitle"
    (describe "in an empty buffer"
      (describe "before"
        (it "passing nothing."
          (with-temp-srt-buffer
           (expect (subed-prepend-subtitle) :to-equal 33)
           (expect (buffer-string) :to-equal (concat "0\n"
                                                     "00:00:00,000 --> 00:00:01,000\n\n"))
           (expect (point) :to-equal 33)))
        (it "passing ID."
          (with-temp-srt-buffer
           (expect (subed-prepend-subtitle 2) :to-equal 33)
           (expect (buffer-string) :to-equal (concat "2\n"
                                                     "00:00:00,000 --> 00:00:01,000\n\n"))
           (expect (point) :to-equal 33)))
        (it "passing ID and start time."
          (with-temp-srt-buffer
           (expect (subed-prepend-subtitle 3 60000) :to-equal 33)
           (expect (buffer-string) :to-equal (concat "3\n"
                                                     "00:01:00,000 --> 00:01:01,000\n\n"))
           (expect (point) :to-equal 33)))
        (it "passing ID, start time and stop time."
          (with-temp-srt-buffer
           (expect (subed-prepend-subtitle 4 60000 65000) :to-equal 33)
           (expect (buffer-string) :to-equal (concat "4\n"
                                                     "00:01:00,000 --> 00:01:05,000\n\n"))
           (expect (point) :to-equal 33)))
        (it "passing ID, start time, stop time and text."
          (with-temp-srt-buffer
           (expect (subed-prepend-subtitle 5 60000 65000 "Foo, bar\nbaz.") :to-equal 33)
           (expect (buffer-string) :to-equal (concat "5\n"
                                                     "00:01:00,000 --> 00:01:05,000\n"
                                                     "Foo, bar\nbaz.\n"))
           (expect (point) :to-equal 33)))
        )
      (describe "after"
        (it "passing nothing."
          (with-temp-srt-buffer
           (expect (subed-append-subtitle) :to-equal 33)
           (expect (buffer-string) :to-equal (concat "0\n"
                                                     "00:00:00,000 --> 00:00:01,000\n\n"))
           (expect (point) :to-equal 33)))
        (it "passing ID."
          (with-temp-srt-buffer
           (expect (subed-append-subtitle 2) :to-equal 33)
           (expect (buffer-string) :to-equal (concat "2\n"
                                                     "00:00:00,000 --> 00:00:01,000\n\n"))
           (expect (point) :to-equal 33)))
        (it "passing ID and start time."
          (with-temp-srt-buffer
           (expect (subed-append-subtitle 3 60000) :to-equal 33)
           (expect (buffer-string) :to-equal (concat "3\n"
                                                     "00:01:00,000 --> 00:01:01,000\n\n"))
           (expect (point) :to-equal 33)))
        (it "passing ID, start time and stop time."
          (with-temp-srt-buffer
           (expect (subed-append-subtitle 4 60000 65000) :to-equal 33)
           (expect (buffer-string) :to-equal (concat "4\n"
                                                     "00:01:00,000 --> 00:01:05,000\n\n"))
           (expect (point) :to-equal 33)))
        (it "passing ID, start time, stop time and text."
          (with-temp-srt-buffer
           (expect (subed-append-subtitle 5 60000 65000 "Foo, bar\nbaz.") :to-equal 33)
           (expect (buffer-string) :to-equal (concat "5\n"
                                                     "00:01:00,000 --> 00:01:05,000\n"
                                                     "Foo, bar\nbaz.\n"))
           (expect (point) :to-equal 33)))
        )
      )
    (describe "in a non-empty buffer"
      (describe "before the current subtitle"
        (describe "with point on the first subtitle"
          (it "passing nothing."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n"))
             (subed-jump-to-subtitle-time-stop)
             (expect (subed-prepend-subtitle) :to-equal 33)
             (expect (buffer-string) :to-equal (concat "0\n"
                                                       "00:00:00,000 --> 00:00:01,000\n"
                                                       "\n\n"
                                                       "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n"))
             (expect (point) :to-equal 33)))
          (it "passing ID."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n"))
             (subed-jump-to-subtitle-time-stop)
             (expect (subed-prepend-subtitle 7) :to-equal 33)
             (expect (buffer-string) :to-equal (concat "7\n"
                                                       "00:00:00,000 --> 00:00:01,000\n"
                                                       "\n\n"
                                                       "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n"))
             (expect (point) :to-equal 33)))
          (it "passing ID and start time."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n"))
             (subed-jump-to-subtitle-time-stop)
             (expect (subed-prepend-subtitle 6 1500) :to-equal 33)
             (expect (buffer-string) :to-equal (concat "6\n"
                                                       "00:00:01,500 --> 00:00:02,500\n"
                                                       "\n\n"
                                                       "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n"))
             (expect (point) :to-equal 33)))
          (it "passing ID, start time and stop time."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n"))
             (subed-jump-to-subtitle-time-stop)
             (expect (subed-prepend-subtitle 5 1500 2000) :to-equal 33)
             (expect (buffer-string) :to-equal (concat "5\n"
                                                       "00:00:01,500 --> 00:00:02,000\n"
                                                       "\n\n"
                                                       "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n"))
             (expect (point) :to-equal 33)))
          (it "passing ID, start time, stop time and text."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n"))
             (subed-jump-to-subtitle-time-stop)
             (expect (subed-prepend-subtitle 4 1500 3000 "Bar.") :to-equal 33)
             (expect (buffer-string) :to-equal (concat "4\n"
                                                       "00:00:01,500 --> 00:00:03,000\n"
                                                       "Bar.\n\n"
                                                       "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n"))
             (expect (point) :to-equal 33)))
          )
        (describe "with point on a non-first subtitle"
          (it "passing nothing."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n\n"
                             "2\n"
                             "00:00:10,000 --> 00:00:12,000\n"
                             "Bar.\n"))
             (subed-jump-to-subtitle-text 2)
             (expect (subed-prepend-subtitle) :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n\n"
                                                       "0\n"
                                                       "00:00:00,000 --> 00:00:01,000\n"
                                                       "\n\n"
                                                       "2\n"
                                                       "00:00:10,000 --> 00:00:12,000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 71)))
          (it "passing ID."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n\n"
                             "2\n"
                             "00:00:10,000 --> 00:00:12,000\n"
                             "Bar.\n"))
             (subed-jump-to-subtitle-text 2)
             (expect (subed-prepend-subtitle 9) :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n\n"
                                                       "9\n"
                                                       "00:00:00,000 --> 00:00:01,000\n"
                                                       "\n\n"
                                                       "2\n"
                                                       "00:00:10,000 --> 00:00:12,000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 71)))
          (it "passing ID and start time."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n\n"
                             "2\n"
                             "00:00:10,000 --> 00:00:12,000\n"
                             "Bar.\n"))
             (subed-jump-to-subtitle-text 2)
             (expect (subed-prepend-subtitle 9 7000) :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n\n"
                                                       "9\n"
                                                       "00:00:07,000 --> 00:00:08,000\n"
                                                       "\n\n"
                                                       "2\n"
                                                       "00:00:10,000 --> 00:00:12,000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 71)))
          (it "passing ID, start time and stop time."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n\n"
                             "2\n"
                             "00:00:10,000 --> 00:00:12,000\n"
                             "Bar.\n"))
             (subed-jump-to-subtitle-text 2)
             (expect (subed-prepend-subtitle 9 7000 7123) :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n\n"
                                                       "9\n"
                                                       "00:00:07,000 --> 00:00:07,123\n"
                                                       "\n\n"
                                                       "2\n"
                                                       "00:00:10,000 --> 00:00:12,000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 71)))
          (it "passing ID, start time, stop time and text."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n\n"
                             "2\n"
                             "00:00:10,000 --> 00:00:12,000\n"
                             "Bar.\n"))
             (subed-jump-to-subtitle-text 2)
             (expect (subed-prepend-subtitle 9 7000 7123 "Baz.") :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n\n"
                                                       "9\n"
                                                       "00:00:07,000 --> 00:00:07,123\n"
                                                       "Baz.\n\n"
                                                       "2\n"
                                                       "00:00:10,000 --> 00:00:12,000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 71)))
          )
        )
      (describe "after the current subtitle"
        (describe "with point on the last subtitle"
          (it "passing nothing."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n"))
             (subed-jump-to-subtitle-text)
             (expect (subed-append-subtitle) :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n\n"
                                                       "0\n"
                                                       "00:00:00,000 --> 00:00:01,000\n"
                                                       "\n"))
             (expect (point) :to-equal 71)))
          (it "passing ID."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n"))
             (subed-jump-to-subtitle-text)
             (expect (subed-append-subtitle 5) :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n\n"
                                                       "5\n"
                                                       "00:00:00,000 --> 00:00:01,000\n"
                                                       "\n"))
             (expect (point) :to-equal 71)))
          (it "passing ID and start time."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n"))
             (subed-jump-to-subtitle-text)
             (expect (subed-append-subtitle 5 12345) :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n\n"
                                                       "5\n"
                                                       "00:00:12,345 --> 00:00:13,345\n"
                                                       "\n"))
             (expect (point) :to-equal 71)))
          (it "passing ID, start time and stop time."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n"))
             (subed-jump-to-subtitle-text)
             (expect (subed-append-subtitle 5 12345 15000) :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n\n"
                                                       "5\n"
                                                       "00:00:12,345 --> 00:00:15,000\n"
                                                       "\n"))
             (expect (point) :to-equal 71)))
          (it "passing ID, start time, stop time and text."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Foo.\n"))
             (subed-jump-to-subtitle-text)
             (expect (subed-append-subtitle 5 12345 15000 "Bar.") :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Foo.\n\n"
                                                       "5\n"
                                                       "00:00:12,345 --> 00:00:15,000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 71)))
          )
        (describe "with point on a non-last subtitle"
          (it "passing nothing."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:01,000 --> 00:00:02,000\n"
                             "Foo.\n\n"
                             "2\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Bar.\n"))
             (subed-jump-to-subtitle-time-start 1)
             (expect (subed-append-subtitle) :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:01,000 --> 00:00:02,000\n"
                                                       "Foo.\n\n"
                                                       "0\n"
                                                       "00:00:00,000 --> 00:00:01,000\n"
                                                       "\n\n"
                                                       "2\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 71)))
          (it "passing ID."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:01,000 --> 00:00:02,000\n"
                             "Foo.\n\n"
                             "2\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Bar.\n"))
             (subed-jump-to-subtitle-time-start 1)
             (expect (subed-append-subtitle 7) :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:01,000 --> 00:00:02,000\n"
                                                       "Foo.\n\n"
                                                       "7\n"
                                                       "00:00:00,000 --> 00:00:01,000\n"
                                                       "\n\n"
                                                       "2\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 71)))
          (it "passing ID and start time."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:01,000 --> 00:00:02,000\n"
                             "Foo.\n\n"
                             "2\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Bar.\n"))
             (subed-jump-to-subtitle-time-start 1)
             (expect (subed-append-subtitle 7 2500) :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:01,000 --> 00:00:02,000\n"
                                                       "Foo.\n\n"
                                                       "7\n"
                                                       "00:00:02,500 --> 00:00:03,500\n"
                                                       "\n\n"
                                                       "2\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 71)))
          (it "passing ID, start time and stop time."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:01,000 --> 00:00:02,000\n"
                             "Foo.\n\n"
                             "2\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Bar.\n"))
             (subed-jump-to-subtitle-time-start 1)
             (expect (subed-append-subtitle 7 2500 4000) :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:01,000 --> 00:00:02,000\n"
                                                       "Foo.\n\n"
                                                       "7\n"
                                                       "00:00:02,500 --> 00:00:04,000\n"
                                                       "\n\n"
                                                       "2\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 71)))
          (it "passing ID, start time, stop time and text."
            (with-temp-srt-buffer
             (insert (concat "1\n"
                             "00:00:01,000 --> 00:00:02,000\n"
                             "Foo.\n\n"
                             "2\n"
                             "00:00:05,000 --> 00:00:06,000\n"
                             "Bar.\n"))
             (subed-jump-to-subtitle-time-start 1)
             (expect (subed-append-subtitle 7 2500 4000 "Baz.") :to-equal 71)
             (expect (buffer-string) :to-equal (concat "1\n"
                                                       "00:00:01,000 --> 00:00:02,000\n"
                                                       "Foo.\n\n"
                                                       "7\n"
                                                       "00:00:02,500 --> 00:00:04,000\n"
                                                       "Baz.\n\n"
                                                       "2\n"
                                                       "00:00:05,000 --> 00:00:06,000\n"
                                                       "Bar.\n"))
             (expect (point) :to-equal 71)))
          )
        )
      (it "when point is on empty text."
        (with-temp-srt-buffer
         (insert (concat "1\n"
                         "00:00:01,000 --> 00:00:02,000\n"
                         "\n"))
         (subed-jump-to-subtitle-text)
         (expect (subed-append-subtitle) :to-equal 67)
         (expect (buffer-string) :to-equal (concat "1\n"
                                                   "00:00:01,000 --> 00:00:02,000\n"
                                                   "\n\n"
                                                   "0\n"
                                                   "00:00:00,000 --> 00:00:01,000\n"
                                                   "\n"))
         (expect (point) :to-equal 67)))
      )
    )

  (describe "Killing a subtitle"
    (it "removes the first subtitle."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-text 1)
       (subed-kill-subtitle)
       (expect (buffer-string) :to-equal (concat "2\n"
                                                 "00:02:02,234 --> 00:02:10,345\n"
                                                 "Bar.\n\n"
                                                 "3\n"
                                                 "00:03:03,45 --> 00:03:15,5\n"
                                                 "Baz.\n"))))
    (it "removes it in between."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-text 2)
       (subed-kill-subtitle)
       (expect (buffer-string) :to-equal (concat "1\n"
                                                 "00:01:01,000 --> 00:01:05,123\n"
                                                 "Foo.\n\n"
                                                 "3\n"
                                                 "00:03:03,45 --> 00:03:15,5\n"
                                                 "Baz.\n"))))
    (it "removes the last subtitle."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-text 3)
       (subed-kill-subtitle)
       (expect (buffer-string) :to-equal (concat "1\n"
                                                 "00:01:01,000 --> 00:01:05,123\n"
                                                 "Foo.\n\n"
                                                 "2\n"
                                                 "00:02:02,234 --> 00:02:10,345\n"
                                                 "Bar.\n"))))
    (describe "removes the previous subtitle when point is right above the ID"
      (it "of the last subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 3)
         (backward-char)
         (expect (looking-at "^\n3\n") :to-be t)
         (subed-kill-subtitle)
         (expect (buffer-string) :to-equal (concat "1\n"
                                                   "00:01:01,000 --> 00:01:05,123\n"
                                                   "Foo.\n\n"
                                                   "3\n"
                                                   "00:03:03,45 --> 00:03:15,5\n"
                                                   "Baz.\n"))))
      (it "of a non-last subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-id 2)
         (backward-char)
         (expect (looking-at "^\n2\n") :to-be t)
         (subed-kill-subtitle)
         (expect (buffer-string) :to-equal (concat "2\n"
                                                   "00:02:02,234 --> 00:02:10,345\n"
                                                   "Bar.\n\n"
                                                   "3\n"
                                                   "00:03:03,45 --> 00:03:15,5\n"
                                                   "Baz.\n"))))
      )
    )

  (describe "Validating"
    (it "works in empty buffer."
      (with-temp-srt-buffer
       (subed-validate)))
    (it "works in buffer that contains only newlines."
      (with-temp-srt-buffer
       (cl-loop for _ from 1 to 10 do
                (insert "\n")
                (subed-validate))))
    (it "works in buffer that contains only spaces."
      (with-temp-srt-buffer
       (cl-loop for _ from 1 to 10 do
                (insert " ")
                (subed-validate))))
    (it "works in buffer that contains only spaces and newlines."
      (with-temp-srt-buffer
       (cl-loop for _ from 1 to 10 do
                (if (eq (random 2) 0)
                    (insert " ")
                  (insert "\n"))
                (subed-validate))))
    (it "reports invalid IDs."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-id 1)
       (insert "x")
       (expect (subed-validate) :to-throw
               'error '("Found invalid subtitle ID: \"x1\""))
       (expect (point) :to-equal 1)))
    (it "reports invalid start time."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-time-start 1)
       (forward-char 5)
       (delete-char 1)
       (expect (subed-validate) :to-throw
               'error '("Found invalid start time: \"00:0101,000 --> 00:01:05,123\""))
       (expect (point) :to-equal 3)))
    (it "reports invalid stop time."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-time-stop 1)
       (forward-char 10)
       (insert "3")
       (expect (subed-validate) :to-throw
               'error '("Found invalid stop time: \"00:01:01,000 --> 00:01:05,1323\""))
       (expect (point) :to-equal 20)))
    (it "reports invalid time separator."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-time-stop 1)
       (delete-char -1)
       (expect (subed-validate) :to-throw
               'error '("Found invalid separator between start and stop time: \"00:01:01,000 -->00:01:05,123\""))
       (expect (point) :to-equal 15)))
    (it "reports invalid start time in later entries."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-time-start 3)
       (forward-char 3)
       (insert "##")
       (expect (subed-validate) :to-throw
               'error '("Found invalid start time: \"00:##03:03,45 --> 00:03:15,5\""))
       (expect (point) :to-equal 79)))
    (it "does not report error when last subtitle text is empty."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-text 3)
       (kill-whole-line)
       (forward-char -2)
       (subed-validate)
       (expect (point) :to-equal 104)))
    (it "preserves point if there is no error."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-text 2)
       (forward-char 2)
       (subed-validate)
       (expect (point) :to-equal 73)))
    (it "runs before saving."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-time-start 3)
       (forward-char 3)
       (insert "##")
       (expect (subed-prepare-to-save) :to-throw
               'error '("Found invalid start time: \"00:##03:03,45 --> 00:03:15,5\""))
       (expect (point) :to-equal 79))))

  (describe "Sanitizing"
    (it "removes trailing tabs and spaces from all lines."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (goto-char (point-min))
       (while (re-search-forward "\n" nil t)
         (replace-match " \n"))
       (expect (buffer-string) :not :to-equal mock-srt-data)
       (subed-sanitize)
       (expect (buffer-string) :to-equal mock-srt-data))
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (goto-char (point-min))
       (while (re-search-forward "\n" nil t)
         (replace-match "\t\n"))
       (expect (buffer-string) :not :to-equal mock-srt-data)
       (subed-sanitize)
       (expect (buffer-string) :to-equal mock-srt-data)))
    (it "removes leading tabs and spaces from all lines."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (goto-char (point-min))
       (while (re-search-forward "\n" nil t)
         (replace-match "\n "))
       (expect (buffer-string) :not :to-equal mock-srt-data)
       (subed-sanitize)
       (expect (buffer-string) :to-equal mock-srt-data))
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (goto-char (point-min))
       (while (re-search-forward "\n" nil t)
         (replace-match "\n\t"))
       (expect (buffer-string) :not :to-equal mock-srt-data)
       (subed-sanitize)
       (expect (buffer-string) :to-equal mock-srt-data)))
    (it "removes excessive empty lines between subtitles."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (goto-char (point-min))
       (while (re-search-forward "\n\n" nil t)
         (replace-match "\n \n  \t  \t\t  \n\n  \t\n"))
       (expect (buffer-string) :not :to-equal mock-srt-data)
       (subed-sanitize)
       (expect (buffer-string) :to-equal mock-srt-data)))
    (it "ensures double newline between subtitles if text of previous subtitle is empty."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-text 1)
       (kill-whole-line)
       (expect (buffer-string) :to-equal (concat "1\n"
                                                 "00:01:01,000 --> 00:01:05,123\n"
                                                 "\n"
                                                 "2\n"
                                                 "00:02:02,234 --> 00:02:10,345\n"
                                                 "Bar.\n\n"
                                                 "3\n"
                                                 "00:03:03,45 --> 00:03:15,5\n"
                                                 "Baz.\n"))
       (subed-sanitize)
       (expect (buffer-string) :to-equal (concat "1\n"
                                                 "00:01:01,000 --> 00:01:05,123\n"
                                                 "\n\n"
                                                 "2\n"
                                                 "00:02:02,234 --> 00:02:10,345\n"
                                                 "Bar.\n\n"
                                                 "3\n"
                                                 "00:03:03,45 --> 00:03:15,5\n"
                                                 "Baz.\n"))))
    (it "removes empty lines from beginning of buffer."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (goto-char (point-min))
       (insert " \n\t\n")
       (expect (buffer-string) :not :to-equal mock-srt-data)
       (subed-sanitize)
       (expect (buffer-string) :to-equal mock-srt-data)))
    (it "removes empty lines from end of buffer."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (goto-char (point-max))
       (insert " \n\t\n\n")
       (expect (buffer-string) :not :to-equal mock-srt-data)
       (subed-sanitize)
       (expect (buffer-string) :to-equal mock-srt-data)))
    (it "ensures a single newline after the last subtitle."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (goto-char (point-max))
       (while (eq (char-before (point-max)) ?\n)
         (delete-backward-char 1))
       (expect (buffer-string) :not :to-equal mock-srt-data)
       (subed-sanitize)
       (expect (buffer-string) :to-equal mock-srt-data)))
    (it "ensures single newline after last subtitle if text is empty."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-text 3)
       (kill-whole-line)
       (expect (buffer-string) :to-equal (concat "1\n"
                                                 "00:01:01,000 --> 00:01:05,123\n"
                                                 "Foo.\n\n"
                                                 "2\n"
                                                 "00:02:02,234 --> 00:02:10,345\n"
                                                 "Bar.\n\n"
                                                 "3\n"
                                                 "00:03:03,45 --> 00:03:15,5\n"
                                                 ""))
       (subed-sanitize)
       (expect (buffer-string) :to-equal (concat "1\n"
                                                 "00:01:01,000 --> 00:01:05,123\n"
                                                 "Foo.\n\n"
                                                 "2\n"
                                                 "00:02:02,234 --> 00:02:10,345\n"
                                                 "Bar.\n\n"
                                                 "3\n"
                                                 "00:03:03,45 --> 00:03:15,5\n"
                                                 "\n"))))
    (it "ensures single space before and after time separators."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (goto-char (point-min))
       (re-search-forward " --> ")
       (replace-match "  --> ")
       (re-search-forward " --> ")
       (replace-match " -->  ")
       (re-search-forward " --> ")
       (replace-match "-->")
       (expect (buffer-string) :not :to-equal mock-srt-data)
       (subed-sanitize)
       (expect (buffer-string) :to-equal mock-srt-data)))
    (it "does not insert newline in empty buffer."
      (with-temp-srt-buffer
       (expect (buffer-string) :to-equal "")
       (subed-sanitize)
       (expect (buffer-string) :to-equal "")))
    (it "runs before saving."
      (with-temp-srt-buffer
        (insert mock-srt-data)
        (goto-char (point-min))
        (re-search-forward " --> ")
        (replace-match "  --> ")
        (re-search-forward " --> ")
        (replace-match " -->  ")
        (re-search-forward " --> ")
        (replace-match "-->")
        (spy-on 'subed-sanitize :and-call-through)
        (expect (buffer-string) :not :to-equal mock-srt-data)
        (subed-prepare-to-save)
        (expect 'subed-sanitize :to-have-been-called)
        (expect (buffer-string) :to-equal mock-srt-data))))

  (describe "Renumbering"
    (it "ensures consecutive subtitle IDs."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (goto-char (point-min))
       (while (looking-at "^[0-9]$")
         (replace-match "123"))
       (expect (buffer-string) :not :to-equal mock-srt-data)
       (subed-regenerate-ids)
       (expect (buffer-string) :to-equal mock-srt-data)))
    (it "runs before saving."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (goto-char (point-min))
       (while (looking-at "^[0-9]$")
         (replace-match "123"))
       (expect (buffer-string) :not :to-equal mock-srt-data)
       (subed-prepare-to-save)
       (expect (buffer-string) :to-equal mock-srt-data)))
    (it "does not modify the kill-ring."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (kill-new "asdf")
       (goto-char (point-min))
       (while (looking-at "^[0-9]$")
         (insert "555"))
       (subed-regenerate-ids)
       (expect (car kill-ring) :to-equal "asdf")))
    (it "does not modify empty buffer."
      (with-temp-srt-buffer
       (subed-regenerate-ids)
       (expect (buffer-string) :to-equal "")))
    )

  (describe "Sorting"
    (it "orders subtitles by start time."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (goto-char (point-min))
       (re-search-forward "01:01")
       (replace-match "12:01")
       (goto-char (point-min))
       (re-search-forward "02:02")
       (replace-match "10:02")
       (goto-char (point-min))
       (re-search-forward "03:03")
       (replace-match "11:03")
       (subed-sort)
       (expect (buffer-string) :to-equal
               (concat
                "1\n"
                "00:10:02,234 --> 00:02:10,345\n"
                "Bar.\n"
                "\n"
                "2\n"
                "00:11:03,45 --> 00:03:15,5\n"
                "Baz.\n"
                "\n"
                "3\n"
                "00:12:01,000 --> 00:01:05,123\n"
                "Foo.\n"))))
    (describe "preserves point in the current subtitle"
      (it "when subtitle text is non-empty."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (goto-char (point-min))
         (re-search-forward "01:01")
         (replace-match "12:01")
         (search-forward "\n")
         (expect (current-word) :to-equal "Foo")
         (subed-sort)
         (expect (current-word) :to-equal "Foo")))
      (it "when subtitle text is empty."
        (with-temp-srt-buffer
         (insert "1\n00:12:01,000 --> 00:01:05,123\n")
         (goto-char (point-max))
         (subed-sort)
         (expect (point) :to-equal 33)))
      )
    )
  (describe "Converting msecs to timestamp"
    (it "uses the right format"
      (with-temp-srt-buffer
       (expect (subed-msecs-to-timestamp 1401) :to-equal "00:00:01,401"))))

  (describe "Merging with next subtitle"
    (it "throws an error in an empty buffer."
      (with-temp-srt-buffer
       (expect (subed-merge-with-next) :to-throw 'error)))
    (it "throws an error with the last subtitle."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-text 3)
       (expect (subed-merge-with-next) :to-throw 'error)))
    (it "combines the text and the time."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-text 2)
       (subed-merge-with-next)
       (expect (subed-subtitle-text) :to-equal "Bar.\nBaz.")
       (expect (subed-subtitle-msecs-start) :to-equal 122234)
       (expect (subed-subtitle-msecs-stop) :to-equal 195500)))
    (it "handles lines that are all numbers."
      (with-temp-srt-buffer
       (insert "1
00:00:00,000 --> 00:00:01,000
This is first subtitle.

2
00:00:01,000 --> 00:00:02,000
This is the second subtitle.

3
00:00:02,000 --> 00:00:03,000
This is the third subtitle.

4
00:00:03,000 --> 00:00:05,000
This is the fourth subtitle.
123456789

5
00:00:05,000 --> 00:00:06,000
This is the sixth subtitle.")
       (re-search-backward "This is the fourth subtitle")
       (subed-merge-with-next)
       (expect (buffer-string) :to-equal
               "1
00:00:00,000 --> 00:00:01,000
This is first subtitle.

2
00:00:01,000 --> 00:00:02,000
This is the second subtitle.

3
00:00:02,000 --> 00:00:03,000
This is the third subtitle.

4
00:00:03,000 --> 00:00:06,000
This is the fourth subtitle.
123456789
This is the sixth subtitle."))))

  (describe "A comment"
    (it "is validated."
      (with-temp-srt-buffer
       (insert mock-srt-data "\n\n4\n00:04:00,000 --> 00:05:00,000\n{\\This is a comment} Hello\n")
       (subed-validate)
       (expect (point) :to-equal (point-max))))
    (it "is highlighted as a comment."
      (with-temp-srt-buffer
       (insert mock-srt-data "\n\n4\n00:04:00,000 --> 00:05:00,000\n{\\This is a comment} Hello\n")
       (re-search-backward "comment")
       (expect (nth 4 (syntax-ppss)) :to-be t)
       (re-search-forward "Hello")
       (expect (nth 4 (syntax-ppss)) :to-be nil))))
  (describe "Font-locking"
    (it "recognizes SRT syntax."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (font-lock-fontify-buffer)
       (goto-char (point-min))
       (re-search-forward "00:01:01")
       (expect (face-at-point) :to-equal 'subed-time-face)
       (re-search-forward "-->")
       (backward-char 1)
       (expect (face-at-point) :to-equal 'subed-time-separator-face)
       (re-search-forward "^2$")       
       (goto-char (match-beginning 0))       
       (expect (face-at-point) :to-equal 'subed-id-face)))))
