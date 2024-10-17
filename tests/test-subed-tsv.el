;; -*- lexical-binding: t; eval: (buttercup-minor-mode) -*-

(add-to-list 'load-path "./subed")
(require 'subed)
(require 'subed-tsv)

(defvar mock-tsv-data
  "11.120000\t14.000000\tHello, world!
14.000000\t16.800000\tThis is a test.
17.000000\t19.800000\tI hope it works.
")

(defmacro with-temp-tsv-buffer (&rest body)
  "Call `subed-tsv--init' in temporary buffer before running BODY."
  `(with-temp-buffer
     (subed-tsv-mode)
     (progn ,@body)))

(describe "subed-tsv"
  (describe "Getting"
    (describe "the subtitle start/stop time"
      (it "returns the time in milliseconds."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-id "14.000000")
         (expect (floor (subed-subtitle-msecs-start)) :to-equal 14000)
         (expect (floor (subed-subtitle-msecs-stop)) :to-equal 16800)))
      (it "returns nil if time can't be found."
        (with-temp-tsv-buffer
         (expect (subed-subtitle-msecs-start) :to-be nil)
         (expect (subed-subtitle-msecs-stop) :to-be nil))))
    (describe "the subtitle start position"
      (it "returns the start from inside a subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (re-search-backward "This is")
         (expect (subed-subtitle-start-pos) :to-equal (line-beginning-position))))
      (it "returns the start from the beginning of the line."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (re-search-backward "14\\.0000")
         (expect (subed-subtitle-start-pos) :to-equal (line-beginning-position)))))
    (describe "the subtitle text"
			(describe "when text is empty"
				(it "and at the beginning with a trailing newline."
					(with-temp-tsv-buffer
					 (insert mock-tsv-data)
					 (subed-jump-to-subtitle-text "14.000000")
					 (kill-line)
					 (expect (subed-subtitle-text) :to-equal "")))))
      (describe "when text is not empty"
        (it "and has no linebreaks."
          (with-temp-tsv-buffer
           (insert mock-tsv-data)
           (subed-jump-to-subtitle-text "14.000000")
           (expect (subed-subtitle-text) :to-equal "This is a test.")))))
  (describe "Converting to msecs"
    (it "works with numbers, although these use seconds because that's what TSV uses."
      (expect (with-temp-tsv-buffer
               (floor (subed-to-msecs "5.123"))) :to-equal 5123))
    (it "works with numbers."
      (expect (subed-to-msecs 5123) :to-equal 5123)))
  (describe "Jumping"
    (describe "to current subtitle timestamp"
      (it "can handle different formats of timestamps."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (expect (subed-jump-to-subtitle-id "11.120") :to-equal 1)
         (expect (floor (subed-subtitle-msecs-start)) :to-equal 11120)))
      (it "returns timestamp's point when point is already on the timestamp."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (goto-char (point-min))
         (subed-jump-to-subtitle-id "11.120000")
         (expect (subed-jump-to-subtitle-time-start) :to-equal (point))
         (expect (looking-at subed-tsv--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "11.120000")))
      (it "returns timestamp's point when point is on the text."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (search-backward "test")
         (expect (thing-at-point 'word) :to-equal "test")
         (expect (subed-jump-to-subtitle-time-start) :to-equal 35)
         (expect (looking-at subed-tsv--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "14.000000")))
      (it "returns nil if buffer is empty."
        (with-temp-tsv-buffer
         (expect (buffer-string) :to-equal "")
         (expect (subed-jump-to-subtitle-time-start) :to-equal nil))))
    (describe "to specific subtitle by timestamp"
      (it "returns timestamp's point if wanted time exists."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (goto-char (point-max))
         (expect (subed-jump-to-subtitle-id "11.12") :to-equal 1)
         (expect (looking-at (regexp-quote "11.120000\t14.000000\tHello, world!")) :to-be t)
         (expect (subed-jump-to-subtitle-id "17.00") :to-equal 71)
         (expect (looking-at (regexp-quote "17.000000\t19.800000\tI hope it works.")) :to-be t)))
      (it "returns nil and does not move if wanted ID does not exists."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (goto-char (point-min))
         (search-forward "test")
         (let ((stored-point (point)))
           (expect (subed-jump-to-subtitle-id "8.00") :to-equal nil)
           (expect stored-point :to-equal (point))))))
    (describe "to subtitle start time"
      (it "returns start time's point if movement was successful."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (re-search-backward "world")
         (expect (subed-jump-to-subtitle-time-start) :to-equal 1)
         (expect (looking-at subed-tsv--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "11.120000")))
      (it "returns nil if movement failed."
        (with-temp-tsv-buffer
         (expect (subed-jump-to-subtitle-time-start) :to-equal nil))))
    (describe "to subtitle stop time"
      (it "returns stop time's point if movement was successful."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (re-search-backward "test")
         (expect (subed-jump-to-subtitle-time-stop) :to-equal 45)
         (expect (looking-at subed-tsv--regexp-timestamp) :to-be t)
         (expect (match-string 0) :to-equal "16.800000")))
      (it "returns nil if movement failed."
        (with-temp-tsv-buffer
         (expect (subed-jump-to-subtitle-time-stop) :to-equal nil))))
    (describe "to subtitle text"
      (it "returns subtitle text's point if movement was successful."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (goto-char (point-min))
         (expect (subed-jump-to-subtitle-text) :to-equal 21)
         (expect (looking-at "Hello, world!") :to-equal t)
         (forward-line 1)
         (expect (subed-jump-to-subtitle-text) :to-equal 55)
         (expect (looking-at "This is a test.") :to-equal t)))
      (it "returns nil if movement failed."
        (with-temp-tsv-buffer
         (expect (subed-jump-to-subtitle-text) :to-equal nil))))
    (describe "to end of subtitle text"
      (it "returns point if subtitle end can be found."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (goto-char (point-min))
         (expect (subed-jump-to-subtitle-end) :to-be 34)
         (expect (looking-back "Hello, world!") :to-be t)
         (forward-char 2)
         (expect (subed-jump-to-subtitle-end) :to-be 70)
         (expect (looking-back "This is a test.") :to-be t)
         (forward-char 2)
         (expect (subed-jump-to-subtitle-end) :to-be 107)
         (expect (looking-back "I hope it works.") :to-be t)))
      (it "returns nil if subtitle end cannot be found."
        (with-temp-tsv-buffer
         (expect (subed-jump-to-subtitle-end) :to-be nil)))
      (it "returns nil if point did not move."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-text "11.12")
         (subed-jump-to-subtitle-end)
         (expect (subed-jump-to-subtitle-end) :to-be nil)))
      (it "works if text is empty."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-text "11.12")
         (kill-line)
         (backward-char)
         (expect (subed-jump-to-subtitle-end) :to-be 21))))
    (describe "to next subtitle ID"
      (it "returns point when there is a next subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-id "11.12")
         (expect (subed-forward-subtitle-id) :to-be 35)
         (expect (looking-at (regexp-quote "14.00")) :to-be t)))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-tsv-buffer
         (expect (thing-at-point 'word) :to-equal nil)
         (expect (subed-forward-subtitle-id) :to-be nil))
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-text "17.00")
         (expect (subed-forward-subtitle-id) :to-be nil))))
    (describe "to previous subtitle ID"
      (it "returns point when there is a previous subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-text "14.00")
         (expect (subed-backward-subtitle-id) :to-be 1)))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-tsv-buffer
         (expect (subed-backward-subtitle-id) :to-be nil))
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-id "11.12")
         (expect (subed-backward-subtitle-id) :to-be nil))))
    (describe "to next subtitle text"
      (it "returns point when there is a next subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-id "14.00")
         (expect (subed-forward-subtitle-text) :to-be 91)
         (expect (thing-at-point 'word) :to-equal "I")))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-tsv-buffer
         (goto-char (point-max))
         (insert (concat mock-tsv-data "\n\n"))
         (subed-jump-to-subtitle-id "17.00")
         (expect (subed-forward-subtitle-text) :to-be nil))))
    (describe "to previous subtitle text"
      (it "returns point when there is a previous subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-id "14.00")
         (expect (subed-backward-subtitle-text) :to-be 21)
         (expect (thing-at-point 'word) :to-equal "Hello")))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (goto-char (point-min))
         (expect (looking-at (regexp-quote "11.12")) :to-be t)
         (expect (subed-backward-subtitle-text) :to-be nil)
         (expect (looking-at (regexp-quote "11.12")) :to-be t))))
    (describe "to next subtitle end"
      (it "returns point when there is a next subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-text "14.00")
         (expect (thing-at-point 'word) :to-equal "This")
         (expect (subed-forward-subtitle-end) :to-be 107)))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-tsv-buffer
         (insert (concat mock-tsv-data "\n\n"))
         (subed-jump-to-subtitle-text "17.00")
         (expect (subed-forward-subtitle-end) :to-be nil))))
    (describe "to previous subtitle end"
      (it "returns point when there is a previous subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-id "14.00")
         (expect (subed-backward-subtitle-end) :to-be 34)))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (goto-char (point-min))
         (expect (looking-at (regexp-quote "11.12")) :to-be t)
         (expect (subed-backward-subtitle-text) :to-be nil)
         (expect (looking-at (regexp-quote "11.12")) :to-be t))))
    (describe "to next subtitle start time"
      (it "returns point when there is a next subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-id "14.00")
         (expect (subed-forward-subtitle-time-start) :to-be 71)))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-id "17.00")
         (let ((pos (point)))
           (expect (subed-forward-subtitle-time-start) :to-be nil)
           (expect (point) :to-be pos)))))
    (describe "to previous subtitle stop"
      (it "returns point when there is a previous subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-id "14.00")
         (expect (subed-backward-subtitle-time-stop) :to-be 11)))
      (it "returns nil and doesn't move when there is no previous subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (goto-char (point-min))
         (expect (subed-backward-subtitle-time-stop) :to-be nil)
         (expect (looking-at (regexp-quote "11.12")) :to-be t))))
    (describe "to next subtitle stop time"
      (it "returns point when there is a next subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-id "14.00")
         (expect (subed-forward-subtitle-time-stop) :to-be 81)))
      (it "returns nil and doesn't move when there is no next subtitle."
        (with-temp-tsv-buffer
         (insert mock-tsv-data)
         (subed-jump-to-subtitle-id "17.00")
         (let ((pos (point)))
           (expect (subed-forward-subtitle-time-stop) :to-be nil)
           (expect (point) :to-be pos))))))

  (describe "Setting start/stop time"
    (it "of subtitle should set it."
      (with-temp-tsv-buffer
       (insert mock-tsv-data)
       (subed-jump-to-subtitle-id "14.00")
       (subed-set-subtitle-time-start (+ (* 15 1000) 400))
       (expect (floor (subed-subtitle-msecs-start)) :to-be (+ (* 15 1000) 400)))))

  (describe "Inserting a subtitle"
    (describe "in an empty buffer"
      (describe "before the current subtitle"
        (it "creates an empty subtitle when passed nothing."
          (with-temp-tsv-buffer
           (subed-prepend-subtitle)
           (expect (buffer-string) :to-equal "0.000000\t1.000000\t\n")))
        (it "creates a subtitle with a start time."
          (with-temp-tsv-buffer
           (subed-prepend-subtitle nil 12340)
           (expect (buffer-string) :to-equal "12.340000\t13.340000\t\n")))
        (it "creates a subtitle with a start time and stop time."
          (with-temp-tsv-buffer
           (subed-prepend-subtitle nil 60000 65000)
           (expect (buffer-string) :to-equal "60.000000\t65.000000\t\n")))
        (it "creates a subtitle with start time, stop time and text."
          (with-temp-tsv-buffer
           (subed-prepend-subtitle nil 60000 65000 "Hello world")
           (expect (buffer-string) :to-equal "60.000000\t65.000000\tHello world\n"))))
      (describe "after the current subtitle"
        (it "creates an empty subtitle when passed nothing."
          (with-temp-tsv-buffer
           (subed-append-subtitle)
           (expect (buffer-string) :to-equal "0.000000\t1.000000\t\n")))
        (it "creates a subtitle with a start time."
          (with-temp-tsv-buffer
           (subed-append-subtitle nil 12340)
           (expect (buffer-string) :to-equal "12.340000\t13.340000\t\n")))
        (it "creates a subtitle with a start time and stop time."
          (with-temp-tsv-buffer
           (subed-append-subtitle nil 60000 65000)
           (expect (buffer-string) :to-equal "60.000000\t65.000000\t\n")))
        (it "creates a subtitle with start time, stop time and text."
          (with-temp-tsv-buffer
           (subed-append-subtitle nil 60000 65000 "Hello world")
           (expect (buffer-string) :to-equal "60.000000\t65.000000\tHello world\n"))))))
  (describe "in a non-empty buffer"
    (describe "before the current subtitle"
      (describe "with point on the first subtitle"
        (it "creates the subtitle before the current one."
          (with-temp-tsv-buffer
           (insert mock-tsv-data)
           (subed-jump-to-subtitle-time-stop)
           (subed-prepend-subtitle)
           (expect (buffer-substring (line-beginning-position) (line-end-position))
                   :to-equal "0.000000\t1.000000\t"))))
      (describe "with point on a middle subtitle"
        (it "creates the subtitle before the current one."
          (with-temp-tsv-buffer
           (insert mock-tsv-data)
           (subed-jump-to-subtitle-time-stop "14.00")
           (subed-prepend-subtitle)
           (expect (buffer-substring (line-beginning-position) (line-end-position))
                   :to-equal "0.000000\t1.000000\t")
           (forward-line 1)
           (beginning-of-line)
           (expect (looking-at "14.00"))))))
    (describe "after the current subtitle"
      (describe "with point on a subtitle"
        (it "creates the subtitle after the current one."
          (with-temp-tsv-buffer
           (insert mock-tsv-data)
           (subed-jump-to-subtitle-time-stop "14.00")
           (subed-append-subtitle)
           (expect (buffer-substring (line-beginning-position) (line-end-position))
                   :to-equal "0.000000\t1.000000\t")
           (forward-line -1)
           (expect (floor (subed-subtitle-msecs-start)) :to-be 14000))))))
  (describe "Killing a subtitle"
    (it "removes the first subtitle."
      (with-temp-tsv-buffer
       (insert mock-tsv-data)
       (subed-jump-to-subtitle-text "11.12")
       (subed-kill-subtitle)
       (expect (floor (subed-subtitle-msecs-start)) :to-be 14000)
       (forward-line -1)
       (beginning-of-line)
       (expect (looking-at "14\\.00000")))))
  (it "removes it in between."
    (with-temp-tsv-buffer
     (insert mock-tsv-data)
     (subed-jump-to-subtitle-text "14.00")
     (subed-kill-subtitle)
     (expect (floor (subed-subtitle-msecs-start)) :to-be 17000)))
  (it "removes the last subtitle."
    (with-temp-tsv-buffer
     (insert mock-tsv-data)
     (subed-jump-to-subtitle-text "17.00")
     (subed-kill-subtitle)
     (expect (buffer-string) :to-equal
             "11.120000\t14.000000\tHello, world!
14.000000\t16.800000\tThis is a test.
")))
  (describe "Merging"
    (it "is limited to the region when at the start of the line."
      (with-temp-tsv-buffer
       (insert "5.673000	5.913000	phone
5.953000	6.013000	to
6.053000	6.213000	write
6.253000	6.333000	the
6.373000	6.713000	text,
")
       (goto-char (point-min))
       (forward-line 3)
       (subed-merge-region (point-min) (point))
       (expect (buffer-string) :to-equal
"5.673000	6.213000	phone to write
6.253000	6.333000	the
6.373000	6.713000	text,
"
               )
       )))
  (describe "Converting msecs to timestamp"
    (it "uses the right format"
      (with-temp-tsv-buffer
       (expect (subed-msecs-to-timestamp 1410) :to-equal "1.410000")))))
