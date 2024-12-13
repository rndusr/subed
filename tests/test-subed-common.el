;; -*- lexical-binding: t; eval: (buttercup-minor-mode) -*-

(load-file "./tests/undercover-init.el")
(require 'subed-srt)
(require 'subed-mpv)

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
  "Call `subed-srt--init' in temporary buffer before running BODY."
  (declare (indent defun))
  `(with-temp-buffer
     ;; subed--init uses file extension to detect format
     (subed-srt-mode)
     (progn ,@body)))

(cl-defun create-sample-media-file (&key
                                    path
                                    duration-video-stream
                                    duration-audio-stream)
  "Create a sample media file.

PATH is the absolute path for the output file. It must be a
string.

AUDIO-DURATION is the duration in seconds for the audio
stream. It must be a number.

VIDEO-DURATION is the duration in seconds for the video stream. It
must be a number."
  (apply 'call-process
         ;; The ffmpeg command shown below can create files with the
         ;; extensions shown below (tested using ffmpeg version
         ;; 4.4.2-0ubuntu0.22.04.1)
         ;; + audio extensions: wav ogg mp3 opus m4a
         ;; + video extensions: mkv mp4 webm avi ts ogv"
         "ffmpeg"
         nil
         nil
         nil
         "-v" "error"
         "-y"
         (append
           ;; Create the video stream
           (when duration-video-stream
             (list "-f" "lavfi" "-i" (format "testsrc=size=100x100:duration=%d" duration-video-stream)))
           ;; Create the audio stream
           (when duration-audio-stream
             (list "-f" "lavfi" "-i" (format "sine=frequency=1000:duration=%d" duration-audio-stream)))
           (list path)))
   path)

(defmacro test-subed-extension (extension &optional has-video)
  `(it ,(if has-video
            (format "reports the duration of %s even with a longer video stream" extension)
          (format "reports the duration of %s" extension))
     (let* (;; `duration-audio-stream' is the duration in seconds for
            ;; the media file that is used inside the tests.  When
            ;; `duration-audio-stream' is an integer, ffprobe might
            ;; report a duration that is slightly greater, so we can't
            ;; expect the duration reported by ffprobe to be equal to
            ;; the duration that we passed to ffmpeg when creating the
            ;; sample media file. For this reason, we define the
            ;; variables `duration-lower-boundary' and
            ;; `duration-upper-boundary' to set a tolerance to the
            ;; reported value by ffprobe.
            ;;
            ;; When `duration-audio-stream' changes, the variables
            ;; `duration-lower-boundary' and
            ;; `duration-upper-boundary' should be set accordingly."
            (duration-audio-stream 3)
            (duration-video-stream 5)
            (duration-lower-boundary 3000)
            (duration-upper-boundary 4000)
            (filename (make-temp-file "test-subed-a" nil ,extension))
            (file
             (create-sample-media-file
              :path filename
              :duration-audio-stream duration-audio-stream
              :duration-video-stream ,(if has-video
                                          'duration-video-stream
                                        nil)))
            (duration-ms (subed-ffprobe-duration-ms filename)))
       (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
       (expect duration-ms :to-be-less-than duration-upper-boundary)
       (delete-file filename))))

(describe "subed-common"
  (describe "Iterating over subtitles"
		(describe "without providing beginning and end"
			(it "goes through each subtitle."
				(with-temp-srt-buffer
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-time-stop 1)
				 (subed-for-each-subtitle nil nil nil
					 (expect (looking-at "^[0-9]$") :to-be t)
					 (forward-line 2)
					 (kill-line)
					 (insert "Hello."))
				 (expect (subed-subtitle-text 1) :to-equal "Hello.")
				 (expect (subed-subtitle-text 2) :to-equal "Bar.")
				 (expect (subed-subtitle-text 3) :to-equal "Baz.")
				 (expect (point) :to-equal 20)
				 (subed-jump-to-subtitle-time-stop 2)
				 (subed-for-each-subtitle nil nil nil
					 (expect (looking-at "^[0-9]$") :to-be t)
					 (forward-line 2)
					 (kill-line)
					 (insert "HEllo."))
				 (expect (subed-subtitle-text 1) :to-equal "Hello.")
				 (expect (subed-subtitle-text 2) :to-equal "HEllo.")
				 (expect (subed-subtitle-text 3) :to-equal "Baz.")
				 (expect (point) :to-equal 60)
				 (subed-jump-to-subtitle-time-stop 3)
				 (subed-for-each-subtitle nil nil nil
					 (expect (looking-at "^[0-9]$") :to-be t)
					 (forward-line 2)
					 (kill-line)
					 (insert "HELlo."))
				 (expect (subed-subtitle-text 1) :to-equal "Hello.")
				 (expect (subed-subtitle-text 2) :to-equal "HEllo.")
				 (expect (subed-subtitle-text 3) :to-equal "HELlo.")
				 (expect (point) :to-equal 99))))
		(describe "providing only the beginning"
			(it "goes forward."
				(with-temp-srt-buffer
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-time-start 1)
				 (expect (point) :to-equal 3)
				 (let ((new-texts (list "A" "B" "C")))
					 (subed-for-each-subtitle 71 nil nil
						 (expect (looking-at "^[0-9]$") :to-be t)
						 (forward-line 2)
						 (kill-line)
						 (insert (pop new-texts))))
				 (expect (subed-subtitle-text 1) :to-equal "Foo.")
				 (expect (subed-subtitle-text 2) :to-equal "A")
				 (expect (subed-subtitle-text 3) :to-equal "B")
				 (expect (point) :to-equal 3)))
			(it "goes backward."
				(with-temp-srt-buffer
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-time-stop 3)
				 (expect (point) :to-equal 95)
				 (let ((new-texts (list "A" "B" "C")))
					 (subed-for-each-subtitle 75 nil :reverse
						 (expect (looking-at "^[0-9]$") :to-be t)
						 (forward-line 2)
						 (kill-line)
						 (insert (pop new-texts))))
				 (expect (subed-subtitle-text 1) :to-equal "Foo.")
				 (expect (subed-subtitle-text 2) :to-equal "B")
				 (expect (subed-subtitle-text 3) :to-equal "A")
				 (expect (point) :to-equal 92)))
			)
		(describe "providing beginning and end,"
			(describe "excluding subtitles above"
				(it "goes forward."
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-time-stop 1)
					 (expect (point) :to-equal 20)
					 (let ((new-texts (list "A" "B" "C")))
						 (subed-for-each-subtitle 71 79 nil
							 (expect (looking-at "^[0-9]$") :to-be t)
							 (forward-line 2)
							 (kill-line)
							 (insert (pop new-texts))))
					 (expect (subed-subtitle-text 1) :to-equal "Foo.")
					 (expect (subed-subtitle-text 2) :to-equal "A")
					 (expect (subed-subtitle-text 3) :to-equal "B")
					 (expect (point) :to-equal 20)))
				(it "goes backward."
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-time-start 3)
					 (expect (point) :to-equal 79)
					 (let ((new-texts (list "A" "B" "C")))
						 (subed-for-each-subtitle 39 77 :reverse
							 (expect (looking-at "^[0-9]$") :to-be t)
							 (forward-line 2)
							 (kill-line)
							 (insert (pop new-texts))))
					 (expect (subed-subtitle-text 1) :to-equal "Foo.")
					 (expect (subed-subtitle-text 2) :to-equal "B")
					 (expect (subed-subtitle-text 3) :to-equal "A")
					 (expect (point) :to-equal 76))))
			(describe "excluding subtitles below"
				(it "goes forward."
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-text 3)
					 (expect (point) :to-equal 106)
					 (let ((new-texts (list "A" "B" "C")))
						 (subed-for-each-subtitle 5 76 nil
							 (expect (looking-at "^[0-9]$") :to-be t)
							 (forward-line 2)
							 (kill-line)
							 (insert (pop new-texts))))
					 (expect (subed-subtitle-text 1) :to-equal "A")
					 (expect (subed-subtitle-text 2) :to-equal "B")
					 (expect (subed-subtitle-text 3) :to-equal "Baz.")
					 (expect (point) :to-equal 100)))
				(it "goes backward."
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-time-stop 2)
					 (expect (point) :to-equal 58)
					 (let ((new-texts (list "A" "B" "C")))
						 (subed-for-each-subtitle 20 76 :reverse
							 (expect (looking-at "^[0-9]$") :to-be t)
							 (forward-line 2)
							 (kill-line)
							 (insert (pop new-texts))))
					 (expect (subed-subtitle-text 1) :to-equal "B")
					 (expect (subed-subtitle-text 2) :to-equal "A")
					 (expect (subed-subtitle-text 3) :to-equal "Baz.")
					 (expect (point) :to-equal 55)))
				)
			))
	(describe "Getting the maximum subtitle ID"
		(it "returns nil in an empty buffer."
			(with-temp-srt-buffer
			 (expect (subed-subtitle-id-max) :to-be nil)))
		(it "returns the subtitle ID at the end."
			(with-temp-srt-buffer
			 (insert mock-srt-data)
			 (expect (subed-subtitle-id-max) :to-be 3))))
	(describe "Setting subtitle start time"
		(it "continues when setting the first subtitle's start time."
			(with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-jump-to-subtitle-id 1)
			 (subed-set-subtitle-time-start 30000)
			 (expect (subed-subtitle-msecs-start) :to-equal 30000)))
		(it "ignores the previous subtitle's stop time if there's enough spacing."
			(with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-jump-to-subtitle-id 2)
			 (let ((subed-enforce-time-boundaries 'error)
						 (subed-subtitle-spacing 100))
				 (subed-set-subtitle-time-start (subed-timestamp-to-msecs "00:01:10,000")))
			 (expect (subed-subtitle-msecs-start) :to-equal (subed-timestamp-to-msecs "00:01:10,000"))
			 (expect (subed-subtitle-msecs-stop 1) :to-equal (subed-timestamp-to-msecs "00:01:05,123"))))
		(describe "when it overlaps with the previous subtitle"
			(it "ignores the previous subtitle's stop time if spacing is unspecified."
				(let ((subed-subtitle-spacing nil)
							(subed-enforce-time-boundaries 'error))
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 2)
					 (subed-set-subtitle-time-start (subed-timestamp-to-msecs "00:01:05,000"))
					 (expect (subed-subtitle-msecs-start) :to-equal (subed-timestamp-to-msecs "00:01:05,000"))
					 (expect (subed-subtitle-msecs-stop 1) :to-equal (subed-timestamp-to-msecs "00:01:05,123")))))
			(describe "when time boundaries are enforced by errors"
				(it "reports an error if the change violates spacing."
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 2)
					 (let ((subed-enforce-time-boundaries 'error)
								 (subed-subtitle-spacing 100))
						 (expect (subed-set-subtitle-time-start (subed-timestamp-to-msecs "00:01:05,000")) :to-throw 'error)))))
			(describe "when time boundaries are enforced by clipping"
				(before-each
					(setq subed-enforce-time-boundaries 'clip
								subed-subtitle-spacing 100))
				(it "clips to preserve spacing based on the previous subtitle's stop time."
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 2)
					 (subed-set-subtitle-time-start (subed-timestamp-to-msecs "00:01:05,000"))
					 (expect (subed-subtitle-msecs-start) :to-equal (subed-timestamp-to-msecs "00:01:05,223"))
					 (expect (subed-subtitle-msecs-stop 1) :to-equal (subed-timestamp-to-msecs "00:01:05,123")))))
			(describe "when time boundaries are enforced by adjusting"
				(before-each
					(setq subed-enforce-time-boundaries 'adjust
								subed-subtitle-spacing 100))
				(it "adjusts the previous subtitle's stop time to maintain spacing."
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 2)
					 (subed-set-subtitle-time-start (subed-timestamp-to-msecs "00:01:05,000"))
					 (expect (subed-subtitle-msecs-start) :to-equal (subed-timestamp-to-msecs "00:01:05,000"))
					 (expect (subed-subtitle-msecs-stop 1) :to-equal (subed-timestamp-to-msecs "00:01:04,900"))))
				(it "adjusts the previous subtitle's stop time, but not the one before it."
					;; TODO: Decide if we want to change this expectation
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 3)
					 (subed-set-subtitle-time-start (subed-timestamp-to-msecs "00:01:05,000"))
					 (expect (subed-subtitle-msecs-start) :to-equal (subed-timestamp-to-msecs "00:01:05,000"))
					 (expect (subed-subtitle-msecs-stop 2) :to-equal (subed-timestamp-to-msecs "00:01:04,900"))
					 (expect (subed-subtitle-msecs-stop 1) :to-equal (subed-timestamp-to-msecs "00:01:05,123"))))
				(it "adjusts the current subtitle's stop time to at least the start time."
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 2)
					 (let ((subed-enforce-time-boundaries 'adjust)
								 (subed-subtitle-spacing 100))
						 (subed-set-subtitle-time-start (subed-timestamp-to-msecs "00:02:11,000")))
					 (expect (subed-subtitle-msecs-start) :to-equal (subed-timestamp-to-msecs "00:02:11,000"))
					 (expect (subed-subtitle-msecs-stop 1) :to-equal (subed-timestamp-to-msecs "00:01:05,123"))))))
		(describe "when it will result in invalid duration"
			:var ((temp-time (+ (* 3 60 1000) (* 17 1000))))
			(it "throws an error when enforcing time boundaries."
				(let ((subed-enforce-time-boundaries 'error))
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 3)
					 (expect (subed-set-subtitle-time-start temp-time)
									 :to-throw 'error))))
			(it "clips the current subtitle's start time to at most the stop time."
				(with-temp-srt-buffer
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-id 3)
				 (let ((subed-enforce-time-boundaries 'clip)
							 (subed-subtitle-spacing 100))
					 (subed-set-subtitle-time-start temp-time))
				 (expect (subed-subtitle-msecs-start) :to-equal (subed-timestamp-to-msecs "00:03:15,500"))))
			(it "changes it when ignoring time boundaries."
				(let ((subed-enforce-time-boundaries nil))
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 3)
					 (subed-set-subtitle-time-start temp-time)
					 (expect (subed-subtitle-msecs-start) :to-equal temp-time))))
			(it "changes it when negative durations are allowed."
				(let ((subed-enforce-time-boundaries 'error))
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 3)
					 (subed-set-subtitle-time-start temp-time nil t)
					 (expect (subed-subtitle-msecs-start) :to-equal temp-time))))))
	(describe "Setting subtitle stop time"
		(it "continues when setting the last subtitle's stop time."
			(with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-jump-to-subtitle-id 3)
			 (let ((subed-enforce-time-boundaries 'error))
				 (subed-set-subtitle-time-stop (subed-timestamp-to-msecs "00:03:30,000")))
			 (expect (subed-subtitle-msecs-stop) :to-equal (subed-timestamp-to-msecs "00:03:30,000"))))
		(it "ignores the next subtitle's start time if there's enough spacing."
			(with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-jump-to-subtitle-id 2)
			 (let ((subed-enforce-time-boundaries 'error))
				 (subed-set-subtitle-time-stop (subed-timestamp-to-msecs "00:03:01,000")))
			 (expect (subed-subtitle-msecs-stop) :to-equal (subed-timestamp-to-msecs "00:03:01,000"))
			 (expect (subed-subtitle-msecs-start 3) :to-equal (subed-timestamp-to-msecs "00:03:03,45"))))
		(describe "when it overlaps with the next subtitle"
			(it "ignores the next subtitle's start time if spacing is unspecified."
				(with-temp-srt-buffer
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-id 2)
				 (let ((subed-enforce-time-boundaries 'error)
							 (subed-subtitle-spacing nil))
					 (subed-set-subtitle-time-stop (subed-timestamp-to-msecs "00:03:05,000")))
				 (expect (subed-subtitle-msecs-stop) :to-equal (subed-timestamp-to-msecs "00:03:05,000"))
				 (expect (subed-subtitle-msecs-start 3) :to-equal (subed-timestamp-to-msecs "00:03:03,45"))))
			(describe "when time boundaries are enforced by errors"
				(before-each
					(setq subed-subtitle-spacing 100
								subed-enforce-time-boundaries 'error))
				(it "reports an error if the change violates spacing."
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 2)
					 (expect (subed-set-subtitle-time-stop (subed-timestamp-to-msecs "00:03:05,000"))
									 :to-throw 'error))))
			(describe "when time boundaries are enforced by clipping"
				(before-each
					(setq subed-subtitle-spacing 100
								subed-enforce-time-boundaries 'clip))
				(it "clips to the next subtitle's start time to maintain spacing."
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 2)
					 (subed-set-subtitle-time-stop (subed-timestamp-to-msecs "00:03:05,000"))
					 (expect (subed-subtitle-msecs-stop) :to-equal (subed-timestamp-to-msecs "00:03:03,350"))
					 (expect (subed-subtitle-msecs-start 3) :to-equal (subed-timestamp-to-msecs "00:03:03,450")))))
			(describe "when time boundaries are enforced by adjusting"
				(before-each
					(setq subed-subtitle-spacing 100
								subed-enforce-time-boundaries 'adjust))
				(it "adjusts the next subtitle's start time to maintain spacing."
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 2)
					 (subed-set-subtitle-time-stop (subed-timestamp-to-msecs "00:03:05,000"))
					 (expect (subed-subtitle-msecs-stop) :to-equal (subed-timestamp-to-msecs "00:03:05,000"))
					 (expect (subed-subtitle-msecs-start 3) :to-equal (subed-timestamp-to-msecs "00:03:05,100"))))
				(it "adjusts the next subtitle's start time, but not the one after it."
					;; TODO: Decide if we want to change this expectation
					(with-temp-srt-buffer
					 (insert mock-srt-data)
					 (subed-jump-to-subtitle-id 1)
					 (subed-set-subtitle-time-stop (subed-timestamp-to-msecs "00:03:05,000"))
					 (expect (subed-subtitle-msecs-stop) :to-equal (subed-timestamp-to-msecs "00:03:05,000"))
					 (expect (subed-subtitle-msecs-start 2) :to-equal (subed-timestamp-to-msecs "00:03:05,100"))
					 (expect (subed-subtitle-msecs-start 3) :to-equal (subed-timestamp-to-msecs "00:03:03,45"))))))
		(describe "when it will result in invalid duration"
			(it "adjusts the start time as needed."
				(with-temp-srt-buffer
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-id 3)
				 (let ((subed-enforce-time-boundaries 'adjust)
							 (temp-time (subed-timestamp-to-msecs "00:03:01,000")))
					 (subed-set-subtitle-time-stop temp-time)
					 (expect (subed-subtitle-msecs-start) :to-equal temp-time)
					 (expect (subed-subtitle-msecs-stop) :to-equal temp-time))))
			(it "throws an error when enforcing time boundaries."
				(with-temp-srt-buffer
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-id 3)
				 (let ((subed-enforce-time-boundaries 'error)
							 (temp-time (subed-timestamp-to-msecs "00:03:01,000")))
					 (expect (subed-set-subtitle-time-stop temp-time)
									 :to-throw 'error))))
			(it "changes it when ignoring time boundaries."
				(with-temp-srt-buffer
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-id 3)
				 (let ((subed-enforce-time-boundaries nil)
							 (temp-time (subed-timestamp-to-msecs "00:03:01,000")))
					 (subed-set-subtitle-time-stop temp-time)
					 (expect (subed-subtitle-msecs-stop) :to-equal temp-time))))
			(it "changes it when negative durations are allowed."
				(with-temp-srt-buffer
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-id 3)
				 (let ((subed-enforce-time-boundaries 'error)
							 (temp-time (subed-timestamp-to-msecs "00:03:01,000")))
					 (subed-set-subtitle-time-stop temp-time nil t)
					 (expect (subed-subtitle-msecs-stop) :to-equal temp-time)))))
		(describe "when looping"
			(it "updates the loop stop time for the current subtitle."
				(with-temp-srt-buffer
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-text 2)
				 (let ((subed-loop-seconds-before 1)
							 (subed-loop-seconds-after 1))
					 (subed--set-subtitle-loop)
					 (expect subed--subtitle-loop-start :to-equal (subed-timestamp-to-msecs "00:02:01,234"))
					 (expect subed--subtitle-loop-stop :to-equal (subed-timestamp-to-msecs "00:02:11,345"))
					 (subed-set-subtitle-time-stop (subed-timestamp-to-msecs "00:02:13,345"))
					 (expect subed--subtitle-loop-start :to-equal (subed-timestamp-to-msecs "00:02:01,234"))
					 (expect subed--subtitle-loop-stop :to-equal (subed-timestamp-to-msecs "00:02:14,345")))))
			(it "leaves other subtitle loops alone."
				(with-temp-srt-buffer
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-text 2)
				 (let ((subed-loop-seconds-before 1)
							 (subed-loop-seconds-after 1))
					 (subed--set-subtitle-loop)
					 (expect subed--subtitle-loop-start :to-equal (subed-timestamp-to-msecs "00:02:01,234"))
					 (expect subed--subtitle-loop-stop :to-equal (subed-timestamp-to-msecs "00:02:11,345"))
					 (subed-jump-to-subtitle-text 1)
					 (subed-set-subtitle-time-stop (subed-timestamp-to-msecs "00:01:04,123"))
					 (expect subed--subtitle-loop-start :to-equal (subed-timestamp-to-msecs "00:02:01,234"))
					 (expect subed--subtitle-loop-stop :to-equal (subed-timestamp-to-msecs "00:02:11,345")))))))
  (describe "Adjusting subtitle start/stop time"
    :var (subed-subtitle-time-adjusted-hook)
    (it "runs the appropriate hook."
      (let ((foo (setf (symbol-function 'foo) (lambda (msecs) ()))))
        (spy-on 'foo)
        (with-temp-srt-buffer
				 (add-hook 'subed-subtitle-time-adjusted-hook 'foo)
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-id 1)
				 (expect (subed-adjust-subtitle-time-start 100) :to-equal 100)
				 (expect 'foo :to-have-been-called-with 61100)
				 (expect 'foo :to-have-been-called-times 1)
				 (expect (subed-adjust-subtitle-time-stop 123) :to-equal 123)
				 (expect 'foo :to-have-been-called-with 61100)
				 (expect 'foo :to-have-been-called-times 2)
				 (subed-jump-to-subtitle-id 2)
				 (expect (subed-adjust-subtitle-time-start 6) :to-equal 6)
				 (expect 'foo :to-have-been-called-with 122240)
				 (expect 'foo :to-have-been-called-times 3)
				 (expect (subed-adjust-subtitle-time-stop 123) :to-equal 123)
				 (expect 'foo :to-have-been-called-with 122240)
				 (expect 'foo :to-have-been-called-times 4))))
    (it "adjusts the start/stop time."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-jump-to-subtitle-id 1)
			 (expect (subed-adjust-subtitle-time-start 100) :to-equal 100)
			 (expect (save-excursion (subed-jump-to-subtitle-time-start)
															 (thing-at-point 'line)) :to-equal "00:01:01,100 --> 00:01:05,123\n")
			 (expect (subed-adjust-subtitle-time-start -200) :to-equal -200)
			 (expect (save-excursion (subed-jump-to-subtitle-time-start)
															 (thing-at-point 'line)) :to-equal "00:01:00,900 --> 00:01:05,123\n")
			 (expect (subed-adjust-subtitle-time-stop 200) :to-equal 200)
			 (expect (save-excursion (subed-jump-to-subtitle-time-start)
															 (thing-at-point 'line)) :to-equal "00:01:00,900 --> 00:01:05,323\n")
			 (expect (subed-adjust-subtitle-time-stop -100) :to-equal -100)
			 (expect (save-excursion (subed-jump-to-subtitle-time-start)
															 (thing-at-point 'line)) :to-equal "00:01:00,900 --> 00:01:05,223\n")))
    (describe "when enforcing boundaries with errors"
      (describe "when decreasing start time"
        (it "handles the first subtitle."
          (with-temp-srt-buffer
			  	 (insert (concat "1\n"
													 "00:00:01,000 --> 00:00:02,000\n"
													 "Foo.\n"))
					 (let ((subed-enforce-time-boundaries 'error))
						 (expect subed-enforce-time-boundaries :to-equal 'error)
						 (expect (subed-adjust-subtitle-time-start -999) :to-be -999)
						 (expect (subed-subtitle-msecs-start) :to-be 1)
						 (expect (subed-adjust-subtitle-time-start -1) :to-be -1)
						 (expect (subed-subtitle-msecs-start) :to-be 0)
						 (expect (subed-adjust-subtitle-time-start -1) :to-throw 'error))))
        (it "handles a non-first subtitle."
          (with-temp-srt-buffer
					 (insert (concat "1\n"
													 "00:00:01,000 --> 00:00:02,000\n"
													 "Foo.\n\n"
													 "2\n"
													 "00:00:03,000 --> 00:00:04,000\n"
													 "Bar.\n\n"))
					 (subed-jump-to-subtitle-id 2)
					 (let ((subed-enforce-time-boundaries 'error)
								 (subed-subtitle-spacing 100))
						 (expect (subed-adjust-subtitle-time-start -899) :to-be -899)
						 (expect (subed-subtitle-msecs-start) :to-be 2101)
						 (expect (subed-adjust-subtitle-time-start -1) :to-be -1)
						 (expect (subed-subtitle-msecs-start) :to-be 2100)
						 ;; report an error if it bumps up against a previous subtitle
						 (expect (subed-adjust-subtitle-time-start -1) :to-throw 'error)
						 (expect (subed-subtitle-msecs-start) :to-be 2100)))))
      (it "increases start time."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:03,000 --> 00:00:04,000\n"
												 "Bar.\n\n"))
				 (subed-jump-to-subtitle-id 2)
				 (let ((subed-enforce-time-boundaries 'error)
							 (subed-subtitle-spacing 100))
					 (expect (subed-adjust-subtitle-time-start 999) :to-be 999)
					 (expect (subed-subtitle-msecs-start) :to-be 3999)
					 (expect (subed-adjust-subtitle-time-start 1) :to-be 1)
					 (expect (subed-subtitle-msecs-start) :to-be 4000)
					 (expect (subed-adjust-subtitle-time-start 1) :to-throw 'error)
					 (expect (subed-subtitle-msecs-start) :to-be 4000))))
      (it "decreases stop time."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:03,000 --> 00:00:04,000\n"
												 "Bar.\n\n"))
				 (subed-jump-to-subtitle-id 2)
				 (let ((subed-enforce-time-boundaries 'error)
							 (subed-subtitle-spacing 100))
					 (expect (subed-adjust-subtitle-time-stop -999) :to-be -999)
					 (expect (subed-subtitle-msecs-stop) :to-be 3001)
					 (expect (subed-adjust-subtitle-time-stop -1) :to-be -1)
					 (expect (subed-subtitle-msecs-stop) :to-be 3000)
					 (expect (subed-adjust-subtitle-time-stop -1) :to-throw 'error)
					 (expect (subed-subtitle-msecs-stop) :to-be 3000))))
      (describe "when increasing stop time"
        (it "increases the last subtitle."
          (with-temp-srt-buffer
					 (insert (concat "1\n"
													 "00:00:01,000 --> 00:00:02,000\n"
													 "Foo.\n\n"
													 "2\n"
													 "00:00:03,000 --> 00:00:04,000\n"
													 "Bar.\n\n"))
					 (subed-jump-to-subtitle-id 2)
					 (expect (subed-adjust-subtitle-time-stop 1000000):to-be 1000000)
					 (expect (subed-subtitle-msecs-stop) :to-be 1004000)))
        (it "increases a non-last subtitle."
          (with-temp-srt-buffer
					 (insert (concat "1\n"
													 "00:00:01,000 --> 00:00:02,000\n"
													 "Foo.\n\n"
													 "2\n"
													 "00:00:03,000 --> 00:00:04,000\n"
													 "Bar.\n\n"))
					 (subed-jump-to-subtitle-id 1)
					 (expect (subed-adjust-subtitle-time-stop 899) :to-be 899)
					 (expect (subed-subtitle-msecs-stop) :to-be 2899)
					 (expect (subed-adjust-subtitle-time-stop 1) :to-be 1)
					 (expect (subed-subtitle-msecs-stop) :to-be 2900)
					 (let ((subed-enforce-time-boundaries 'error)
								 (subed-subtitle-spacing 100))
						 (expect (subed-adjust-subtitle-time-stop 1) :to-throw 'error)
						 (expect (subed-subtitle-msecs-stop) :to-be 2900)))))
      (it "increases without undershooting the target time."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:02,000 --> 00:00:03,000\n"
												 "Bar.\n"))
				 (subed-jump-to-subtitle-id 1)
				 (let ((subed-enforce-time-boundaries 'error)
							 (subed-subtitle-spacing 100))
					 (expect (subed-adjust-subtitle-time-stop 1) :to-throw 'error)
					 (expect (subed-subtitle-msecs-stop) :to-equal 2000))))
      (it "increases without overshooting the target time."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:02,000 --> 00:00:03,000\n"
												 "Bar.\n"))
				 (subed-jump-to-subtitle-id 2)
				 (let ((subed-enforce-time-boundaries 'error)
							 (subed-subtitle-spacing 100))
					 (expect (subed-adjust-subtitle-time-start -1) :to-throw 'error)
					 (expect (subed-subtitle-msecs-start) :to-equal 2000))))
      )
    (describe "ignores negative duration if the second argument is truthy"
      (it "when adjusting start time."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"))
				 (expect (subed-adjust-subtitle-time-start 2000 t) :to-be 2000)
				 (expect (subed-subtitle-msecs-start) :to-be 3000)
				 (expect (subed-subtitle-msecs-stop) :to-be 2000)
				 (expect (subed-adjust-subtitle-time-start -500 t) :to-be -500)
				 (expect (subed-subtitle-msecs-start) :to-be 2500)
				 (expect (subed-subtitle-msecs-stop) :to-be 2000)))
      (it "when adjusting stop time."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"))
				 (expect (subed-adjust-subtitle-time-stop -1500 t) :to-be -1500)
				 (expect (subed-subtitle-msecs-stop) :to-be 500)
				 (expect (subed-subtitle-msecs-start) :to-be 1000)
				 (expect (subed-adjust-subtitle-time-stop 200 t) :to-be 200)
				 (expect (subed-subtitle-msecs-stop) :to-be 700)
				 (expect (subed-subtitle-msecs-start) :to-be 1000)))
      )
    (describe "ignores subtitle spacing if the second argument is truthy"
      (it "when adjusting start time."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:02,200 --> 00:00:03,000\n"
												 "Bar.\n"))
				 (subed-jump-to-subtitle-id 2)
				 (expect (subed-adjust-subtitle-time-start -150 nil t) :to-be -150)
				 (expect (subed-subtitle-msecs-start 2) :to-be 2050)
				 (expect (subed-subtitle-msecs-stop 1) :to-be 2000)
				 (expect (subed-adjust-subtitle-time-start -51 nil t) :to-be -51)
				 (expect (subed-subtitle-msecs-start 2) :to-be 1999)
				 (expect (subed-subtitle-msecs-stop 1) :to-be 2000)))
      (it "when adjusting stop time."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:02,200 --> 00:00:03,000\n"
												 "Bar.\n"))
				 (subed-jump-to-subtitle-id 1)
				 (expect (subed-adjust-subtitle-time-stop 150 nil t) :to-be 150)
				 (expect (subed-subtitle-msecs-stop 1) :to-be 2150)
				 (expect (subed-subtitle-msecs-start 2) :to-be 2200)
				 (expect (subed-adjust-subtitle-time-stop 51 nil t) :to-be 51)
				 (expect (subed-subtitle-msecs-stop 1) :to-be 2201)
				 (expect (subed-subtitle-msecs-start 2) :to-be 2200)))
      )
    (describe "ignores negative duration if subed-enforce-time-boundaries is falsy"
      (it "when adjusting start time."
        (with-temp-srt-buffer
				 (setq-local subed-enforce-time-boundaries nil)
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"))
				 (expect (subed-adjust-subtitle-time-start 2000) :to-be 2000)
				 (expect (subed-subtitle-msecs-start) :to-be 3000)
				 (expect (subed-subtitle-msecs-stop) :to-be 2000)
				 (expect (subed-adjust-subtitle-time-start -500) :to-be -500)
				 (expect (subed-subtitle-msecs-start) :to-be 2500)
				 (expect (subed-subtitle-msecs-stop) :to-be 2000)))
      (it "when adjusting stop time."
        (with-temp-srt-buffer
				 (setq-local subed-enforce-time-boundaries nil)
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"))
				 (expect (subed-adjust-subtitle-time-stop -1500) :to-be -1500)
				 (expect (subed-subtitle-msecs-stop) :to-be 500)
				 (expect (subed-subtitle-msecs-start) :to-be 1000)
				 (expect (subed-adjust-subtitle-time-stop 200) :to-be 200)
				 (expect (subed-subtitle-msecs-stop) :to-be 700)
				 (expect (subed-subtitle-msecs-start) :to-be 1000)))
      )
    (describe "ignores subtitle spacing if subed-enforce-time-boundaries is falsy"
      (it "when adjusting start time."
        (with-temp-srt-buffer
				 (setq-local subed-enforce-time-boundaries nil)
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:02,200 --> 00:00:03,000\n"
												 "Bar.\n"))
				 (subed-jump-to-subtitle-id 2)
				 (expect (subed-adjust-subtitle-time-start -150) :to-be -150)
				 (expect (subed-subtitle-msecs-start 2) :to-be 2050)
				 (expect (subed-subtitle-msecs-stop 1) :to-be 2000)
				 (expect (subed-adjust-subtitle-time-start -51) :to-be -51)
				 (expect (subed-subtitle-msecs-start 2) :to-be 1999)
				 (expect (subed-subtitle-msecs-stop 1) :to-be 2000)))
      (it "when adjusting stop time."
        (with-temp-srt-buffer
				 (setq-local subed-enforce-time-boundaries nil)
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:02,200 --> 00:00:03,000\n"
												 "Bar.\n"))
				 (subed-jump-to-subtitle-id 1)
				 (expect (subed-adjust-subtitle-time-stop 150) :to-be 150)
				 (expect (subed-subtitle-msecs-stop 1) :to-be 2150)
				 (expect (subed-subtitle-msecs-start 2) :to-be 2200)
				 (expect (subed-adjust-subtitle-time-stop 51) :to-be 51)
				 (expect (subed-subtitle-msecs-stop 1) :to-be 2201)
				 (expect (subed-subtitle-msecs-start 2) :to-be 2200)))
      )
    (describe "prevents negative time even if subed-enforce-time-boundaries is falsy"
      (it "when adjusting start time."
        (with-temp-srt-buffer
				 (setq-local subed-enforce-time-boundaries nil)
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"))
				 (expect (subed-adjust-subtitle-time-start -1000) :to-be -1000)
				 (expect (subed-subtitle-msecs-start) :to-be 0)
				 (expect (subed-adjust-subtitle-time-start -1) :to-be 0)
				 (expect (subed-subtitle-msecs-start) :to-be 0)))
      (it "when adjusting stop time."
        (with-temp-srt-buffer
				 (setq-local subed-enforce-time-boundaries nil)
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"))
				 (expect (subed-adjust-subtitle-time-stop -2000) :to-be -2000)
				 (expect (subed-subtitle-msecs-stop) :to-be 0)
				 (expect (subed-adjust-subtitle-time-stop -1) :to-be nil)
				 (expect (subed-subtitle-msecs-stop) :to-be 0)))
      )
    (it "does nothing if no timestamp can be found."
      (with-temp-srt-buffer
			 (insert "foo")
			 (goto-char (point-min))
			 (expect (subed-adjust-subtitle-time-start 123) :to-be nil)
			 (expect (buffer-string) :to-equal "foo")
			 (expect (subed-adjust-subtitle-time-start -123) :to-be nil)
			 (expect (buffer-string) :to-equal "foo")))
    )

  (describe "Copy start/stop time from player"
    :var (subed-mpv-playback-position)
    (it "does nothing in an empty buffer."
      (with-temp-srt-buffer
			 (let ((subed-mpv-playback-position 12345))
				 (expect (subed-copy-player-pos-to-start-time) :to-be nil)
				 (expect (subed-copy-player-pos-to-stop-time) :to-be nil)
				 (expect (buffer-string) :to-equal ""))))
    (it "does nothing if player position is unknown."
      (with-temp-srt-buffer
			 (insert (concat "1\n"
											 "00:00:01,000 --> 00:00:02,000\n"
											 "Foo.\n"))
			 (let ((subed-mpv-playback-position nil))
				 (expect (subed-copy-player-pos-to-start-time) :to-be nil)
				 (expect (subed-copy-player-pos-to-stop-time) :to-be nil)
				 (expect (buffer-string) :to-equal (concat "1\n"
																									 "00:00:01,000 --> 00:00:02,000\n"
																									 "Foo.\n")))))
    (it "sets start/stop time when possible."
      (with-temp-srt-buffer
			 (insert (concat "1\n"
											 "00:00:01,000 --> 00:00:02,000\n"
											 "Foo.\n"))
			 (let ((subed-mpv-playback-position (+ 60000 2000 123))
						 (subed-enforce-time-boundaries nil))
				 (expect (subed-copy-player-pos-to-start-time) :to-be subed-mpv-playback-position)
				 (expect (buffer-string) :to-equal (concat "1\n"
																									 "00:01:02,123 --> 00:00:02,000\n"
																									 "Foo.\n")))
			 (let ((subed-mpv-playback-position (+ 60000 5000 456))
						 (subed-enforce-time-boundaries nil))
				 (expect (subed-copy-player-pos-to-stop-time) :to-be subed-mpv-playback-position)
				 (expect (buffer-string) :to-equal (concat "1\n"
																									 "00:01:02,123 --> 00:01:05,456\n"
																									 "Foo.\n")))))
    (it "runs the appropriate hook."
      (with-temp-srt-buffer
			 (insert (concat "1\n"
											 "00:00:01,000 --> 00:00:02,000\n"
											 "Foo.\n"))
			 (let ((foo (setf (symbol-function 'foo) (lambda (msecs) ())))
						 (subed-enforce-time-boundaries nil))
				 (spy-on 'foo)
				 (add-hook 'subed-subtitle-time-adjusted-hook 'foo)
				 (let ((subed-mpv-playback-position (+ 60000 2000 123)))
					 (expect (subed-copy-player-pos-to-start-time) :to-be subed-mpv-playback-position)
					 (expect (buffer-string) :to-equal (concat "1\n"
																										 "00:01:02,123 --> 00:00:02,000\n"
																										 "Foo.\n"))
					 (expect (spy-calls-args-for 'foo 0) :to-equal `(,(+ 60000 2000 123)))
					 (expect (spy-calls-count 'foo) :to-equal 1)))
			 (let ((subed-mpv-playback-position (+ 60000 5000 456)))
				 (expect (subed-copy-player-pos-to-stop-time) :to-be subed-mpv-playback-position)
				 (expect (buffer-string) :to-equal (concat "1\n"
																									 "00:01:02,123 --> 00:01:05,456\n"
																									 "Foo.\n"))
				 (expect (spy-calls-args-for 'foo 0) :to-equal `(,(+ 60000 2000 123)))
				 (expect (spy-calls-count 'foo) :to-equal 2)))
      (remove-hook 'subed-subtitle-time-adjusted-hook 'foo))
    )

  (describe "Jumping"
    (describe "to subtitle text given msecs"
      (it "finds the right subtitle"
        (with-temp-srt-buffer
				 (insert mock-srt-data)
				 (subed-jump-to-subtitle-text-at-msecs 122234)
				 (expect (looking-at "Bar\\.") :to-equal t)))))
  (describe "Moving"
    (it "adjusts start and stop time by the same amount."
      (with-temp-srt-buffer
			 (insert (concat "1\n"
											 "00:00:01,000 --> 00:00:02,000\n"
											 "Foo.\n"))
			 (let ((orig-point (subed-jump-to-subtitle-text 1)))
				 (subed-move-subtitle-forward 100)
				 (expect (subed-subtitle-msecs-start) :to-equal 1100)
				 (expect (subed-subtitle-msecs-stop) :to-equal 2100)
				 (subed-move-subtitle-backward 200)
				 (expect (subed-subtitle-msecs-start) :to-equal 900)
				 (expect (subed-subtitle-msecs-stop) :to-equal 1900)
				 (expect (point) :to-equal orig-point))))
		(describe "when clipping to time boundaries"
			(it "adjusts start and stop time by the same amount when bumping into next subtitle."
				(with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:01,600\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:02,000 --> 00:00:03,000\n"
												 "Bar.\n"))
				 (let ((orig-point (subed-jump-to-subtitle-id 1))
							 (subed-subtitle-spacing 100)
							 (subed-enforce-time-boundaries 'clip))
					 (subed-move-subtitle-forward 1000)
					 (expect (subed-subtitle-msecs-start) :to-equal 1300)
					 (expect (subed-subtitle-msecs-stop) :to-equal 1900)
					 (expect (point) :to-equal orig-point))))
			(it "adjusts start and stop time by the same amount when bumping into previous subtitle."
				(with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:01,600\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:02,000 --> 00:00:03,000\n"
												 "Bar.\n"))
				 (let ((orig-point (subed-jump-to-subtitle-id 2))
							 (subed-subtitle-spacing 100)
							 (subed-enforce-time-boundaries 'clip))
					 (subed-move-subtitle-backward 1000)
					 (expect (subed-subtitle-msecs-start) :to-equal 1700)
					 (expect (subed-subtitle-msecs-stop) :to-equal 2700)
					 (expect (point) :to-equal orig-point)))))
		(describe "when time boundaries are enforced with errors"
			(it "does not adjust anything if subtitle cannot be moved forward at all."
				(with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:02,000 --> 00:00:03,000\n"
												 "Bar.\n"))
				 (let ((orig-point (subed-jump-to-subtitle-id 1))
							 (subed-enforce-time-boundaries 'error))
					 (expect (subed-move-subtitle-forward 1) :to-throw 'error)
					 (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
					 (expect (subed-subtitle-msecs-start 2) :to-equal 2000)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
					 (expect (point) :to-equal orig-point))))
			(it "does not adjust anything if subtitle cannot be moved backward at all."
				(with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:02,000 --> 00:00:03,000\n"
												 "Bar.\n"))
				 (let ((orig-point (subed-jump-to-subtitle-id 2))
							 (subed-enforce-time-boundaries 'error))
					 (expect (subed-move-subtitle-backward 1) :to-throw 'error)
					 (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
					 (expect (subed-subtitle-msecs-start 2) :to-equal 2000)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
					 (expect (point) :to-equal orig-point)))))
    (describe "adjusts subtitles in the active region,"
      (it "excluding the first subtitle."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:03,000 --> 00:00:04,000\n"
												 "Bar.\n\n"
												 "3\n"
												 "00:00:05,000 --> 00:00:06,000\n"
												 "Baz.\n"))
				 (setq mark-active t)
				 (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-text 2))
				 (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-time-start 3))
				 (let ((orig-point (subed-jump-to-subtitle-text 2)))
					 (subed-move-subtitle-forward 100)
					 (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
					 (expect (subed-subtitle-msecs-start 2) :to-equal 3100)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 4100)
					 (expect (subed-subtitle-msecs-start 3) :to-equal 5100)
					 (expect (subed-subtitle-msecs-stop 3) :to-equal 6100)
					 (expect (point) :to-equal orig-point)
					 (subed-move-subtitle-backward 200)
					 (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
					 (expect (subed-subtitle-msecs-start 2) :to-equal 2900)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 3900)
					 (expect (subed-subtitle-msecs-start 3) :to-equal 4900)
					 (expect (subed-subtitle-msecs-stop 3) :to-equal 5900)
					 (expect (point) :to-equal orig-point))))
      (it "excluding the last subtitle."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:03,000 --> 00:00:04,000\n"
												 "Bar.\n\n"
												 "3\n"
												 "00:00:05,000 --> 00:00:06,000\n"
												 "Baz.\n"))
				 (setq mark-active t)
				 (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-text 1))
				 (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-time-stop 2))
				 (let ((orig-point (subed-jump-to-subtitle-time-stop 3)))
					 (subed-move-subtitle-forward 500)
					 (expect (subed-subtitle-msecs-start 1) :to-equal 1500)
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2500)
					 (expect (subed-subtitle-msecs-start 2) :to-equal 3500)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 4500)
					 (expect (subed-subtitle-msecs-start 3) :to-equal 5000)
					 (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
					 (expect (point) :to-equal orig-point)
					 (subed-move-subtitle-backward 300)
					 (expect (subed-subtitle-msecs-start 1) :to-equal 1200)
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2200)
					 (expect (subed-subtitle-msecs-start 2) :to-equal 3200)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 4200)
					 (expect (subed-subtitle-msecs-start 3) :to-equal 5000)
					 (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
					 (expect (point) :to-equal orig-point))))
			(describe "when ignoring time boundaries"
				(it "does not change spacing between subtitles when moving subtitles forward."
					(with-temp-srt-buffer
					 (insert "1\n"
									 "00:00:01,000 --> 00:00:02,000\n"
									 "Foo.\n\n"
									 "2\n"
									 "00:00:10,000 --> 00:00:11,000\n"
									 "Bar.\n\n"
									 "3\n"
									 "00:00:12,000 --> 00:00:13,000\n"
									 "Baz.\n")
					 (setq mark-active t)
					 (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 1))
					 (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 2))
					 (let ((orig-point (subed-jump-to-subtitle-time-start 1))
								 (subed-enforce-time-boundaries nil))
						 (subed-move-subtitle-forward 2000)
						 (expect (subed-subtitle-msecs-start 1) :to-equal 3000)
						 (expect (subed-subtitle-msecs-stop 1) :to-equal 4000)
						 (expect (subed-subtitle-msecs-start 2) :to-equal 12000)
						 (expect (subed-subtitle-msecs-stop 2) :to-equal 13000)
						 (expect (subed-subtitle-msecs-start 3) :to-equal 12000)
						 (expect (subed-subtitle-msecs-stop 3) :to-equal 13000)
						 (expect (point) :to-equal orig-point))))
				(it "does not change spacing between subtitles when moving subtitles backward."
					(with-temp-srt-buffer
					 (insert (concat "1\n"
													 "00:00:01,000 --> 00:00:02,000\n"
													 "Foo.\n\n"
													 "2\n"
													 "00:00:03,000 --> 00:00:04,000\n"
													 "Bar.\n\n"
													 "3\n"
													 "00:00:10,000 --> 00:00:11,000\n"
													 "Baz.\n"))
					 (setq mark-active t)
					 (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 2))
					 (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 3))
					 (let ((orig-point (subed-jump-to-subtitle-time-start 2))
								 (subed-enforce-time-boundaries nil))
						 (subed-move-subtitle-backward 1000)
						 (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
						 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
						 (expect (subed-subtitle-msecs-start 2) :to-equal 2000)
						 (expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
						 (expect (subed-subtitle-msecs-start 3) :to-equal 9000)
						 (expect (subed-subtitle-msecs-stop 3) :to-equal 10000)
						 (expect (point) :to-equal orig-point))))))
		;; What does it mean by not having space left?
    ;; (describe "unless there is no space left"
		;; 	(describe "when moving forward"
		;; 		(it "updates the start time."
		;; 			(with-temp-srt-buffer
		;; 				(insert (concat "1\n"
		;; 												"00:00:01,000 --> 00:00:02,000\n"
		;; 												"Foo.\n\n"
		;; 												"2\n"
		;; 												"00:00:10,000 --> 00:00:11,000\n"
		;; 												"Bar.\n\n"
		;; 												"3\n"
		;; 												"00:00:11,000 --> 00:00:12,000\n"
		;; 												"Baz.\n"))
		;; 				(setq mark-active t)
		;; 				(spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 1))
		;; 				(spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 2))
		;; 				(let ((orig-point (subed-jump-to-subtitle-text 1)))
		;; 					(subed-move-subtitle-forward 1)
		;; 					(expect (subed-subtitle-msecs-start 1) :to-equal 1000)
		;; 					(expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
		;; 					(expect (subed-subtitle-msecs-start 2) :to-equal 10000)
		;; 					(expect (subed-subtitle-msecs-stop 2) :to-equal 11000)
		;; 					(expect (subed-subtitle-msecs-start 3) :to-equal 11000)
		;; 					(expect (subed-subtitle-msecs-stop 3) :to-equal 12000)
		;; 					(expect (point) :to-equal orig-point)))))
    ;;   (it "when moving backward."
    ;;     (with-temp-srt-buffer
		;; 			(insert (concat "1\n"
		;; 											"00:00:01,000 --> 00:00:02,000\n"
		;; 											"Foo.\n\n"
		;; 											"2\n"
		;; 											"00:00:02,000 --> 00:00:03,000\n"
		;; 											"Bar.\n\n"
		;; 											"3\n"
		;; 											"00:00:11,000 --> 00:00:12,000\n"
		;; 											"Baz.\n"))
		;; 			(setq mark-active t)
		;; 			(spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 2))
		;; 			(spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 3))
		;; 			(let ((orig-point (subed-jump-to-subtitle-id 3)))
		;; 				(subed-move-subtitle-backward 1)
		;; 				(expect (subed-subtitle-msecs-start 1) :to-equal 1000)
		;; 				(expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
		;; 				(expect (subed-subtitle-msecs-start 2) :to-equal 2000)
		;; 				(expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
		;; 				(expect (subed-subtitle-msecs-start 3) :to-equal 11000)
		;; 				(expect (subed-subtitle-msecs-stop 3) :to-equal 12000)
		;; 				(expect (point) :to-equal orig-point))))
    ;;   )
    (describe "ignoring spacing for non-leading subtitles"
      (it "when moving forward."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:00,000 --> 00:00:01,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:01,050 --> 00:00:02,000\n"
												 "Bar.\n\n"
												 "3\n"
												 "00:00:05,000 --> 00:00:6,000\n"
												 "Baz.\n"))
				 (setq mark-active t)
				 (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 1))
				 (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 2))
				 (let ((orig-point (subed-jump-to-subtitle-time-start 3)))
					 (subed-move-subtitle-forward 1000)
					 (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
					 (expect (subed-subtitle-msecs-start 2) :to-equal 2050)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
					 (expect (subed-subtitle-msecs-start 3) :to-equal 5000)
					 (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
					 (expect (point) :to-equal orig-point))))
      (it "when moving backward."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:04,000 --> 00:00:05,000\n"
												 "Bar.\n\n"
												 "3\n"
												 "00:00:05,000 --> 00:00:05,000\n"
												 "Baz.\n"))
				 (setq mark-active t)
				 (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 2))
				 (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 3))
				 (let ((orig-point (subed-jump-to-subtitle-time-stop 1)))
					 (subed-move-subtitle-backward 1000)
					 (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
					 (expect (subed-subtitle-msecs-start 2) :to-equal 3000)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 4000)
					 (expect (subed-subtitle-msecs-start 3) :to-equal 4000)
					 (expect (subed-subtitle-msecs-stop 3) :to-equal 4000)
					 (expect (point) :to-equal orig-point))))
      )
    (describe "ignoring overlapping subtitles"
      (it "when moving forward."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:01,500\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:01,300 --> 00:00:02,000\n"
												 "Bar.\n\n"
												 "3\n"
												 "00:00:05,000 --> 00:00:6,000\n"
												 "Baz.\n"))
				 (setq mark-active t)
				 (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 1))
				 (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 2))
				 (let ((orig-point (subed-jump-to-subtitle-text 2)))
					 (subed-move-subtitle-forward 1000)
					 (expect (subed-subtitle-msecs-start 1) :to-equal 2000)
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2500)
					 (expect (subed-subtitle-msecs-start 2) :to-equal 2300)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
					 (expect (subed-subtitle-msecs-start 3) :to-equal 5000)
					 (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
					 (expect (point) :to-equal orig-point))))
      (it "when moving backward."
        (with-temp-srt-buffer
				 (insert (concat "1\n"
												 "00:00:01,000 --> 00:00:02,000\n"
												 "Foo.\n\n"
												 "2\n"
												 "00:00:04,500 --> 00:00:04,000\n"
												 "Bar.\n\n"
												 "3\n"
												 "00:00:04,500 --> 00:00:04,490\n"
												 "Baz.\n"))
				 (setq mark-active t)
				 (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 2))
				 (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 3))
				 (let ((orig-point (subed-jump-to-subtitle-text 1)))
					 (subed-move-subtitle-backward 1000)
					 (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
					 (expect (subed-subtitle-msecs-start 2) :to-equal 3500)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
					 (expect (subed-subtitle-msecs-start 3) :to-equal 3500)
					 (expect (subed-subtitle-msecs-stop 3) :to-equal 3490)
					 (expect (point) :to-equal orig-point))))
      )
    (it "ignoring start time being larger than stop time."
      (with-temp-srt-buffer
			 (insert (concat "1\n"
											 "00:00:01,500 --> 00:00:01,400\n"
											 "Foo.\n\n"
											 "2\n"
											 "00:00:02,500 --> 00:00:02,499\n"
											 "Bar.\n\n"
											 "3\n"
											 "00:00:05,000 --> 00:00:06,000\n"
											 "Bar.\n"))
			 (setq mark-active t)
			 (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-text 1))
			 (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-time-start 2))
			 (let ((orig-point (subed-jump-to-subtitle-time-stop 1)))
				 (subed-move-subtitle-forward 1000)
				 (expect (subed-subtitle-msecs-start 1) :to-equal 2500)
				 (expect (subed-subtitle-msecs-stop 1) :to-equal 2400)
				 (expect (subed-subtitle-msecs-start 2) :to-equal 3500)
				 (expect (subed-subtitle-msecs-stop 2) :to-equal 3499)
				 (expect (subed-subtitle-msecs-start 3) :to-equal 5000)
				 (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
				 (expect (point) :to-equal orig-point)
				 (subed-move-subtitle-backward 500)
				 (expect (subed-subtitle-msecs-start 1) :to-equal 2000)
				 (expect (subed-subtitle-msecs-stop 1) :to-equal 1900)
				 (expect (subed-subtitle-msecs-start 2) :to-equal 3000)
				 (expect (subed-subtitle-msecs-stop 2) :to-equal 2999)
				 (expect (subed-subtitle-msecs-start 3) :to-equal 5000)
				 (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
				 (expect (point) :to-equal orig-point))))
    (it "ignoring stop time being smaller than start time."
      (with-temp-srt-buffer
			 (insert (concat "1\n"
											 "00:00:01,000 --> 00:00:02,000\n"
											 "Foo.\n\n"
											 "2\n"
											 "00:00:04,100 --> 00:00:04,099\n"
											 "Bar.\n\n"
											 "3\n"
											 "00:00:05,500 --> 00:00:05,000\n"
											 "Bar.\n"))
			 (setq mark-active t)
			 (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-text 2))
			 (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-time-start 3))
			 (let ((orig-point (subed-jump-to-subtitle-text 1)))
				 (subed-move-subtitle-forward 1000)
				 (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
				 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
				 (expect (subed-subtitle-msecs-start 2) :to-equal 5100)
				 (expect (subed-subtitle-msecs-stop 2) :to-equal 5099)
				 (expect (subed-subtitle-msecs-start 3) :to-equal 6500)
				 (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
				 (expect (point) :to-equal orig-point)
				 (subed-move-subtitle-backward 500)
				 (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
				 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
				 (expect (subed-subtitle-msecs-start 2) :to-equal 4600)
				 (expect (subed-subtitle-msecs-stop 2) :to-equal 4599)
				 (expect (subed-subtitle-msecs-start 3) :to-equal 6000)
				 (expect (subed-subtitle-msecs-stop 3) :to-equal 5500)
				 (expect (point) :to-equal orig-point))))
    (it "disables subtitle replay while moving subtitles."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-enable-replay-adjusted-subtitle :quiet)
			 (spy-on 'subed-enable-replay-adjusted-subtitle :and-call-through)
			 (spy-on 'subed-disable-replay-adjusted-subtitle :and-call-through)
			 (spy-on 'subed-adjust-subtitle-time-start :and-call-fake
							 (lambda (msecs &optional a b) (expect (subed-replay-adjusted-subtitle-p) :to-be nil)))
			 (spy-on 'subed-adjust-subtitle-stop :and-call-fake
							 (lambda (msecs &optional a b) (expect (subed-replay-adjusted-subtitle-p) :to-be nil)))
			 (subed-move-subtitle-forward 100)
			 (expect 'subed-disable-replay-adjusted-subtitle :to-have-been-called-times 1)
			 (expect 'subed-enable-replay-adjusted-subtitle :to-have-been-called-times 1)
			 (subed-move-subtitle-backward 100)
			 (expect 'subed-disable-replay-adjusted-subtitle :to-have-been-called-times 2)
			 (expect 'subed-enable-replay-adjusted-subtitle :to-have-been-called-times 2)))
    (it "does not enable subtitle replay afterwards if it is disabled."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-disable-replay-adjusted-subtitle :quiet)
			 (spy-on 'subed-enable-replay-adjusted-subtitle :and-call-through)
			 (spy-on 'subed-disable-replay-adjusted-subtitle :and-call-through)
			 (spy-on 'subed-adjust-subtitle-time-start :and-call-fake
							 (lambda (msecs &optional a b) (expect (subed-replay-adjusted-subtitle-p) :to-be nil)))
			 (spy-on 'subed-adjust-subtitle-stop :and-call-fake
							 (lambda (msecs &optional a b) (expect (subed-replay-adjusted-subtitle-p) :to-be nil)))
			 (subed-move-subtitle-forward 100)
			 (expect 'subed-disable-replay-adjusted-subtitle :to-have-been-called-times 1)
			 (expect 'subed-enable-replay-adjusted-subtitle :to-have-been-called-times 0)
			 (subed-move-subtitle-backward 100)
			 (expect 'subed-disable-replay-adjusted-subtitle :to-have-been-called-times 2)
			 (expect 'subed-enable-replay-adjusted-subtitle :to-have-been-called-times 0)))
    (it "seeks player to current subtitle if region is not active."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (spy-on 'subed-replay-adjusted-subtitle-p :and-return-value t)
			 (spy-on 'subed-mpv-jump)
			 (subed-move-subtitle-forward 100)
			 (expect 'subed-mpv-jump :to-have-been-called-times 1)
			 (expect 'subed-mpv-jump :to-have-been-called-with 183550)
			 (subed-move-subtitle-backward 200)
			 (expect 'subed-mpv-jump :to-have-been-called-times 2)
			 (expect 'subed-mpv-jump :to-have-been-called-with 183350)))
    (it "seeks player to first subtitle in active region."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (let ((beg 15)
						 (end (point-max)))
				 (setq mark-active t)
				 (spy-on 'region-beginning :and-return-value beg)
				 (spy-on 'region-end :and-return-value end)
				 (spy-on 'subed-replay-adjusted-subtitle-p :and-return-value t)
				 (spy-on 'subed-mpv-jump)
				 (subed-move-subtitle-forward 100)
				 (expect 'subed-mpv-jump :to-have-been-called-times 1)
				 (expect 'subed-mpv-jump :to-have-been-called-with '61100)
				 (subed-move-subtitle-backward 300)
				 (expect 'subed-mpv-jump :to-have-been-called-times 2)
				 (expect 'subed-mpv-jump :to-have-been-called-with '60800))))
    (describe "to a specified start timestamp"
      (describe "when focusing on the current subtitle"
        (it "adjusts start and stop time by the same amount."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-text 2)
           (subed-move-subtitles-to-start-at-timestamp "00:02:02,334" nil nil)
           (expect (subed-subtitle-msecs-start 1) :to-equal (subed-timestamp-to-msecs "00:01:01,000"))
           (expect (subed-subtitle-msecs-stop 1) :to-equal (subed-timestamp-to-msecs "00:01:05,123"))
           (expect (subed-subtitle-msecs-start 2) :to-equal (subed-timestamp-to-msecs "00:02:02,334"))
           (expect (subed-subtitle-msecs-stop 2) :to-equal (subed-timestamp-to-msecs "00:02:10,445"))
           (expect (subed-subtitle-msecs-start 3) :to-equal (subed-timestamp-to-msecs "00:03:03,450"))
           (expect (subed-subtitle-msecs-stop 3) :to-equal (subed-timestamp-to-msecs "00:03:15,500")))))
      (describe "when moving current and following subtitles"
        (it "adjusts start and stop time by the same amount."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-text 2)
           (subed-move-subtitles-to-start-at-timestamp "00:02:02,334" (point) (point-max))
           (expect (subed-subtitle-msecs-start 1) :to-equal (subed-timestamp-to-msecs "00:01:01,000"))
           (expect (subed-subtitle-msecs-stop 1) :to-equal (subed-timestamp-to-msecs "00:01:05,123"))
           (expect (subed-subtitle-msecs-start 2) :to-equal (subed-timestamp-to-msecs "00:02:02,334"))
           (expect (subed-subtitle-msecs-stop 2) :to-equal (subed-timestamp-to-msecs "00:02:10,445"))
           (expect (subed-subtitle-msecs-start 3) :to-equal (subed-timestamp-to-msecs "00:03:03,550"))
           (expect (subed-subtitle-msecs-stop 3) :to-equal (subed-timestamp-to-msecs "00:03:15,600")))))))
  (describe "Shifting"
    (describe "by an msec offset"
      (it "adjusts start and stop time by the same amount."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 2)
         (subed-shift-subtitles 100)
         (expect (subed-subtitle-msecs-start 1) :to-equal (subed-timestamp-to-msecs "00:01:01,000"))
         (expect (subed-subtitle-msecs-stop 1) :to-equal (subed-timestamp-to-msecs "00:01:05,123"))
         (expect (subed-subtitle-msecs-start 2) :to-equal (subed-timestamp-to-msecs "00:02:02,334"))
         (expect (subed-subtitle-msecs-stop 2) :to-equal (subed-timestamp-to-msecs "00:02:10,445"))
         (expect (subed-subtitle-msecs-start 3) :to-equal (subed-timestamp-to-msecs "00:03:03,550"))
         (expect (subed-subtitle-msecs-stop 3) :to-equal (subed-timestamp-to-msecs "00:03:15,600")))))
    (describe "to a specified start timestamp"
      (it "adjusts start and stop time by the same amount."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-text 2)
         (subed-shift-subtitles-to-start-at-timestamp "00:02:02,334")
         (expect (subed-subtitle-msecs-start 1) :to-equal (subed-timestamp-to-msecs "00:01:01,000"))
         (expect (subed-subtitle-msecs-stop 1) :to-equal (subed-timestamp-to-msecs "00:01:05,123"))
         (expect (subed-subtitle-msecs-start 2) :to-equal (subed-timestamp-to-msecs "00:02:02,334"))
         (expect (subed-subtitle-msecs-stop 2) :to-equal (subed-timestamp-to-msecs "00:02:10,445"))
         (expect (subed-subtitle-msecs-start 3) :to-equal (subed-timestamp-to-msecs "00:03:03,550"))
         (expect (subed-subtitle-msecs-stop 3) :to-equal (subed-timestamp-to-msecs "00:03:15,600"))))))

  (describe "Inserting evenly spaced"
    (describe "in an empty buffer,"
      (describe "appending"
        (it "a single subtile."
          (cl-loop for arg in (list nil 1) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (expect (subed-insert-subtitle arg) :to-equal 33)
                    (expect (buffer-string) :to-equal (concat "0\n"
                                                              "00:00:00,000 --> 00:00:01,000\n"
                                                              "\n"))
                    (expect (point) :to-equal 33)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtiles."
          (cl-loop for arg in (list 2) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (expect (subed-insert-subtitle arg) :to-equal 33)
                    (expect (buffer-string) :to-equal (concat "0\n"
                                                              "00:00:00,000 --> 00:00:01,000\n"
                                                              "\n\n"
                                                              "0\n"
                                                              "00:00:01,100 --> 00:00:02,100\n"
                                                              "\n"))
                    (expect (point) :to-equal 33)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      (describe "prepending"
        (it "a single subtile."
          (cl-loop for arg in (list '- -1 (list 4)) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (expect (subed-insert-subtitle arg) :to-equal 33)
                    (expect (buffer-string) :to-equal (concat "0\n"
                                                              "00:00:00,000 --> 00:00:01,000\n"
                                                              "\n"))
                    (expect (point) :to-equal 33)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtiles."
          (cl-loop for arg in (list -2 (list -16)) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (expect (subed-insert-subtitle arg) :to-equal 33)
                    (expect (buffer-string) :to-equal (concat "0\n"
                                                              "00:00:00,000 --> 00:00:01,000\n"
                                                              "\n\n"
                                                              "0\n"
                                                              "00:00:01,100 --> 00:00:02,100\n"
                                                              "\n"))
                    (expect (point) :to-equal 33)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      )
    (describe "in a non-empty buffer"
      (describe "prepending between subtitles"
        (it "a single subtitle."
          (cl-loop for arg in (list '- -1 (list 4)) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:00:59,000 --> 00:01:00,000\n"
                                    "Foo.\n\n"
                                    "2\n"
                                    "00:02:00,000 --> 00:02:01,000\n"
                                    "Bar.\n"))
                    (subed-jump-to-subtitle-text 2)
                    (expect (subed-insert-subtitle arg) :to-equal 71)
                    (expect (buffer-string) :to-equal (concat "1\n"
                                                              "00:00:59,000 --> 00:01:00,000\n"
                                                              "Foo.\n\n"
                                                              "0\n"
                                                              "00:01:30,000 --> 00:01:31,000\n"
                                                              "\n\n"
                                                              "2\n"
                                                              "00:02:00,000 --> 00:02:01,000\n"
                                                              "Bar.\n"))
                    (expect (point) :to-equal 71)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtitles."
          (cl-loop for arg in (list -2 (list 16)) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:00:59,000 --> 00:01:00,000\n"
                                    "Foo.\n\n"
                                    "2\n"
                                    "00:02:00,000 --> 00:02:01,000\n"
                                    "Bar.\n"))
                    (subed-jump-to-subtitle-text 2)
                    (expect (subed-insert-subtitle arg) :to-equal 71)
                    (expect (buffer-string) :to-equal (concat "1\n"
                                                              "00:00:59,000 --> 00:01:00,000\n"
                                                              "Foo.\n\n"
                                                              "0\n"
                                                              "00:01:20,000 --> 00:01:21,000\n"
                                                              "\n\n"
                                                              "0\n"
                                                              "00:01:40,000 --> 00:01:41,000\n"
                                                              "\n\n"
                                                              "2\n"
                                                              "00:02:00,000 --> 00:02:01,000\n"
                                                              "Bar.\n"))
                    (expect (point) :to-equal 71)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      (describe "appending between subtitles"
        (it "a single subtitle."
          (cl-loop for arg in (list nil 1) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:00:59,000 --> 00:01:00,000\n"
                                    "Foo.\n\n"
                                    "2\n"
                                    "00:02:00,000 --> 00:02:01,000\n"
                                    "Bar.\n"))
                    (subed-jump-to-subtitle-text 1)
                    (expect (subed-insert-subtitle arg) :to-equal 71)
                    (expect (buffer-string) :to-equal (concat "1\n"
                                                              "00:00:59,000 --> 00:01:00,000\n"
                                                              "Foo.\n\n"
                                                              "0\n"
                                                              "00:01:30,000 --> 00:01:31,000\n"
                                                              "\n\n"
                                                              "2\n"
                                                              "00:02:00,000 --> 00:02:01,000\n"
                                                              "Bar.\n"))
                    (expect (point) :to-equal 71)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtitles."
          (cl-loop for arg in (list 2) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:00:59,000 --> 00:01:00,000\n"
                                    "Foo.\n\n"
                                    "2\n"
                                    "00:02:00,000 --> 00:02:01,000\n"
                                    "Bar.\n"))
                    (subed-jump-to-subtitle-text 1)
                    (expect (subed-insert-subtitle arg) :to-equal 71)
                    (expect (buffer-string) :to-equal (concat "1\n"
                                                              "00:00:59,000 --> 00:01:00,000\n"
                                                              "Foo.\n\n"
                                                              "0\n"
                                                              "00:01:20,000 --> 00:01:21,000\n"
                                                              "\n\n"
                                                              "0\n"
                                                              "00:01:40,000 --> 00:01:41,000\n"
                                                              "\n\n"
                                                              "2\n"
                                                              "00:02:00,000 --> 00:02:01,000\n"
                                                              "Bar.\n"))
                    (expect (point) :to-equal 71)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      (describe "prepending to the first subtitle"
        (it "a single subtitle."
          (cl-loop for arg in (list '- -1 (list 4)) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:01:00,000 --> 00:01:01,000\n"
                                    "Foo.\n"))
                    (subed-jump-to-subtitle-text 1)
                    (expect (subed-insert-subtitle arg) :to-equal 33)
                    (expect (buffer-string) :to-equal (concat "0\n"
                                                              "00:00:30,000 --> 00:00:31,000\n"
                                                              "\n\n"
                                                              "1\n"
                                                              "00:01:00,000 --> 00:01:01,000\n"
                                                              "Foo.\n"))
                    (expect (point) :to-equal 33)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtitles."
          (cl-loop for arg in (list -2 (list 16)) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:01:00,000 --> 00:01:01,000\n"
                                    "Foo.\n"))
                    (subed-jump-to-subtitle-text 1)
                    (expect (subed-insert-subtitle arg) :to-equal 33)
                    (expect (buffer-string) :to-equal (concat "0\n"
                                                              "00:00:20,000 --> 00:00:21,000\n"
                                                              "\n\n"
                                                              "0\n"
                                                              "00:00:40,000 --> 00:00:41,000\n"
                                                              "\n\n"
                                                              "1\n"
                                                              "00:01:00,000 --> 00:01:01,000\n"
                                                              "Foo.\n"))
                    (expect (point) :to-equal 33)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      (describe "appending to the last subtitle"
        (it "a single subtitle."
          (cl-loop for arg in (list nil 1) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:00:59,000 --> 00:01:00,000\n"
                                    "Foo.\n"))
                    (subed-jump-to-subtitle-text 1)
                    (expect (subed-insert-subtitle arg) :to-equal 71)
                    (expect (buffer-string) :to-equal (concat "1\n"
                                                              "00:00:59,000 --> 00:01:00,000\n"
                                                              "Foo.\n\n"
                                                              "0\n"
                                                              "00:01:00,100 --> 00:01:01,100\n"
                                                              "\n"))
                    (expect (point) :to-equal 71)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtitles."
          (cl-loop for arg in (list 2) do
                   (with-temp-srt-buffer
                    (insert (concat "1\n"
                                    "00:00:59,000 --> 00:01:00,000\n"
                                    "Foo.\n"))
                    (subed-jump-to-subtitle-text 3)
                    (expect (subed-insert-subtitle arg) :to-equal 71)
                    (expect (buffer-string) :to-equal (concat "1\n"
                                                              "00:00:59,000 --> 00:01:00,000\n"
                                                              "Foo.\n\n"
                                                              "0\n"
                                                              "00:01:00,100 --> 00:01:01,100\n"
                                                              "\n\n"
                                                              "0\n"
                                                              "00:01:01,200 --> 00:01:02,200\n"
                                                              "\n"))
                    (expect (point) :to-equal 71))))
        )
      (describe "when there is not enough time for the subtitles"
        (describe "to append"
          (it "a single subtitle."
            (cl-loop for arg in (list nil 1) do
                     (with-temp-srt-buffer
                      (spy-on 'subed-regenerate-ids-soon)
                      (insert (concat "1\n"
                                      "00:00:59,000 --> 00:01:00,000\n"
                                      "Foo.\n\n"
                                      "2\n"
                                      "00:01:00,600 --> 00:01:02,000\n"
                                      "Foo.\n"))
                      (subed-jump-to-subtitle-text 1)
                      (expect (subed-insert-subtitle arg) :to-equal 71)
                      (expect (buffer-string) :to-equal (concat "1\n"
                                                                "00:00:59,000 --> 00:01:00,000\n"
                                                                "Foo.\n\n"
                                                                "0\n"
                                                                "00:01:00,100 --> 00:01:00,500\n"
                                                                "\n\n"
                                                                "2\n"
                                                                "00:01:00,600 --> 00:01:02,000\n"
                                                                "Foo.\n"))
                      (expect (point) :to-equal 71)
                      (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                      (spy-calls-reset 'subed-regenerate-ids-soon))))
          (it "multiple subtitles."
            (cl-loop for arg in (list 2) do
                     (with-temp-srt-buffer
                      (spy-on 'subed-regenerate-ids-soon)
                      (insert (concat "1\n"
                                      "00:00:59,000 --> 00:01:00,000\n"
                                      "Foo.\n\n"
                                      "2\n"
                                      "00:01:00,600 --> 00:01:02,000\n"
                                      "Foo.\n"))
                      (subed-jump-to-subtitle-text 1)
                      (expect (subed-insert-subtitle arg) :to-equal 71)
                      (expect (buffer-string) :to-equal (concat "1\n"
                                                                "00:00:59,000 --> 00:01:00,000\n"
                                                                "Foo.\n\n"
                                                                "0\n"
                                                                "00:01:00,100 --> 00:01:00,250\n"
                                                                "\n\n"
                                                                "0\n"
                                                                "00:01:00,350 --> 00:01:00,500\n"
                                                                "\n\n"
                                                                "2\n"
                                                                "00:01:00,600 --> 00:01:02,000\n"
                                                                "Foo.\n"))
                      (expect (point) :to-equal 71)
                      (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                      (spy-calls-reset 'subed-regenerate-ids-soon))))
          )
        (describe "to prepend"
          (describe "between subtitles"
            (it "a single subtitle."
              (cl-loop for arg in (list '- -1 (list 4)) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:57,000 --> 00:00:59,100\n"
                                        "Foo.\n\n"
                                        "2\n"
                                        "00:01:00,000 --> 00:01:02,000\n"
                                        "Foo.\n"))
                        (subed-jump-to-subtitle-text 2)
                        (expect (subed-insert-subtitle arg) :to-equal 71)
                        (expect (buffer-string) :to-equal (concat "1\n"
                                                                  "00:00:57,000 --> 00:00:59,100\n"
                                                                  "Foo.\n\n"
                                                                  "0\n"
                                                                  "00:00:59,200 --> 00:00:59,900\n"
                                                                  "\n\n"
                                                                  "2\n"
                                                                  "00:01:00,000 --> 00:01:02,000\n"
                                                                  "Foo.\n"))
                        (expect (point) :to-equal 71)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            (it "multiple subtitles."
              (cl-loop for arg in (list -2 (list 16)) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:57,000 --> 00:00:59,100\n"
                                        "Foo.\n\n"
                                        "2\n"
                                        "00:01:00,000 --> 00:01:02,000\n"
                                        "Foo.\n"))
                        (subed-jump-to-subtitle-text 2)
                        (expect (subed-insert-subtitle arg) :to-equal 71)
                        (expect (buffer-string) :to-equal (concat "1\n"
                                                                  "00:00:57,000 --> 00:00:59,100\n"
                                                                  "Foo.\n\n"
                                                                  "0\n"
                                                                  "00:00:59,200 --> 00:00:59,500\n"
                                                                  "\n\n"
                                                                  "0\n"
                                                                  "00:00:59,600 --> 00:00:59,900\n"
                                                                  "\n\n"
                                                                  "2\n"
                                                                  "00:01:00,000 --> 00:01:02,000\n"
                                                                  "Foo.\n"))
                        (expect (point) :to-equal 71)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            )
          (describe "before the first subtitle"
            (it "a single subtitle."
              (cl-loop for arg in (list '- -1 (list 4)) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:00,500 --> 00:00:02,000\n"
                                        "Foo.\n"))
                        (subed-jump-to-subtitle-text 1)
                        (expect (subed-insert-subtitle arg) :to-equal 33)
                        (expect (buffer-string) :to-equal (concat "0\n"
                                                                  "00:00:00,100 --> 00:00:00,400\n"
                                                                  "\n\n"
                                                                  "1\n"
                                                                  "00:00:00,500 --> 00:00:02,000\n"
                                                                  "Foo.\n"))
                        (expect (point) :to-equal 33)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            (it "multiple subtitles."
              (cl-loop for arg in (list -2 (list 16)) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:00,600 --> 00:00:01,500\n"
                                        "Foo.\n"))
                        (subed-jump-to-subtitle-text 1)
                        (expect (subed-insert-subtitle arg) :to-equal 33)
                        (expect (buffer-string) :to-equal (concat "0\n"
                                                                  "00:00:00,100 --> 00:00:00,250\n"
                                                                  "\n\n"
                                                                  "0\n"
                                                                  "00:00:00,350 --> 00:00:00,500\n"
                                                                  "\n\n"
                                                                  "1\n"
                                                                  "00:00:00,600 --> 00:00:01,500\n"
                                                                  "Foo.\n"))
                        (expect (point) :to-equal 33)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            )
          )
        )
      (describe "when there is not enough time for spacing"
        (describe "between subtitles"
          (describe "when prepending"
            (it "a single subtitle."
              (cl-loop for arg in (list '- -1 (list 4)) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:55,000 --> 00:00:59,950\n"
                                        "Foo.\n\n"
                                        "2\n"
                                        "00:01:00,000 --> 00:01:02,000\n"
                                        "Bar.\n"))
                        (subed-jump-to-subtitle-text 2)
                        (expect (subed-insert-subtitle arg) :to-equal 71)
                        (expect (buffer-string) :to-equal (concat "1\n"
                                                                  "00:00:55,000 --> 00:00:59,950\n"
                                                                  "Foo.\n\n"
                                                                  "0\n"
                                                                  "00:00:59,950 --> 00:00:59,950\n"
                                                                  "\n\n"
                                                                  "2\n"
                                                                  "00:01:00,000 --> 00:01:02,000\n"
                                                                  "Bar.\n"))
                        (expect (point) :to-equal 71)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            (it "multiple subtitles."
              (cl-loop for arg in (list -2 (list 16)) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:57,000 --> 00:00:59,999\n"
                                        "Foo.\n\n"
                                        "2\n"
                                        "00:01:00,000 --> 00:01:02,000\n"
                                        "Bar.\n"))
                        (subed-jump-to-subtitle-text 2)
                        (expect (subed-insert-subtitle arg) :to-equal 71)
                        (expect (buffer-string) :to-equal (concat "1\n"
                                                                  "00:00:57,000 --> 00:00:59,999\n"
                                                                  "Foo.\n\n"
                                                                  "0\n"
                                                                  "00:00:59,999 --> 00:00:59,999\n"
                                                                  "\n\n"
                                                                  "0\n"
                                                                  "00:00:59,999 --> 00:00:59,999\n"
                                                                  "\n\n"
                                                                  "2\n"
                                                                  "00:01:00,000 --> 00:01:02,000\n"
                                                                  "Bar.\n"))
                        (expect (point) :to-equal 71)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            )
          (describe "when appending"
            (it "a single subtitle."
              (cl-loop for arg in (list nil 1) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:59,000 --> 00:01:00,000\n"
                                        "Foo.\n\n"
                                        "2\n"
                                        "00:01:00,010 --> 00:01:02,000\n"
                                        "Bar.\n"))
                        (subed-jump-to-subtitle-text 1)
                        (expect (subed-insert-subtitle arg) :to-equal 71)
                        (expect (buffer-string) :to-equal (concat "1\n"
                                                                  "00:00:59,000 --> 00:01:00,000\n"
                                                                  "Foo.\n\n"
                                                                  "0\n"
                                                                  "00:01:00,000 --> 00:01:00,000\n"
                                                                  "\n\n"
                                                                  "2\n"
                                                                  "00:01:00,010 --> 00:01:02,000\n"
                                                                  "Bar.\n"))
                        (expect (point) :to-equal 71)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            (it "multiple subtitles."
              (cl-loop for arg in (list 2) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:59,000 --> 00:01:00,000\n"
                                        "Foo.\n\n"
                                        "2\n"
                                        "00:01:00,100 --> 00:01:02,000\n"
                                        "Bar.\n"))
                        (subed-jump-to-subtitle-text 1)
                        (expect (subed-insert-subtitle arg) :to-equal 71)
                        (expect (buffer-string) :to-equal (concat "1\n"
                                                                  "00:00:59,000 --> 00:01:00,000\n"
                                                                  "Foo.\n\n"
                                                                  "0\n"
                                                                  "00:01:00,000 --> 00:01:00,000\n"
                                                                  "\n\n"
                                                                  "0\n"
                                                                  "00:01:00,000 --> 00:01:00,000\n"
                                                                  "\n\n"
                                                                  "2\n"
                                                                  "00:01:00,100 --> 00:01:02,000\n"
                                                                  "Bar.\n"))
                        (expect (point) :to-equal 71)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            )
          )
        (describe "before the first subtitle"
          (it "a single subtitle."
            (cl-loop for arg in (list '- -1 (list 4)) do
                     (with-temp-srt-buffer
                      (spy-on 'subed-regenerate-ids-soon)
                      (insert (concat "1\n"
                                      "00:00:00,050 --> 00:00:02,000\n"
                                      "Foo.\n"))
                      (subed-jump-to-subtitle-text 1)
                      (expect (subed-insert-subtitle arg) :to-equal 33)
                      (expect (buffer-string) :to-equal (concat "0\n"
                                                                "00:00:00,000 --> 00:00:00,000\n"
                                                                "\n\n"
                                                                "1\n"
                                                                "00:00:00,050 --> 00:00:02,000\n"
                                                                "Foo.\n"))
                      (expect (point) :to-equal 33)
                      (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                      (spy-calls-reset 'subed-regenerate-ids-soon))))
          (it "multiple subtitles."
            (cl-loop for arg in (list -2 (list 16)) do
                     (with-temp-srt-buffer
                      (spy-on 'subed-regenerate-ids-soon)
                      (insert (concat "1\n"
                                      "00:00:00,100 --> 00:00:01,500\n"
                                      "Foo.\n"))
                      (subed-jump-to-subtitle-text 1)
                      (expect (subed-insert-subtitle arg) :to-equal 33)
                      (expect (buffer-string) :to-equal (concat "0\n"
                                                                "00:00:00,000 --> 00:00:00,000\n"
                                                                "\n\n"
                                                                "0\n"
                                                                "00:00:00,000 --> 00:00:00,000\n"
                                                                "\n\n"
                                                                "1\n"
                                                                "00:00:00,100 --> 00:00:01,500\n"
                                                                "Foo.\n"))
                      (expect (point) :to-equal 33)
                      (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                      (spy-calls-reset 'subed-regenerate-ids-soon))))
          )
        )
      )
    )

  (describe "Inserting adjacent"
    (describe "in an empty buffer,"
      (describe "appending"
        (it "a single subtile."
          (cl-loop for arg in (list nil 1) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                    (expect (buffer-string) :to-equal (concat "0\n"
                                                              "00:00:00,000 --> 00:00:01,000\n"
                                                              "\n"))
                    (expect (point) :to-equal 33)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtiles."
          (cl-loop for arg in (list 2) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                    (expect (buffer-string) :to-equal (concat "0\n"
                                                              "00:00:00,000 --> 00:00:01,000\n"
                                                              "\n\n"
                                                              "0\n"
                                                              "00:00:01,100 --> 00:00:02,100\n"
                                                              "\n"))
                    (expect (point) :to-equal 33)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      (describe "prepending"
        (it "a single subtile."
          (cl-loop for arg in (list '- -1 (list 4)) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                    (expect (buffer-string) :to-equal (concat "0\n"
                                                              "00:00:00,000 --> 00:00:01,000\n"
                                                              "\n"))
                    (expect (point) :to-equal 33)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtiles."
          (cl-loop for arg in (list -2 (list -16)) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                    (expect (buffer-string) :to-equal (concat "0\n"
                                                              "00:00:00,000 --> 00:00:01,000\n"
                                                              "\n\n"
                                                              "0\n"
                                                              "00:00:01,100 --> 00:00:02,100\n"
                                                              "\n"))
                    (expect (point) :to-equal 33)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      )
    (describe "in a non-empty buffer"
      (describe "prepending between subtitles"
        (it "a single subtitle."
          (cl-loop for arg in (list '- -1 (list 4)) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:00:59,000 --> 00:01:00,000\n"
                                    "Foo.\n\n"
                                    "2\n"
                                    "00:02:00,000 --> 00:02:01,000\n"
                                    "Bar.\n"))
                    (subed-jump-to-subtitle-text 2)
                    (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                    (expect (buffer-string) :to-equal (concat "1\n"
                                                              "00:00:59,000 --> 00:01:00,000\n"
                                                              "Foo.\n\n"
                                                              "0\n"
                                                              "00:01:58,900 --> 00:01:59,900\n"
                                                              "\n\n"
                                                              "2\n"
                                                              "00:02:00,000 --> 00:02:01,000\n"
                                                              "Bar.\n"))
                    (expect (point) :to-equal 71)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtitles."
          (cl-loop for arg in (list -2 (list 16)) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:00:59,000 --> 00:01:00,000\n"
                                    "Foo.\n\n"
                                    "2\n"
                                    "00:02:00,000 --> 00:02:01,000\n"
                                    "Bar.\n"))
                    (subed-jump-to-subtitle-text 2)
                    (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                    (expect (buffer-string) :to-equal (concat "1\n"
                                                              "00:00:59,000 --> 00:01:00,000\n"
                                                              "Foo.\n\n"
                                                              "0\n"
                                                              "00:01:57,800 --> 00:01:58,800\n"
                                                              "\n\n"
                                                              "0\n"
                                                              "00:01:58,900 --> 00:01:59,900\n"
                                                              "\n\n"
                                                              "2\n"
                                                              "00:02:00,000 --> 00:02:01,000\n"
                                                              "Bar.\n"))
                    (expect (point) :to-equal 71)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      (describe "appending between subtitles"
        (it "a single subtitle."
          (cl-loop for arg in (list nil 1) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:00:59,000 --> 00:01:00,000\n"
                                    "Foo.\n\n"
                                    "2\n"
                                    "00:02:00,000 --> 00:02:01,000\n"
                                    "Bar.\n"))
                    (subed-jump-to-subtitle-text 1)
                    (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                    (expect (buffer-string) :to-equal (concat "1\n"
                                                              "00:00:59,000 --> 00:01:00,000\n"
                                                              "Foo.\n\n"
                                                              "0\n"
                                                              "00:01:00,100 --> 00:01:01,100\n"
                                                              "\n\n"
                                                              "2\n"
                                                              "00:02:00,000 --> 00:02:01,000\n"
                                                              "Bar.\n"))
                    (expect (point) :to-equal 71)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtitles."
          (cl-loop for arg in (list 2) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:00:59,000 --> 00:01:00,000\n"
                                    "Foo.\n\n"
                                    "2\n"
                                    "00:02:00,000 --> 00:02:01,000\n"
                                    "Bar.\n"))
                    (subed-jump-to-subtitle-text 1)
                    (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                    (expect (buffer-string) :to-equal (concat "1\n"
                                                              "00:00:59,000 --> 00:01:00,000\n"
                                                              "Foo.\n\n"
                                                              "0\n"
                                                              "00:01:00,100 --> 00:01:01,100\n"
                                                              "\n\n"
                                                              "0\n"
                                                              "00:01:01,200 --> 00:01:02,200\n"
                                                              "\n\n"
                                                              "2\n"
                                                              "00:02:00,000 --> 00:02:01,000\n"
                                                              "Bar.\n"))
                    (expect (point) :to-equal 71)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      (describe "prepending to the first subtitle"
        (it "a single subtitle."
          (cl-loop for arg in (list '- -1 (list 4)) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:01:00,000 --> 00:01:01,000\n"
                                    "Foo.\n"))
                    (subed-jump-to-subtitle-text 1)
                    (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                    (expect (buffer-string) :to-equal (concat "0\n"
                                                              "00:00:58,900 --> 00:00:59,900\n"
                                                              "\n\n"
                                                              "1\n"
                                                              "00:01:00,000 --> 00:01:01,000\n"
                                                              "Foo.\n"))
                    (expect (point) :to-equal 33)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtitles."
          (cl-loop for arg in (list -2 (list 16)) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:01:00,000 --> 00:01:01,000\n"
                                    "Foo.\n"))
                    (subed-jump-to-subtitle-text 1)
                    (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                    (expect (buffer-string) :to-equal (concat "0\n"
                                                              "00:00:57,800 --> 00:00:58,800\n"
                                                              "\n\n"
                                                              "0\n"
                                                              "00:00:58,900 --> 00:00:59,900\n"
                                                              "\n\n"
                                                              "1\n"
                                                              "00:01:00,000 --> 00:01:01,000\n"
                                                              "Foo.\n"))
                    (expect (point) :to-equal 33)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      (describe "appending to the last subtitle"
        (it "a single subtitle."
          (cl-loop for arg in (list nil 1) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:00:59,000 --> 00:01:00,000\n"
                                    "Foo.\n"))
                    (subed-jump-to-subtitle-text 1)
                    (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                    (expect (buffer-string) :to-equal (concat "1\n"
                                                              "00:00:59,000 --> 00:01:00,000\n"
                                                              "Foo.\n\n"
                                                              "0\n"
                                                              "00:01:00,100 --> 00:01:01,100\n"
                                                              "\n"))
                    (expect (point) :to-equal 71)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtitles."
          (cl-loop for arg in (list 2) do
                   (with-temp-srt-buffer
                    (spy-on 'subed-regenerate-ids-soon)
                    (insert (concat "1\n"
                                    "00:00:59,000 --> 00:01:00,000\n"
                                    "Foo.\n"))
                    (subed-jump-to-subtitle-text 3)
                    (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                    (expect (buffer-string) :to-equal (concat "1\n"
                                                              "00:00:59,000 --> 00:01:00,000\n"
                                                              "Foo.\n\n"
                                                              "0\n"
                                                              "00:01:00,100 --> 00:01:01,100\n"
                                                              "\n\n"
                                                              "0\n"
                                                              "00:01:01,200 --> 00:01:02,200\n"
                                                              "\n"))
                    (expect (point) :to-equal 71)
                    (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                    (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      (describe "when there is not enough time for the subtitles"
        (describe "to append"
          (it "a single subtitle."
            (cl-loop for arg in (list nil 1) do
                     (with-temp-srt-buffer
                      (spy-on 'subed-regenerate-ids-soon)
                      (insert (concat "1\n"
                                      "00:00:59,000 --> 00:01:00,000\n"
                                      "Foo.\n\n"
                                      "2\n"
                                      "00:01:00,500 --> 00:01:05,000\n"
                                      "Bar.\n"))
                      (subed-jump-to-subtitle-text 1)
                      (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                      (expect (buffer-string) :to-equal (concat "1\n"
                                                                "00:00:59,000 --> 00:01:00,000\n"
                                                                "Foo.\n\n"
                                                                "0\n"
                                                                "00:01:00,100 --> 00:01:00,400\n"
                                                                "\n\n"
                                                                "2\n"
                                                                "00:01:00,500 --> 00:01:05,000\n"
                                                                "Bar.\n"))
                      (expect (point) :to-equal 71)
                      (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                      (spy-calls-reset 'subed-regenerate-ids-soon))))
          (it "multiple subtitles."
            (cl-loop for arg in (list 2) do
                     (with-temp-srt-buffer
                      (spy-on 'subed-regenerate-ids-soon)
                      (insert (concat "1\n"
                                      "00:00:59,000 --> 00:01:00,000\n"
                                      "Foo.\n\n"
                                      "2\n"
                                      "00:01:00,500 --> 00:01:05,000\n"
                                      "Bar.\n"))
                      (subed-jump-to-subtitle-text 1)
                      (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                      (expect (buffer-string) :to-equal (concat "1\n"
                                                                "00:00:59,000 --> 00:01:00,000\n"
                                                                "Foo.\n\n"
                                                                "0\n"
                                                                "00:01:00,100 --> 00:01:00,200\n"
                                                                "\n\n"
                                                                "0\n"
                                                                "00:01:00,300 --> 00:01:00,400\n"
                                                                "\n\n"
                                                                "2\n"
                                                                "00:01:00,500 --> 00:01:05,000\n"
                                                                "Bar.\n"))
                      (expect (point) :to-equal 71)
                      (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                      (spy-calls-reset 'subed-regenerate-ids-soon))))
          )
        (describe "to prepend"
          (describe "between subtitles"
            (it "a single subtitle."
              (cl-loop for arg in (list '- -1 (list 4)) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:59,000 --> 00:01:00,000\n"
                                        "Foo.\n\n"
                                        "2\n"
                                        "00:01:00,700 --> 00:01:05,000\n"
                                        "Bar.\n"))
                        (subed-jump-to-subtitle-text 2)
                        (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                        (expect (buffer-string) :to-equal (concat "1\n"
                                                                  "00:00:59,000 --> 00:01:00,000\n"
                                                                  "Foo.\n\n"
                                                                  "0\n"
                                                                  "00:01:00,100 --> 00:01:00,600\n"
                                                                  "\n\n"
                                                                  "2\n"
                                                                  "00:01:00,700 --> 00:01:05,000\n"
                                                                  "Bar.\n"))
                        (expect (point) :to-equal 71)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            (it "multiple subtitles."
              (cl-loop for arg in (list -2 (list 16)) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:59,000 --> 00:01:00,000\n"
                                        "Foo.\n\n"
                                        "2\n"
                                        "00:01:00,500 --> 00:01:05,000\n"
                                        "Bar.\n"))
                        (subed-jump-to-subtitle-text 2)
                        (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                        (expect (buffer-string) :to-equal (concat "1\n"
                                                                  "00:00:59,000 --> 00:01:00,000\n"
                                                                  "Foo.\n\n"
                                                                  "0\n"
                                                                  "00:01:00,100 --> 00:01:00,200\n"
                                                                  "\n\n"
                                                                  "0\n"
                                                                  "00:01:00,300 --> 00:01:00,400\n"
                                                                  "\n\n"
                                                                  "2\n"
                                                                  "00:01:00,500 --> 00:01:05,000\n"
                                                                  "Bar.\n"))
                        (expect (point) :to-equal 71)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            )
          (describe "before the first subtitle"
            (it "a single subtitle."
              (cl-loop for arg in (list '- -1 (list 4)) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:01,000 --> 00:00:02,000\n"
                                        "Foo.\n"))
                        (subed-jump-to-subtitle-text 1)
                        (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                        (expect (buffer-string) :to-equal (concat "0\n"
                                                                  "00:00:00,100 --> 00:00:00,900\n"
                                                                  "\n\n"
                                                                  "1\n"
                                                                  "00:00:01,000 --> 00:00:02,000\n"
                                                                  "Foo.\n"))
                        (expect (point) :to-equal 33)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            (it "multiple subtitles."
              (cl-loop for arg in (list -2 (list 16)) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:00,800 --> 00:00:03,000\n"
                                        "Foo.\n"))
                        (subed-jump-to-subtitle-text 2)
                        (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                        (expect (buffer-string) :to-equal (concat "0\n"
                                                                  "00:00:00,100 --> 00:00:00,350\n"
                                                                  "\n\n"
                                                                  "0\n"
                                                                  "00:00:00,450 --> 00:00:00,700\n"
                                                                  "\n\n"
                                                                  "1\n"
                                                                  "00:00:00,800 --> 00:00:03,000\n"
                                                                  "Foo.\n"))
                        (expect (point) :to-equal 33)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            )
          )
        )
      (describe "when there is not enough time for spacing"
        (describe "between subtitles"
          (describe "when prepending"
            (it "a single subtitle."
              (cl-loop for arg in (list '- -1 (list 4)) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:59,000 --> 00:01:00,000\n"
                                        "Foo.\n\n"
                                        "2\n"
                                        "00:01:00,005 --> 00:01:05,000\n"
                                        "Bar.\n"))
                        (subed-jump-to-subtitle-text 2)
                        (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                        (expect (buffer-string) :to-equal (concat "1\n"
                                                                  "00:00:59,000 --> 00:01:00,000\n"
                                                                  "Foo.\n\n"
                                                                  "0\n"
                                                                  "00:01:00,005 --> 00:01:00,005\n"
                                                                  "\n\n"
                                                                  "2\n"
                                                                  "00:01:00,005 --> 00:01:05,000\n"
                                                                  "Bar.\n"))
                        (expect (point) :to-equal 71)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            (it "multiple subtitles."
              (cl-loop for arg in (list -2 (list 16)) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:59,000 --> 00:01:00,000\n"
                                        "Foo.\n\n"
                                        "2\n"
                                        "00:01:00,025 --> 00:01:05,000\n"
                                        "Bar.\n"))
                        (subed-jump-to-subtitle-text 2)
                        (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                        (expect (buffer-string) :to-equal (concat "1\n"
                                                                  "00:00:59,000 --> 00:01:00,000\n"
                                                                  "Foo.\n\n"
                                                                  "0\n"
                                                                  "00:01:00,025 --> 00:01:00,025\n"
                                                                  "\n\n"
                                                                  "0\n"
                                                                  "00:01:00,025 --> 00:01:00,025\n"
                                                                  "\n\n"
                                                                  "2\n"
                                                                  "00:01:00,025 --> 00:01:05,000\n"
                                                                  "Bar.\n"))
                        (expect (point) :to-equal 71)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            )
          (describe "when appending"
            (it "a single subtitle."
              (cl-loop for arg in (list nil 1) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:59,000 --> 00:01:00,000\n"
                                        "Foo.\n\n"
                                        "2\n"
                                        "00:01:00,099 --> 00:01:05,000\n"
                                        "Bar.\n"))
                        (subed-jump-to-subtitle-text 1)
                        (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                        (expect (buffer-string) :to-equal (concat "1\n"
                                                                  "00:00:59,000 --> 00:01:00,000\n"
                                                                  "Foo.\n\n"
                                                                  "0\n"
                                                                  "00:01:00,000 --> 00:01:00,000\n"
                                                                  "\n\n"
                                                                  "2\n"
                                                                  "00:01:00,099 --> 00:01:05,000\n"
                                                                  "Bar.\n"))
                        (expect (point) :to-equal 71)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            (it "multiple subtitles."
              (cl-loop for arg in (list 2) do
                       (with-temp-srt-buffer
                        (spy-on 'subed-regenerate-ids-soon)
                        (insert (concat "1\n"
                                        "00:00:59,000 --> 00:01:00,000\n"
                                        "Foo.\n\n"
                                        "2\n"
                                        "00:01:00,075 --> 00:01:05,000\n"
                                        "Bar.\n"))
                        (subed-jump-to-subtitle-text 1)
                        (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                        (expect (buffer-string) :to-equal (concat "1\n"
                                                                  "00:00:59,000 --> 00:01:00,000\n"
                                                                  "Foo.\n\n"
                                                                  "0\n"
                                                                  "00:01:00,000 --> 00:01:00,000\n"
                                                                  "\n\n"
                                                                  "0\n"
                                                                  "00:01:00,000 --> 00:01:00,000\n"
                                                                  "\n\n"
                                                                  "2\n"
                                                                  "00:01:00,075 --> 00:01:05,000\n"
                                                                  "Bar.\n"))
                        (expect (point) :to-equal 71)
                        (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                        (spy-calls-reset 'subed-regenerate-ids-soon))))
            )
          )
        (describe "before the first subtitle"
          (it "a single subtitle."
            (cl-loop for arg in (list '- -1 (list 4)) do
                     (with-temp-srt-buffer
                      (spy-on 'subed-regenerate-ids-soon)
                      (insert (concat "1\n"
                                      "00:00:00,040 --> 00:00:05,000\n"
                                      "Foo.\n"))
                      (subed-jump-to-subtitle-text 1)
                      (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                      (expect (buffer-string) :to-equal (concat "0\n"
                                                                "00:00:00,040 --> 00:00:00,040\n"
                                                                "\n\n"
                                                                "1\n"
                                                                "00:00:00,040 --> 00:00:05,000\n"
                                                                "Foo.\n"))
                      (expect (point) :to-equal 33)
                      (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                      (spy-calls-reset 'subed-regenerate-ids-soon))))
          (it "multiple subtitles."
            (cl-loop for arg in (list -2 (list 16)) do
                     (with-temp-srt-buffer
                      (spy-on 'subed-regenerate-ids-soon)
                      (insert (concat "1\n"
                                      "00:00:00,024 --> 00:00:05,000\n"
                                      "Foo.\n"))
                      (subed-jump-to-subtitle-text 1)
                      (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                      (expect (buffer-string) :to-equal (concat "0\n"
                                                                "00:00:00,024 --> 00:00:00,024\n"
                                                                "\n\n"
                                                                "0\n"
                                                                "00:00:00,024 --> 00:00:00,024\n"
                                                                "\n\n"
                                                                "1\n"
                                                                "00:00:00,024 --> 00:00:05,000\n"
                                                                "Foo.\n"))
                      (expect (point) :to-equal 33)
                      (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                      (spy-calls-reset 'subed-regenerate-ids-soon))))
          )
        )
      )
    )

  (describe "Syncing player to point"
    :var (subed-mpv-playback-position)
    (before-each
      (setq subed-mpv-playback-position 0)
      (spy-on 'subed-subtitle-msecs-start :and-return-value 5000)
      (spy-on 'subed-subtitle-msecs-stop :and-return-value 6500)
      (spy-on 'subed-mpv-jump))
    (it "does not seek player if point is on current subtitle."
      (setq subed-mpv-playback-position 5000)
      (subed--sync-player-to-point)
      (expect 'subed-mpv-jump :not :to-have-been-called)
      (setq subed-mpv-playback-position 6500)
      (subed--sync-player-to-point)
      (expect 'subed-mpv-jump :not :to-have-been-called))
    (it "seeks player if point is on future subtitle."
      (with-temp-buffer
        (subed-srt-mode)
        (setq subed-mpv-playback-position 6501)
        (subed--sync-player-to-point)
        (expect 'subed-mpv-jump :to-have-been-called-with 5000)))
    (it "seeks player if point is on past subtitle."
      (with-temp-buffer
        (subed-srt-mode)
        (setq subed-mpv-playback-position 4999)
        (subed--sync-player-to-point)
        (expect 'subed-mpv-jump :to-have-been-called-with 5000)))
    )

  (describe "Temporarily disabling point-to-player syncing"
    (before-each
      (spy-on 'subed-disable-sync-point-to-player)
			(spy-on 'timerp :and-return-value t))
    (describe "when point-to-player syncing is disabled"
      (before-each
        (setq subed--point-sync-delay-after-motion-timer nil)
				(spy-on 'subed-sync-point-to-player-p :and-return-value nil)
        (spy-on 'run-at-time))
      (it "does not disable point-to-player syncing."
        (subed-disable-sync-point-to-player-temporarily)
        (expect 'subed-disable-sync-point-to-player :not :to-have-been-called))
      (it "does not schedule re-enabling of point-to-player syncing."
        (subed-disable-sync-point-to-player-temporarily)
        (expect 'run-at-time :not :to-have-been-called)
        (expect subed--point-sync-delay-after-motion-timer :to-be nil))
      )
    (describe "when point-to-player syncing is enabled"
      :var (subed--point-sync-delay-after-motion-timer)
      (before-each
        (spy-on 'subed-sync-point-to-player-p :and-return-value t)
        (spy-on 'run-at-time :and-return-value "mock timer")
        (spy-on 'cancel-timer)
        (setq subed--point-sync-delay-after-motion-timer nil))
      (it "disables point-to-player syncing."
        (subed-disable-sync-point-to-player-temporarily)
        (expect 'subed-disable-sync-point-to-player :to-have-been-called))
      (it "schedules re-enabling of point-to-player syncing."
        (subed-disable-sync-point-to-player-temporarily)
        (expect 'run-at-time :to-have-been-called)
        ;; Does not play well with undercover and edebug
        ;; (expect 'run-at-time :to-have-been-called-with
        ;;         subed-point-sync-delay-after-motion nil
        ;;         '(closure (t) nil
        ;;                   (setq subed--point-sync-delay-after-motion-timer nil)
        ;;                   (subed-enable-sync-point-to-player :quiet)))
        )
      (it "cancels previously scheduled re-enabling of point-to-player syncing."
        (subed-disable-sync-point-to-player-temporarily)
        (expect 'cancel-timer :not :to-have-been-called-with "mock timer")
        (subed-disable-sync-point-to-player-temporarily)
        (expect 'cancel-timer :to-have-been-called-with "mock timer")
        (expect 'cancel-timer :to-have-been-called-times 1))
      )
    )

  (describe "Splitting subtitles"
    (it "handles empty subtitles"
      (with-temp-srt-buffer
			 (insert "1
00:01:23,000 --> 00:02:34,567

")
			 (forward-line -1)
			 (let ((subed-subtitle-spacing 100))
				 (subed-split-subtitle 100))
			 (expect (buffer-string) :to-equal
							 "1
00:01:23,000 --> 00:01:23,100


0
00:01:23,200 --> 00:02:34,567

")))
    (describe "when there are multiple lines"
      (describe "at the last subtitle"
        :var ((text "1
00:01:23,000 --> 00:02:34,567
This is a subtitle
that has two lines.

")
              (subed-subtitle-spacing 100))
        (it "properly splits text when called at the beginning of the subtitle."
          (with-temp-srt-buffer
					 (insert text)
					 (re-search-backward "This is a subtitle")
					 (goto-char (match-beginning 0))
					 (save-excursion (subed-split-subtitle 100))
					 (expect (buffer-string) :to-equal "1
00:01:23,000 --> 00:01:23,100


0
00:01:23,200 --> 00:02:34,567
This is a subtitle
that has two lines.
")))
        (it "properly splits text when called in the middle of the subtitle."
          (with-temp-srt-buffer
					 (insert text)
					 (re-search-backward "This is a")
					 (goto-char (match-end 0))
					 (subed-split-subtitle 100)
					 (expect (subed-subtitle-text 1) :to-equal "This is a")
					 (subed-regenerate-ids)
					 (expect (subed-subtitle-text 2) :to-equal "subtitle\nthat has two lines.")))
        (it "properly splits text when called at the end of a line in the middle of the subtitle"
          (with-temp-srt-buffer
					 (insert text)
					 (re-search-backward "This is a subtitle")
					 (goto-char (match-end 0))
					 (subed-split-subtitle 100)
					 (expect (subed-subtitle-text 1) :to-equal "This is a subtitle")
					 (subed-regenerate-ids)
					 (expect (subed-subtitle-text 2) :to-equal "that has two lines.")))
        (it "properly splits text when called at the beginning of a line in the middle of the subtitle."
          (with-temp-srt-buffer
					 (insert text)
					 (re-search-backward "that has two lines")
					 (goto-char (match-beginning 0))
					 (subed-split-subtitle 100)
					 (expect (buffer-string) :to-equal "1
00:01:23,000 --> 00:01:23,100
This is a subtitle

0
00:01:23,200 --> 00:02:34,567
that has two lines.
")  (expect (subed-subtitle-text 1) :to-equal "This is a subtitle")
					 (subed-regenerate-ids)
					 (expect (subed-subtitle-text 2) :to-equal "that has two lines.")))
        (it "properly splits text when called at the end of the subtitle."
          (with-temp-srt-buffer
					 (insert text)
					 (subed-jump-to-subtitle-end 1)
					 (subed-split-subtitle 100)
					 (expect (subed-subtitle-text 1) :to-equal "This is a subtitle\nthat has two lines.")
					 (subed-regenerate-ids)
					 (expect (subed-subtitle-text 2) :to-equal "")))
        (it "properly splits text when called before whitespace at the end of the subtitle."
          (with-temp-srt-buffer
					 (insert text)
					 (subed-jump-to-subtitle-end 1)
					 (save-excursion (insert "  "))
					 (subed-split-subtitle 100)
					 (expect (subed-subtitle-text 1) :to-equal "This is a subtitle\nthat has two lines.")
					 (subed-regenerate-ids)
					 (expect (subed-subtitle-text 2) :to-equal ""))))
      (describe "with another subtitle after it"
        :var ((text "1
00:01:23,000 --> 00:02:34,567
This is a subtitle
that has two lines.

2
00:05:00,000 --> 00:06:00,000
This is another.
"))
        (it "properly splits text when called at the beginning of the subtitle."
          (with-temp-srt-buffer
					 (insert text)
					 (re-search-backward "This is a subtitle")
					 (goto-char (match-beginning 0))
					 (let ((subed-subtitle-spacing 100))
						 (save-excursion (subed-split-subtitle 100))
						 (expect subed-subtitle-spacing :to-equal 100))
					 (expect (buffer-string) :to-equal "1
00:01:23,000 --> 00:01:23,100

0
00:01:23,200 --> 00:02:34,567
This is a subtitle
that has two lines.

2
00:05:00,000 --> 00:06:00,000
This is another.
")))
        (it "properly splits text when called in the middle of the subtitle."
          (with-temp-srt-buffer
					 (insert text)
					 (re-search-backward "This is a subtitle")
					 (goto-char (match-end 0))
					 (backward-word 1)
					 (subed-split-subtitle 100)
					 (expect (subed-subtitle-text 1) :to-equal "This is a")
					 (subed-regenerate-ids)
					 (expect (subed-subtitle-text 2) :to-equal "subtitle\nthat has two lines.")))
        (it "properly splits text when called at the end of a line in the middle of the subtitle"
          (with-temp-srt-buffer
					 (insert text)
					 (re-search-backward "This is a subtitle")
					 (goto-char (match-end 0))
					 (subed-split-subtitle 100)
					 (expect (subed-subtitle-text 1) :to-equal "This is a subtitle")
					 (subed-regenerate-ids)
					 (expect (subed-subtitle-text 2) :to-equal "that has two lines.")))
        (it "properly splits text when called at the beginning of a line in the middle of the subtitle."
          (with-temp-srt-buffer
					 (insert text)
					 (re-search-backward "that has two lines")
					 (goto-char (match-beginning 0))
					 (let ((subed-enforce-time-boundaries 'adjust)
								 (subed-subtitle-spacing 100))
						 (subed-split-subtitle 100))
					 (expect (buffer-string) :to-equal "1
00:01:23,000 --> 00:01:23,100
This is a subtitle

0
00:01:23,200 --> 00:02:34,567
that has two lines.

2
00:05:00,000 --> 00:06:00,000
This is another.
")  (expect (subed-subtitle-text 1) :to-equal "This is a subtitle")
					 (subed-regenerate-ids)
					 (expect (subed-subtitle-text 2) :to-equal "that has two lines.")))
        (it "properly splits text when called at the end of the subtitle."
          (with-temp-srt-buffer
					 (insert text)
					 (subed-jump-to-subtitle-end 1)
					 (subed-split-subtitle 100)
					 (expect (subed-subtitle-text 1) :to-equal "This is a subtitle\nthat has two lines.")
					 (subed-regenerate-ids)
					 (expect (subed-subtitle-text 2) :to-equal "")))
        (it "properly splits text when called before whitespace at the end of the subtitle."
          (with-temp-srt-buffer
					 (insert text)
					 (subed-jump-to-subtitle-end 1)
					 (save-excursion (insert "  "))
					 (subed-split-subtitle 100)
					 (expect (subed-subtitle-text 1) :to-equal "This is a subtitle\nthat has two lines.")
					 (subed-regenerate-ids)
					 (expect (subed-subtitle-text 2) :to-equal "")))
        (it "accepts a timestamp."
          (with-temp-srt-buffer
					 (insert text)
					 (re-search-backward "subtitle")
					 (end-of-line)
					 (subed-split-subtitle "00:01:43,100")
					 (expect (subed-subtitle-msecs-start) :to-equal 103100)
					 (subed-backward-subtitle-time-start)
					 (expect (subed-subtitle-msecs-stop) :to-equal (- 103100 subed-subtitle-spacing))))))
    (describe "when playing the media in MPV"
      (it "splits at point in the middle of the subtitle."
        (with-temp-srt-buffer
				 (insert mock-srt-data)
				 (re-search-backward "Foo\\.")
				 (end-of-line)
				 (save-excursion (insert " Some text here."))
				 (setq-local subed-mpv-playback-position 61600)
				 (setq-local subed-subtitle-spacing 100)
				 (subed-split-subtitle)
				 (expect (subed-subtitle-msecs-start) :to-equal 61700)
				 (expect (subed-subtitle-msecs-stop) :to-equal 65123)
				 (expect (subed-subtitle-text) :to-equal "Some text here.")
				 (subed-backward-subtitle-time-start)
				 (expect (subed-subtitle-msecs-stop) :to-equal 61600)
				 (expect (subed-subtitle-text) :to-equal "Foo.")))
      (it "splits at the end even if there are spaces."
        (with-temp-srt-buffer
				 (insert mock-srt-data)
				 (re-search-backward "Foo\\.")
				 (subed-jump-to-subtitle-end)
				 (insert " ")
				 (setq-local subed-mpv-playback-position 61600)
				 (setq-local subed-subtitle-spacing 100)
				 (subed-split-subtitle)
				 (expect (subed-subtitle-text) :to-equal "")
				 (expect (subed-subtitle-msecs-start) :to-equal 61700)
				 (expect (subed-subtitle-msecs-stop) :to-equal 65123)
				 (subed-backward-subtitle-time-start)
				 (expect (subed-subtitle-text) :to-equal "Foo.")
				 (expect (subed-subtitle-msecs-stop) :to-equal 61600)))
      (it "splits at the beginning."
        (with-temp-srt-buffer
				 (save-excursion (insert mock-srt-data))
				 (subed-jump-to-subtitle-text)
				 (setq-local subed-mpv-playback-position 61600)
				 (setq-local subed-subtitle-spacing 100)
				 (subed-split-subtitle)
				 (expect (subed-subtitle-text) :to-equal "Foo.")
				 (expect (subed-subtitle-msecs-start) :to-equal 61700)
				 (expect (subed-subtitle-msecs-stop) :to-equal 65123)
				 (subed-backward-subtitle-time-start)
				 (expect (subed-subtitle-text) :to-equal "")
				 (expect (subed-subtitle-msecs-stop) :to-equal 61600))))
    (describe "when a positive offset is specified"
      (it "splits from the starting time."
        (with-temp-srt-buffer
				 (insert mock-srt-data)
				 (re-search-backward "Foo\\.")
				 (end-of-line)
				 (save-excursion (insert " Some text here."))
				 (setq-local subed-subtitle-spacing 100)
				 (subed-split-subtitle 300)
				 (expect (subed-subtitle-msecs-start) :to-equal 61400)
				 (expect (subed-subtitle-msecs-stop) :to-equal 65123)
				 (expect (subed-subtitle-text) :to-equal "Some text here.")
				 (subed-backward-subtitle-time-start)
				 (expect (subed-subtitle-msecs-stop) :to-equal 61300)
				 (expect (subed-subtitle-text) :to-equal "Foo.")))
      (it "uses the offset instead of the playing position."
        (with-temp-srt-buffer
				 (insert mock-srt-data)
				 (re-search-backward "Foo\\.")
				 (setq-local subed-mpv-playback-position 61600)
				 (setq-local subed-subtitle-spacing 100)
				 (subed-split-subtitle 300)
				 (expect (subed-subtitle-msecs-start) :to-equal 61400)
				 (expect (subed-subtitle-msecs-stop) :to-equal 65123))))
    (describe "when a negative offset is specified"
      (it "splits from the ending time."
        (with-temp-srt-buffer
				 (insert mock-srt-data)
				 (re-search-backward "Foo\\.")
				 (end-of-line)
				 (save-excursion (insert " Some text here."))
				 (setq-local subed-subtitle-spacing 100)
				 (subed-split-subtitle -300)
				 (expect (subed-subtitle-msecs-start) :to-equal 64923)
				 (expect (subed-subtitle-msecs-stop) :to-equal 65123)
				 (expect (subed-subtitle-text) :to-equal "Some text here.")
				 (subed-backward-subtitle-time-start)
				 (expect (subed-subtitle-msecs-stop) :to-equal 64823)
				 (expect (subed-subtitle-text) :to-equal "Foo.")))
      (it "uses the offset instead of the playing position."
        (with-temp-srt-buffer
				 (insert mock-srt-data)
				 (re-search-backward "Foo\\.")
				 (setq-local subed-subtitle-spacing 100)
				 (setq-local subed-mpv-playback-position 61600)
				 (subed-split-subtitle -300)
				 (expect (subed-subtitle-msecs-start) :to-equal 64923)
				 (expect (subed-subtitle-msecs-stop) :to-equal 65123)
				 (subed-backward-subtitle-time-start)
				 (expect (subed-subtitle-msecs-stop) :to-equal 64823))))
    (describe "when nothing is specified"
      (it "splits proportional to the location."
        (with-temp-srt-buffer
				 (insert mock-srt-data)
				 (re-search-backward "Foo\\.")
				 (end-of-line)
				 (save-excursion (insert " Bar"))
				 (setq-local subed-subtitle-spacing 100)
				 (subed-split-subtitle)
				 (expect (subed-subtitle-msecs-start) :to-equal 63161)
				 (expect (subed-subtitle-msecs-stop) :to-equal 65123)
				 (expect (subed-subtitle-text) :to-equal "Bar")
				 (subed-backward-subtitle-time-start)
				 (expect (subed-subtitle-msecs-stop) :to-equal 63061)
				 (expect (subed-subtitle-text) :to-equal "Foo.")))))

  (describe "Scaling subtitles"
    (it "without providing beginning and end."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (spy-on 'subed-scale-subtitles :and-call-through)
			 (subed-scale-subtitles-forward 1000)
			 (expect (subed-subtitle-msecs-start 1) :to-equal 61000)
			 (expect (subed-subtitle-msecs-stop 1) :to-equal 65123)
			 (expect (subed-subtitle-msecs-start 2) :to-equal 122734)
			 (expect (subed-subtitle-msecs-stop 2) :to-equal 130845)
			 (expect (subed-subtitle-msecs-start 3) :to-equal 184450)
			 (expect (subed-subtitle-msecs-stop 3) :to-equal 196500)
			 (subed-scale-subtitles-backward 1000)
			 (expect (subed-subtitle-msecs-start 1) :to-equal 61000)
			 (expect (subed-subtitle-msecs-stop 1) :to-equal 65123)
			 (expect (subed-subtitle-msecs-start 2) :to-equal 122234)
			 (expect (subed-subtitle-msecs-stop 2) :to-equal 130345)
			 (expect (subed-subtitle-msecs-start 3) :to-equal 183450)
			 (expect (subed-subtitle-msecs-stop 3) :to-equal 195500)
			 (expect (spy-calls-all-args 'subed-scale-subtitles) :to-equal
							 '((+1000 nil nil)
								 (-1000 nil nil)))))
    (it "without providing end."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-scale-subtitles 1000 (point-min) nil)
			 (expect (subed-subtitle-msecs-start 1) :to-equal 61000)
			 (expect (subed-subtitle-msecs-stop 1) :to-equal 65123)
			 (expect (subed-subtitle-msecs-start 2) :to-equal 122734)
			 (expect (subed-subtitle-msecs-stop 2) :to-equal 130845)
			 (expect (subed-subtitle-msecs-start 3) :to-equal 184450)
			 (expect (subed-subtitle-msecs-stop 3) :to-equal 196500)
			 (subed-scale-subtitles -1000 (point-min) nil)
			 (expect (subed-subtitle-msecs-start 1) :to-equal 61000)
			 (expect (subed-subtitle-msecs-stop 1) :to-equal 65123)
			 (expect (subed-subtitle-msecs-start 2) :to-equal 122234)
			 (expect (subed-subtitle-msecs-stop 2) :to-equal 130345)
			 (expect (subed-subtitle-msecs-start 3) :to-equal 183450)
			 (expect (subed-subtitle-msecs-stop 3) :to-equal 195500)))
    (it "without providing beginning."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-scale-subtitles 1000 nil (point-max))
			 (expect (subed-subtitle-msecs-start 1) :to-equal 61000)
			 (expect (subed-subtitle-msecs-stop 1) :to-equal 65123)
			 (expect (subed-subtitle-msecs-start 2) :to-equal 122734)
			 (expect (subed-subtitle-msecs-stop 2) :to-equal 130845)
			 (expect (subed-subtitle-msecs-start 3) :to-equal 184450)
			 (expect (subed-subtitle-msecs-stop 3) :to-equal 196500)
			 (subed-scale-subtitles -1000 nil (point-max))
			 (expect (subed-subtitle-msecs-start 1) :to-equal 61000)
			 (expect (subed-subtitle-msecs-stop 1) :to-equal 65123)
			 (expect (subed-subtitle-msecs-start 2) :to-equal 122234)
			 (expect (subed-subtitle-msecs-stop 2) :to-equal 130345)
			 (expect (subed-subtitle-msecs-start 3) :to-equal 183450)
			 (expect (subed-subtitle-msecs-stop 3) :to-equal 195500)))
    (it "with active region on entire buffer."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (let ((beg (point-min))
						 (end (point-max)))
				 (spy-on 'subed-scale-subtitles :and-call-through)
				 (spy-on 'region-beginning :and-return-value beg)
				 (spy-on 'region-end :and-return-value end)
				 (setq mark-active t)
				 (subed-scale-subtitles-forward 1000)
				 (expect (subed-subtitle-msecs-start 1) :to-equal 61000)
				 (expect (subed-subtitle-msecs-stop 1) :to-equal 65123)
				 (expect (subed-subtitle-msecs-start 2) :to-equal 122734)
				 (expect (subed-subtitle-msecs-stop 2) :to-equal 130845)
				 (expect (subed-subtitle-msecs-start 3) :to-equal 184450)
				 (expect (subed-subtitle-msecs-stop 3) :to-equal 196500)
				 (subed-scale-subtitles-backward 1000)
				 (expect (subed-subtitle-msecs-start 1) :to-equal 61000)
				 (expect (subed-subtitle-msecs-stop 1) :to-equal 65123)
				 (expect (subed-subtitle-msecs-start 2) :to-equal 122234)
				 (expect (subed-subtitle-msecs-stop 2) :to-equal 130345)
				 (expect (subed-subtitle-msecs-start 3) :to-equal 183450)
				 (expect (subed-subtitle-msecs-stop 3) :to-equal 195500)
				 (expect (spy-calls-all-args 'subed-scale-subtitles) :to-equal
								 `((+1000 ,beg ,end)
									 (-1000 ,beg ,end))))))
    (it "with a zero msec extension/contraction."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-scale-subtitles-forward 0)
			 (expect (subed-subtitle-msecs-start 1) :to-equal 61000)
			 (expect (subed-subtitle-msecs-stop 1) :to-equal 65123)
			 (expect (subed-subtitle-msecs-start 2) :to-equal 122234)
			 (expect (subed-subtitle-msecs-stop 2) :to-equal 130345)
			 (expect (subed-subtitle-msecs-start 3) :to-equal 183450)
			 (expect (subed-subtitle-msecs-stop 3) :to-equal 195500)
			 (subed-scale-subtitles-backward 0)
			 (expect (subed-subtitle-msecs-start 1) :to-equal 61000)
			 (expect (subed-subtitle-msecs-stop 1) :to-equal 65123)
			 (expect (subed-subtitle-msecs-start 2) :to-equal 122234)
			 (expect (subed-subtitle-msecs-stop 2) :to-equal 130345)
			 (expect (subed-subtitle-msecs-start 3) :to-equal 183450)
			 (expect (subed-subtitle-msecs-stop 3) :to-equal 195500)))
    (it "with active region on one subtitle."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (let ((beg 77) ; point at ID of third subtitle
						 (end (point-max)))
				 (spy-on 'region-beginning :and-return-value beg)
				 (spy-on 'region-end :and-return-value end)
				 (spy-on 'user-error :and-call-through)
				 (setq mark-active t)
				 (expect (subed-scale-subtitles-forward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale with fewer than 3 subtitles")))
				 (expect (buffer-string) :to-equal mock-srt-data))))
    (it "with active region on two subtitles."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (let ((beg 39) ; point at ID of second subtitle
						 (end (point-max)))
				 (spy-on 'region-beginning :and-return-value beg)
				 (spy-on 'region-end :and-return-value end)
				 (spy-on 'user-error :and-call-through)
				 (setq mark-active t)
				 (expect (subed-scale-subtitles-forward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale with only 2 subtitles")))
				 (expect (buffer-string) :to-equal mock-srt-data))))
    (it "with active region contraction."
      (with-temp-srt-buffer
			 (insert (concat "1\n"
											 "00:00:43,233 --> 00:00:45,861\n"
											 "a\n"
											 "\n"
											 "2\n"
											 "00:00:51,675 --> 00:00:54,542\n"
											 "b\n"
											 "\n"
											 "3\n"
											 "00:01:00,717 --> 00:01:02,378\n"
											 "c\n"
											 "\n"
											 "4\n"
											 "00:01:02,452 --> 00:01:05,216\n"
											 "d\n"))
			 (let ((beg (point-min))
						 (end 103)) ; point at TEXT of third subtitle
				 (spy-on 'region-beginning :and-return-value beg)
				 (spy-on 'region-end :and-return-value end)
				 (setq mark-active t)
				 (subed-scale-subtitles-backward 1000)
				 (expect (subed-subtitle-msecs-start 1) :to-equal 43233)
				 (expect (subed-subtitle-msecs-stop 1) :to-equal 45861)
				 (expect (subed-subtitle-msecs-start 2) :to-equal 51192)
				 (expect (subed-subtitle-msecs-stop 2) :to-equal 54059)
				 (expect (subed-subtitle-msecs-start 3) :to-equal 59717)
				 (expect (subed-subtitle-msecs-stop 3) :to-equal 61378)
				 (expect (subed-subtitle-msecs-start 4) :to-equal 62452)
				 (expect (subed-subtitle-msecs-stop 4) :to-equal 65216))))
    (it "with active region extension."
      (with-temp-srt-buffer
			 (insert (concat "1\n"
											 "00:00:43,233 --> 00:00:45,861\n"
											 "a\n"
											 "\n"
											 "2\n"
											 "00:00:51,192 --> 00:00:54,059\n"
											 "b\n"
											 "\n"
											 "3\n"
											 "00:00:59,717 --> 00:01:01,378\n"
											 "c\n"
											 "\n"
											 "4\n"
											 "00:01:02,452 --> 00:01:05,216\n"
											 "d\n"))
			 (let ((beg (point-min))
						 (end 103)) ; point at TEXT of third subtitle
				 (spy-on 'region-beginning :and-return-value beg)
				 (spy-on 'region-end :and-return-value end)
				 (setq mark-active t)
				 (setq-local subed-subtitle-spacing 0)
				 (subed-scale-subtitles-forward 1000)
				 (expect (subed-subtitle-msecs-start 1) :to-equal 43233)
				 (expect (subed-subtitle-msecs-stop 1) :to-equal 45861)
				 (expect (subed-subtitle-msecs-start 2) :to-equal 51675)
				 (expect (subed-subtitle-msecs-stop 2) :to-equal 54542)
				 (expect (subed-subtitle-msecs-start 3) :to-equal 60717)
				 (expect (subed-subtitle-msecs-stop 3) :to-equal 62378)
				 (expect (subed-subtitle-msecs-start 4) :to-equal 62452)
				 (expect (subed-subtitle-msecs-stop 4) :to-equal 65216))))
		(describe "when active region extension overlaps next subtitle"
			(it "reports an error"
				(with-temp-srt-buffer
				 (let ((initial-contents
								(concat "1\n"
												"00:00:43,233 --> 00:00:45,861\n"
												"a\n"
												"\n"
												"2\n"
												"00:00:51,675 --> 00:00:54,542\n"
												"b\n"
												"\n"
												"3\n"
												"00:01:00,717 --> 00:01:02,378\n"
												"c\n"
												"\n"
												"4\n"
												"00:01:02,452 --> 00:01:05,216\n"
												"d\n"))
							 (beg 1)
							 (end 103)) ; point at TEXT of third subtitle
					 (spy-on 'region-beginning :and-return-value beg)
					 (spy-on 'region-end :and-return-value end)
					 (spy-on 'user-error :and-call-through)
					 (insert initial-contents)
					 (setq mark-active t)
					 (let ((subed-enforce-time-boundaries 'error))
						 (expect (subed-scale-subtitles-forward 1000) :to-throw 'error))
					 (expect (spy-calls-all-args 'user-error) :to-equal
									 '(("Can't scale when extension would overlap subsequent subtitles")))
					 (expect (buffer-string) :to-equal initial-contents))))
			(it "when end subtitle start time moved to same time as begin subtitle start time."
				(with-temp-srt-buffer
				 (insert mock-srt-data)
				 (let ((beg (point-min))
							 (end (point-max))
							 (delta (- (subed-subtitle-msecs-start 3)
												 (subed-subtitle-msecs-start 1))))
					 (spy-on 'region-beginning :and-return-value beg)
					 (spy-on 'region-end :and-return-value end)
					 (spy-on 'user-error :and-call-through)
					 (setq mark-active t)
					 (expect (subed-scale-subtitles-backward delta) :to-throw 'error)
					 (expect (spy-calls-all-args 'user-error) :to-equal
									 '(("Can't scale when contraction would eliminate region")))
					 (expect (buffer-string) :to-equal mock-srt-data)))))
    (it "when end subtitle start time moved to just before begin subtitle start time."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (let ((beg (point-min))
						 (end (point-max))
						 (delta (- (subed-subtitle-msecs-start 3)
											 (subed-subtitle-msecs-start 1))))
				 (spy-on 'region-beginning :and-return-value beg)
				 (spy-on 'region-end :and-return-value end)
				 (spy-on 'user-error :and-call-through)
				 (setq mark-active t)
				 (expect (subed-scale-subtitles-backward (+ delta 1)) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale when contraction would eliminate region")))
				 (expect (buffer-string) :to-equal mock-srt-data))))
    (it "when end subtitle start time moved to just after begin subtitle start time."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (let ((beg (point-min))
						 (end (point-max))
						 (delta (- (subed-subtitle-msecs-start 3)
											 (subed-subtitle-msecs-start 1))))
				 (spy-on 'region-beginning :and-return-value beg)
				 (spy-on 'region-end :and-return-value end)
				 (setq mark-active t)
				 (subed-scale-subtitles-backward (- delta 1))
				 (expect (subed-subtitle-msecs-start 1) :to-equal 61000)
				 (expect (subed-subtitle-msecs-stop 1) :to-equal 65123)
				 (expect (subed-subtitle-msecs-start 2) :to-equal 61001)
				 (expect (subed-subtitle-msecs-stop 2) :to-equal 69112)
				 (expect (subed-subtitle-msecs-start 3) :to-equal 61001)
				 (expect (subed-subtitle-msecs-stop 3) :to-equal 73051))))
    (it "when begin start time same as end start time."
      (with-temp-srt-buffer
			 (let ((initial-contents
              (concat "1\n"
                      "00:01:01,000 --> 00:01:05,123\n"
                      "Foo.\n"
                      "\n"
                      "2\n"
                      "00:01:01,000 --> 00:01:05,123\n"
                      "Bar.\n"
                      "\n"
                      "3\n"
                      "00:01:01,000 --> 00:01:05,123\n"
                      "Baz.\n")))
				 (spy-on 'user-error :and-call-through)
				 (insert initial-contents)
				 (expect (subed-scale-subtitles-forward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale subtitle range with 0 time interval")))
				 (expect (buffer-string) :to-equal initial-contents)
				 (spy-calls-reset 'user-error)
				 (expect (subed-scale-subtitles-backward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale subtitle range with 0 time interval")))
				 (expect (buffer-string) :to-equal initial-contents))))
    (it "when buffer is empty."
      (spy-on 'user-error :and-call-through)
      (with-temp-srt-buffer
			 (expect (subed-scale-subtitles-forward 1000) :to-throw 'error)
			 (expect (spy-calls-all-args 'user-error) :to-equal
							 '(("Can't scale with fewer than 3 subtitles")))
			 (expect (buffer-string) :to-equal "")
			 (spy-calls-reset 'user-error)
			 (expect (subed-scale-subtitles-backward 1000) :to-throw 'error)
			 (expect (spy-calls-all-args 'user-error) :to-equal
							 '(("Can't scale with fewer than 3 subtitles")))
			 (expect (buffer-string) :to-equal "")))
    (it "when buffer contains one subtitle."
      (spy-on 'user-error :and-call-through)
      (with-temp-srt-buffer
			 (let ((initial-contents
              (concat "1\n"
                      "00:01:01,000 --> 00:01:05,123\n"
                      "Foo.\n")))
				 (insert initial-contents)
				 (expect (subed-scale-subtitles-forward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale with fewer than 3 subtitles")))
				 (expect (buffer-string) :to-equal initial-contents)
				 (spy-calls-reset 'user-error)
				 (expect (subed-scale-subtitles-backward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale with fewer than 3 subtitles")))
				 (expect (buffer-string) :to-equal initial-contents))))
    (it "when buffer contains two subtitles."
      (spy-on 'user-error :and-call-through)
      (with-temp-srt-buffer
			 (let ((initial-contents
              (concat "1\n"
                      "00:01:01,000 --> 00:01:05,123\n"
                      "Foo.\n"
                      "\n"
                      "2\n"
                      "00:02:02,234 --> 00:02:10,345\n"
                      "Bar.\n")))
				 (insert initial-contents)
				 (expect (subed-scale-subtitles-forward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale with only 2 subtitles")))
				 (expect (buffer-string) :to-equal initial-contents)
				 (spy-calls-reset 'user-error)
				 (expect (subed-scale-subtitles-backward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale with only 2 subtitles")))
				 (expect (buffer-string) :to-equal initial-contents))))
    (it "reports an error if the subtitle in region has a start time after end start time."
      (spy-on 'user-error :and-call-through)
      (with-temp-srt-buffer
			 (let ((initial-contents
              (concat "1\n"
                      "00:01:01,000 --> 00:01:05,123\n"
                      "Foo.\n"
                      "\n"
                      "2\n"
                      "00:03:03,45 --> 00:03:15,5\n"
                      "Baz.\n"
                      "\n"
                      "3\n"
                      "00:02:02,234 --> 00:02:10,345\n"
                      "Bar.\n"))
						 (subed-enforce-time-boundaries 'error)
						 (subed-subtitle-spacing 100))
				 (insert initial-contents)
				 (expect subed-enforce-time-boundaries :to-equal 'error)
				 (expect (subed-scale-subtitles-forward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale when nonchronological subtitles exist")))
				 (expect (buffer-string) :to-equal initial-contents)
				 (spy-calls-reset 'user-error)
				 (expect (subed-scale-subtitles-backward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale when nonchronological subtitles exist")))
				 (expect (buffer-string) :to-equal initial-contents))))
    (it "with first subtitle containing no timestamp."
      (spy-on 'user-error :and-call-through)
      (with-temp-srt-buffer
			 (let ((initial-contents
              (concat "1\n"
                      "a\n"
                      "\n"
                      "2\n"
                      "00:00:51,675 --> 00:00:54,542\n"
                      "b\n"
                      "\n"
                      "3\n"
                      "00:01:00,717 --> 00:01:02,378\n"
                      "c\n")))
				 (insert initial-contents)
				 (expect (subed-scale-subtitles-forward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale when first subtitle timestamp missing")))
				 (expect (buffer-string) :to-equal initial-contents)
				 (spy-calls-reset 'user-error)
				 (expect (subed-scale-subtitles-backward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale when first subtitle timestamp missing")))
				 (expect (buffer-string) :to-equal initial-contents))))
    (it "with last subtitle containing no timestamp."
      (spy-on 'user-error :and-call-through)
      (with-temp-srt-buffer
			 (let ((initial-contents
              (concat "1\n"
                      "00:00:43,233 --> 00:00:45,861\n"
                      "a\n"
                      "\n"
                      "2\n"
                      "00:00:51,675 --> 00:00:54,542\n"
                      "b\n"
                      "\n"
                      "3\n"
                      "c\n")))
				 (insert initial-contents)
				 (expect (subed-scale-subtitles-forward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale when last subtitle timestamp missing")))
				 (expect (buffer-string) :to-equal initial-contents)
				 (spy-calls-reset 'user-error)
				 (expect (subed-scale-subtitles-backward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale when last subtitle timestamp missing")))
				 (expect (buffer-string) :to-equal initial-contents))))
    (it "with subtitle in region containing no timestamp."
      (spy-on 'user-error :and-call-through)
      (with-temp-srt-buffer
			 (let ((initial-contents
              (concat "1\n"
                      "00:00:43,233 --> 00:00:45,861\n"
                      "a\n"
                      "\n"
                      "2\n"
                      "b\n"
                      "\n"
                      "3\n"
                      "00:01:00,717 --> 00:01:02,378\n"
                      "c\n")))
				 (insert initial-contents)
				 (expect (subed-scale-subtitles-forward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale when subtitle timestamp missing")))
				 (expect (buffer-string) :to-equal initial-contents)
				 (spy-calls-reset 'user-error)
				 (expect (subed-scale-subtitles-backward 1000) :to-throw 'error)
				 (expect (spy-calls-all-args 'user-error) :to-equal
								 '(("Can't scale when subtitle timestamp missing")))
				 (expect (buffer-string) :to-equal initial-contents))))
    (it "with out-of-order range."
      (spy-on 'user-error :and-call-through)
      (with-temp-srt-buffer
			 (expect (subed-scale-subtitles 1000 5 4) :to-throw 'error)
			 (expect (spy-calls-all-args 'user-error) :to-equal
							 '(("Can't scale with improper range"))))))

  (describe "Trimming subtitles"
    (describe "when spacing is 0"
      (it "detects overlaps"
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:05,000\nA\n\n"
								 "2\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 0))
					 (expect (subed--identify-overlaps)
									 :to-equal '(1)))))
      (it "ignores non-overlapping subtitles"
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:04,000\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 0))
					 (expect (subed--identify-overlaps)
									 :to-equal nil)))))
    (describe "when spacing is 1"
      (it "detects overlaps"
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:04,000\nA\n\n"
								 "2\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 1))
					 (expect (subed--identify-overlaps)
									 :to-equal '(1)))))
      (it "ignores non-overlapping subtitles"
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,999\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:04,000\nA\n\n")
				 (let ((subed-subtitle-spacing 1))
					 (expect (subed--identify-overlaps)
									 :to-equal nil)))))
    (describe "when spacing is greater"
      (it "detects overlaps because of spacing"
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,999\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:05,000\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100))
					 (expect (subed--identify-overlaps)
									 :to-equal '(1 2)))))
      (it "ignores non-overlapping subtitles."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:03,500\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100))
					 (expect (subed--identify-overlaps)
									 :to-equal nil)))))
    (describe "overlap end time"
      (it "sets it to the next timestamp minus spacing."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:04,500\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100))
					 (subed-jump-to-subtitle-id 2)
					 (subed-trim-overlap-stop)
					 (expect (subed-subtitle-msecs-stop) :to-equal 3900))))
      (it "sets it to the next timestamp minus the argument."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:04,500\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100))
					 (subed-jump-to-subtitle-id 2)
					 (subed-trim-overlap-stop 500)
					 (expect (subed-subtitle-msecs-stop) :to-equal 3500))))
      (it "ignores non-overlapping subtitles."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:04,500\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100))
					 (subed-jump-to-subtitle-id 1)
					 (subed-trim-overlap-stop)
					 (expect (subed-subtitle-msecs-stop) :to-equal 2000))))
      (it "handles the last subtitle gracefully."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:04,500\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100))
					 (subed-jump-to-subtitle-id 3)
					 (subed-trim-overlap-stop 500)
					 (expect (subed-subtitle-msecs-stop) :to-equal 6000))))
      (it "handles empty buffers gracefully."
        (with-temp-srt-buffer
				 (subed-trim-overlap-stop 500)
				 (expect (subed-subtitle-msecs-stop) :to-equal nil)))
			(describe "when adjusting to time boundaries"
				(it "adjusts the start time if the new stop would be before the start time."
					(with-temp-srt-buffer
					 (insert "1\n00:00:01,500 --> 00:00:02,000\nA\n\n"
									 "2\n00:00:01,000 --> 00:00:02,000\nA\n\n"
									 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
					 (let ((subed-subtitle-spacing 100)
								 (subed-enforce-time-boundaries 'adjust))
						 (subed-jump-to-subtitle-id 1)
						 (expect (subed-trim-overlap-stop) :to-equal 900)
						 (expect (subed-subtitle-msecs-stop 1) :to-equal 900)
						 (expect (subed-subtitle-msecs-start 1) :to-equal 900)))))
			(describe "when clipping to time boundaries"
				(it "adjusts the start time if the new stop would be before the start time."
					(with-temp-srt-buffer
					 (insert "1\n00:00:01,500 --> 00:00:02,000\nA\n\n"
									 "2\n00:00:01,000 --> 00:00:02,000\nA\n\n"
									 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
					 (let ((subed-subtitle-spacing 100)
								 (subed-enforce-time-boundaries 'clip))
						 (subed-jump-to-subtitle-id 1)
						 (expect (subed-trim-overlap-stop) :to-equal 1500)
						 (expect (subed-subtitle-msecs-stop 1) :to-equal 1500)
						 (expect (subed-subtitle-msecs-start 1) :to-equal 1500))))))
    (describe "overlap start time"
      (it "sets next start to the current timestamp plus spacing."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:04,500\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100))
					 (subed-jump-to-subtitle-id 2)
					 (subed-trim-overlap-next-start)
					 (subed-forward-subtitle-time-start)
					 (expect (subed-subtitle-msecs-start) :to-equal 4600))))
      (it "sets next start to the current timestamp plus the argument."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:04,500\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100))
					 (subed-jump-to-subtitle-id 2)
					 (subed-trim-overlap-next-start 500)
					 (subed-forward-subtitle-time-start)
					 (expect (subed-subtitle-msecs-start) :to-equal 5000))))
      (it "handles the last subtitle gracefully."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:04,500\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100))
					 (subed-jump-to-subtitle-id 3)
					 (subed-trim-overlap-next-start 500)
					 (expect (subed-subtitle-msecs-start) :to-equal 4000))))
      (it "adjusts the timestamp if the new start is past the stop time."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:01,500 --> 00:00:02,000\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100)
							 (subed-enforce-time-boundaries 'adjust))
					 (subed-jump-to-subtitle-id 1)
					 (expect (subed-trim-overlap-next-start 500) :to-equal 2500)
					 (expect (subed-subtitle-msecs-start 2) :to-equal 2500)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 2500))))
      (it "handles empty buffers gracefully."
        (with-temp-srt-buffer
				 (subed-trim-overlap-next-start 500)
				 (expect (subed-subtitle-msecs-stop) :to-equal nil))))
    (describe "trimming overlaps"
      (it "adjusts stop times by default."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:04,500\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n"
								 "4\n00:00:05,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100))
					 (subed-trim-overlaps))
				 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
				 (expect (subed-subtitle-msecs-stop 2) :to-equal 3900)
				 (expect (subed-subtitle-msecs-stop 3) :to-equal 4900)))
      (it "adjusts start times if specified."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:04,500\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n"
								 "4\n00:00:05,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100)
							 (subed-enforce-time-boundaries 'adjust)
							 (subed-trim-overlap-use-start t))
					 (subed-trim-overlaps)
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 4500)
					 (expect (subed-subtitle-msecs-start 3) :to-equal 4600)
					 (expect (subed-subtitle-msecs-start 4) :to-equal 6100))))
      (it "can specify the number of milliseconds."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
								 "2\n00:00:03,000 --> 00:00:04,500\nA\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA\n\n"
								 "4\n00:00:05,000 --> 00:00:06,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100))
					 (subed-trim-overlaps 200))
				 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
				 (expect (subed-subtitle-msecs-stop 2) :to-equal 3800)
				 (expect (subed-subtitle-msecs-stop 3) :to-equal 4800)))
      (it "handles empty buffers gracefully."
        (with-temp-srt-buffer
				 (expect (subed-trim-overlaps) :not :to-throw)))
      (it "handles single subtitles gracefully."
        (with-temp-srt-buffer
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n")
				 (let ((subed-subtitle-spacing 100))
					 (expect (subed-trim-overlaps) :not :to-throw))
				 (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
				 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000))))
    (describe "when configured to trim on save,"
      (it "trims overlaps after sorting."
        (with-temp-srt-buffer
				 (let ((subed-trim-overlap-on-save t)
							 (subed-subtitle-spacing 200))
					 (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
									 "2\n00:00:04,000 --> 00:00:06,000\nA\n\n"
									 "3\n00:00:03,000 --> 00:00:04,500\nA\n\n"
									 "4\n00:00:05,000 --> 00:00:06,000\nA\n\n")
					 (subed-prepare-to-save)
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 3800)
					 (expect (subed-subtitle-msecs-stop 3) :to-equal 4800)))))
		(describe "when configured to check on save,"
			(it "reports overlaps."
				(with-temp-srt-buffer
				 ;; Changed the test data to avoid sorting confusion
				 (insert "1\n00:00:01,000 --> 00:00:02,000\nA1\n\n"
								 "2\n00:00:03,000 --> 00:00:04,500\nA2\n\n"
								 "3\n00:00:04,000 --> 00:00:06,000\nA3\n\n"
								 "4\n00:00:05,000 --> 00:00:06,000\nA4\n\n")
				 (let ((subed-trim-overlap-check-on-save t)
							 (subed-trim-overlap-on-save nil)
							 (subed-subtitle-spacing 200)
							 (subed-enforce-time-boundaries 'adjust)
							 (buffer-modified-p nil))
					 (spy-on 'subed-trim-overlap-check :and-call-through)
					 (spy-on 'subed-trim-overlaps :and-call-through)
					 (spy-on 'yes-or-no-p :and-return-value t)
					 (subed-prepare-to-save)
					 (expect 'subed-trim-overlap-check :to-have-been-called)
					 (expect 'yes-or-no-p :to-have-been-called)
					 ;; Note changed behaviour: adjust the start time if needed,
					 ;; and don't change stop if there's enough space
					 (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
					 (expect (subed-subtitle-msecs-start 2) :to-equal 3000)
					 (expect (subed-subtitle-msecs-stop 2) :to-equal 3800)
					 (expect (subed-subtitle-msecs-stop 3) :to-equal 4800)))))
    (describe "when configured to check on load,"
      (it "reports overlaps."
        (with-temp-buffer
          (insert "1\n00:00:01,000 --> 00:00:02,000\nA\n\n"
                  "2\n00:00:04,000 --> 00:00:06,000\nA\n\n"
                  "3\n00:00:03,000 --> 00:00:04,500\nA\n\n"
                  "4\n00:00:05,000 --> 00:00:06,000\nA\n\n")
          (let ((subed-trim-overlap-check-on-load t)
                (subed-subtitle-spacing 200))
            (spy-on 'subed-trim-overlap-check :and-return-value nil)
            (subed-srt-mode)
            (expect subed--subtitle-format :to-equal "srt")
            (expect 'subed-trim-overlap-check :to-have-been-called))))))
  (describe "Getting a list of subtitles"
    (it "returns nil in an empty buffer."
      (with-temp-srt-buffer
			 (expect (subed-subtitle-list) :to-equal nil)))
    (it "returns the list."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (expect (subed-subtitle-list) :to-equal
							 '((1 61000 65123 "Foo." nil)
								 (2 122234 130345 "Bar." nil)
								 (3 183450 195500 "Baz." nil)))))
    (it "returns a subset when bounds are specified."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-jump-to-subtitle-id 3)
			 (backward-char 1)
			 (expect (subed-subtitle-list (point-min) (point))
							 :to-equal
							 '((1 61000 65123 "Foo." nil)
								 (2 122234 130345 "Bar." nil))))))
  (describe "Appending a list of subtitles"
    (it "adds them."
      (with-temp-srt-buffer
       (let ((list '((1 61000 65123 "Foo." nil)
									   (2 122234 130345 "Bar." nil)
									   (3 183450 195500 "Baz." nil))))
         (subed-append-subtitle-list list)
				 (expect (subed-subtitle-list)
                 :to-equal list)))))
  (describe "Getting the text of a list"
    (it "returns a blank string when given nothing."
      (expect (subed-subtitle-list-text nil) :to-equal ""))
    (it "returns the text of a list of subtitles."
      (expect
       (subed-subtitle-list-text
        '((nil 0 99 "Hello")
          (nil 100 199 "world" "Comment")))
       :to-equal "Hello\nworld\n"))
    (it "includes comments."
      (expect
       (subed-subtitle-list-text
        '((nil 0 99 "Hello")
          (nil 100 199 "world" "Comment"))
        t)
       :to-equal "Hello\n\nComment\n\nworld\n"))
    (it "includes comments transformed by a function."
      (let ((val
             (subed-subtitle-list-text
              '((nil 0 99 "Hello")
                (nil 100 199 "world" "Comment"))
              #'upcase)))
        (expect val :to-equal "Hello\n\nCOMMENT\n\nworld\n"))))
  (describe "Copying region text"
    (it "works on the whole buffer"
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-copy-region-text)
			 (expect (current-kill 0) :to-equal "Foo.\nBar.\nBaz.\n")))
    (it "works on a specified region."
      (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-copy-region-text (re-search-backward "Foo.") (re-search-forward "Bar."))
			 (expect (current-kill 0) :to-equal "Foo.\nBar.\n"))))
  (describe "Sorting"
    (it "detects sorted lists."
      (expect (subed--sorted-p '((1 1000 2000 "Test")
                                 (2 2000 3000 "Test")
                                 (3 3000 4000 "Test")))))
    (it "detects unsorted lists."
      (expect (subed--sorted-p '((1 3000 2000 "Test")
                                 (2 4000 3000 "Test")
                                 (3 1000 4000 "Test")))
              :to-be nil))
    (it "doesn't happen in an empty buffer."
      (with-temp-srt-buffer
       (spy-on 'sort-subr :and-call-through)
       (subed-sort)
       (expect 'sort-subr :not :to-have-been-called)))
    (describe "already-sorted subtitles"
      (it "doesn't rearrange subtitles."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (spy-on 'sort-subr :and-call-through)
         (subed-sort)
         (expect 'sort-subr :not :to-have-been-called)))
      (it "maintains the mark ring."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (let ((mark-ring))
           (push-mark 10 t nil)
           (push-mark 20 t nil)
           (push-mark 3 t nil)
           (expect (marker-position (car mark-ring)) :to-be 20)
           (expect (marker-position (cadr mark-ring)) :to-be 10)
           (subed-sort)
           (expect (marker-position (car mark-ring)) :to-be 20)
           (expect (marker-position (cadr mark-ring)) :to-be 10)))))
    (it "sorts subtitles by start time."
      (with-temp-srt-buffer
       (insert mock-srt-data "\n4\n00:02:01,000 --> 00:03:01,000\nNot sorted.\n")
       (expect (subed--sorted-p) :to-be nil)
       (goto-char (point-min))
       (subed-sort)
       (expect (subed-subtitle-text 2) :to-equal "Not sorted.")
       (expect (subed-subtitle-text 3) :to-equal "Bar.")
       (expect (subed-subtitle-text 4) :to-equal "Baz."))))

  (describe "An old generic function"
    :var ((function-list
           (list "subtitle-id" "subtitle-id-max" "subtitle-id-at-msecs"
                 "subtitle-msecs-start" "subtitle-msecs-stop"
                 "subtitle-text" "subtitle-relative-point"
                 "msecs-to-timestamp" "timestamp-to-msecs"
                 "jump-to-subtitle-id" "jump-to-subtitle-id-at-msecs"
                 "jump-to-subtitle-time-start" "jump-to-subtitle-time-stop"
                 "jump-to-subtitle-text" "jump-to-subtitle-text-at-msecs"
                 "jump-to-subtitle-end"
                 "forward-subtitle-id" "backward-subtitle-id"
                 "forward-subtitle-text" "backward-subtitle-text"
                 "forward-subtitle-end" "backward-subtitle-end"
                 "forward-subtitle-time-start" "backward-subtitle-time-start"
                 "forward-subtitle-time-stop" "backward-subtitle-time-stop"
                 "set-subtitle-time-start" "set-subtitle-time-stop"
                 "prepend-subtitle" "append-subtitle" "kill-subtitle" "merge-with-next"
                 "regenerate-ids" "regenerate-ids-soon"
                 "sanitize" "validate" "sort" "make-subtitle")))
    (it "is declared as a common function"
      (mapc (lambda (f)
              (let ((function-name (format "subed-%s" f)))
                (unless (functionp (intern function-name))
                  (buttercup-fail "%s is not a function" function-name))))
            function-list))
    (it "has format-specific internal functions"
      (mapc (lambda (f)
              (mapc (lambda (sub-format)
                      (let ((function-name (format "subed-%s--%s" sub-format f)))
                        (unless (functionp (intern function-name))
                          (buttercup-fail "%s is not a function" function-name))))
                    '("srt" "vtt" "ass")))
            function-list)))

  (describe "Setting subtitle"
    (describe "text"
      (it "replaces the text."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (re-search-backward "Foo")
         (subed-set-subtitle-text "Hello world")
         (expect (subed-subtitle-text) :to-equal "Hello world")))
      (it "replaces the text of a specified subtitle."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-set-subtitle-text "Hello world" 1)
         (expect (subed-subtitle-text) :to-equal "Hello world")
         (expect (subed-subtitle-id) :to-equal 1)))
      (it "blanks out subtitles."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-set-subtitle-text "" 1)
         (expect (subed-subtitle-text) :to-equal "")
         (expect (subed-subtitle-id) :to-equal 1)
         (expect (buffer-string) :to-equal
                 "1
00:01:01,000 --> 00:01:05,123


2
00:02:02,234 --> 00:02:10,345
Bar.

3
00:03:03,45 --> 00:03:15,5
Baz.
")))))

  (describe "Merging a region"
    (it "handles empty buffers."
      (with-temp-srt-buffer
       (subed-merge-region (point-min) (point-max))
       (expect (buffer-string) :to-equal "")))
    (it "merges all the subtitles if requested."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (expect (boundp 'subed--regenerate-ids-soon-timer) :to-be t)
       (subed-merge-region (point-min) (point-max))
       (expect (buffer-string) :to-equal "1
00:01:01,000 --> 00:03:15,500
Foo. Bar. Baz.
")))
    (it "merges some subtitles."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (re-search-backward "Bar")
       (subed-merge-region (point-min) (point))
       (expect (buffer-string) :to-equal "1
00:01:01,000 --> 00:02:10,345
Foo. Bar.

3
00:03:03,45 --> 00:03:15,5
Baz.
")))
    (it "merges some subtitles, including the last one."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (re-search-backward "Bar")
       (subed-merge-region (point) (point-max))
       (expect (buffer-string) :to-equal "1
00:01:01,000 --> 00:01:05,123
Foo.

2
00:02:02,234 --> 00:03:15,500
Bar. Baz.
"))))

  (describe "Merging a region and setting the text"
    (it "handles empty buffers."
      (with-temp-srt-buffer
       (subed-merge-region-and-set-text (point-min) (point-max) "")
       (expect (buffer-string) :to-equal "")))
    (it "merges all the subtitles if requested."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (expect (boundp 'subed--regenerate-ids-soon-timer) :to-be t)
       (subed-merge-region-and-set-text (point-min) (point-max) "Hello world")
       (expect (buffer-string) :to-equal "1
00:01:01,000 --> 00:03:15,500
Hello world
")))
    (it "merges some subtitles."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (re-search-backward "Bar")
       (subed-merge-region-and-set-text (point-min) (point) "Hello world")
       (expect (buffer-string) :to-equal "1
00:01:01,000 --> 00:02:10,345
Hello world

3
00:03:03,45 --> 00:03:15,5
Baz.
")))
    (it "merges some subtitles, including the last one."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (re-search-backward "Bar")
       (subed-merge-region-and-set-text (point) (point-max) "Hello world")
       (expect (buffer-string) :to-equal "1
00:01:01,000 --> 00:01:05,123
Foo.

2
00:02:02,234 --> 00:03:15,500
Hello world
"))))

  (describe "Conversion"
    (describe "from SRT"
      (describe "to VTT"
        (it "creates subtitles in the expected format"
          (with-temp-buffer
            (insert mock-srt-data)
            (subed-srt-mode)
            (with-current-buffer (subed-convert "VTT")
              (expect major-mode :to-equal 'subed-vtt-mode)
              (expect (buffer-string) :to-equal "WEBVTT


00:01:01.000 --> 00:01:05.123
Foo.

00:02:02.234 --> 00:02:10.345
Bar.

00:03:03.450 --> 00:03:15.500
Baz.
")))))
      ))

  (describe "Iterating over subtitles"
    (it "without providing beginning and end."
      (with-temp-srt-buffer
       (insert mock-srt-data)
       (subed-jump-to-subtitle-time-stop 1)
       (subed-for-each-subtitle nil nil nil
         (expect (looking-at "^[0-9]$") :to-be t)
         (forward-line 2)
         (kill-line)
         (insert "Hello."))
       (expect (subed-subtitle-text 1) :to-equal "Hello.")
       (expect (subed-subtitle-text 2) :to-equal "Bar.")
       (expect (subed-subtitle-text 3) :to-equal "Baz.")
       (expect (point) :to-equal 20)
       (subed-jump-to-subtitle-time-stop 2)
       (subed-for-each-subtitle nil nil nil
         (expect (looking-at "^[0-9]$") :to-be t)
         (forward-line 2)
         (kill-line)
         (insert "HEllo."))
       (expect (subed-subtitle-text 1) :to-equal "Hello.")
       (expect (subed-subtitle-text 2) :to-equal "HEllo.")
       (expect (subed-subtitle-text 3) :to-equal "Baz.")
       (expect (point) :to-equal 60)
       (subed-jump-to-subtitle-time-stop 3)
       (subed-for-each-subtitle nil nil nil
         (expect (looking-at "^[0-9]$") :to-be t)
         (forward-line 2)
         (kill-line)
         (insert "HELlo."))
       (expect (subed-subtitle-text 1) :to-equal "Hello.")
       (expect (subed-subtitle-text 2) :to-equal "HEllo.")
       (expect (subed-subtitle-text 3) :to-equal "HELlo.")
       (expect (point) :to-equal 99)))
    (describe "providing only the beginning"
      (it "forwards."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-time-start 1)
         (expect (point) :to-equal 3)
         (let ((new-texts (list "A" "B" "C")))
           (subed-for-each-subtitle 71 nil nil
             (expect (looking-at "^[0-9]$") :to-be t)
             (forward-line 2)
             (kill-line)
             (insert (pop new-texts))))
         (expect (subed-subtitle-text 1) :to-equal "Foo.")
         (expect (subed-subtitle-text 2) :to-equal "A")
         (expect (subed-subtitle-text 3) :to-equal "B")
         (expect (point) :to-equal 3)))
      (it "backwards."
        (with-temp-srt-buffer
         (insert mock-srt-data)
         (subed-jump-to-subtitle-time-stop 3)
         (expect (point) :to-equal 95)
         (let ((new-texts (list "A" "B" "C")))
           (subed-for-each-subtitle 75 nil :reverse
             (expect (looking-at "^[0-9]$") :to-be t)
             (forward-line 2)
             (kill-line)
             (insert (pop new-texts))))
         (expect (subed-subtitle-text 1) :to-equal "Foo.")
         (expect (subed-subtitle-text 2) :to-equal "B")
         (expect (subed-subtitle-text 3) :to-equal "A")
         (expect (point) :to-equal 92)))
      )
    (describe "providing beginning and end,"
      (describe "excluding subtitles above"
        (it "forwards."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-time-stop 1)
           (expect (point) :to-equal 20)
           (let ((new-texts (list "A" "B" "C")))
             (subed-for-each-subtitle 71 79 nil
               (expect (looking-at "^[0-9]$") :to-be t)
               (forward-line 2)
               (kill-line)
               (insert (pop new-texts))))
           (expect (subed-subtitle-text 1) :to-equal "Foo.")
           (expect (subed-subtitle-text 2) :to-equal "A")
           (expect (subed-subtitle-text 3) :to-equal "B")
           (expect (point) :to-equal 20)))
        (it "backwards."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-time-start 3)
           (expect (point) :to-equal 79)
           (let ((new-texts (list "A" "B" "C")))
             (subed-for-each-subtitle 39 77 :reverse
               (expect (looking-at "^[0-9]$") :to-be t)
               (forward-line 2)
               (kill-line)
               (insert (pop new-texts))))
           (expect (subed-subtitle-text 1) :to-equal "Foo.")
           (expect (subed-subtitle-text 2) :to-equal "B")
           (expect (subed-subtitle-text 3) :to-equal "A")
           (expect (point) :to-equal 76)))
        )
      (describe "excluding subtitles below"
        (it "forwards."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-text 3)
           (expect (point) :to-equal 106)
           (let ((new-texts (list "A" "B" "C")))
             (subed-for-each-subtitle 5 76 nil
               (expect (looking-at "^[0-9]$") :to-be t)
               (forward-line 2)
               (kill-line)
               (insert (pop new-texts))))
           (expect (subed-subtitle-text 1) :to-equal "A")
           (expect (subed-subtitle-text 2) :to-equal "B")
           (expect (subed-subtitle-text 3) :to-equal "Baz.")
           (expect (point) :to-equal 100)))
        (it "backwards."
          (with-temp-srt-buffer
           (insert mock-srt-data)
           (subed-jump-to-subtitle-time-stop 2)
           (expect (point) :to-equal 58)
           (let ((new-texts (list "A" "B" "C")))
             (subed-for-each-subtitle 20 76 :reverse
               (expect (looking-at "^[0-9]$") :to-be t)
               (forward-line 2)
               (kill-line)
               (insert (pop new-texts))))
           (expect (subed-subtitle-text 1) :to-equal "B")
           (expect (subed-subtitle-text 2) :to-equal "A")
           (expect (subed-subtitle-text 3) :to-equal "Baz.")
           (expect (point) :to-equal 55)))
        )
      )
    )

  (describe "Parsing files"
		(it "returns a list of subtitles."
			(let ((filename (make-temp-file "test" nil ".srt")))
				(unwind-protect
						(progn
							(with-temp-file filename
								(insert mock-srt-data))
							(let ((data (subed-parse-file filename)))
								(expect (length data) :to-equal 3)
								(expect (elt (elt data 0) 3) :to-equal "Foo.")
								(expect (elt (elt data 1) 3) :to-equal "Bar.")
								(expect (elt (elt data 2) 3) :to-equal "Baz.")))
					(delete-file filename))))
		(it "uses the specified mode function."
			(let ((filename (make-temp-file "test")))
				(unwind-protect
						(progn
							(with-temp-file filename
								(insert mock-srt-data))
							(let ((data (subed-parse-file filename 'subed-srt-mode)))
								(expect (length data) :to-equal 3)
								(expect (elt (elt data 0) 3) :to-equal "Foo.")
								(expect (elt (elt data 1) 3) :to-equal "Bar.")
								(expect (elt (elt data 2) 3) :to-equal "Baz.")))
					(delete-file filename))))
		(it "defaults to subed-tsv if unknown."
			(require 'subed-tsv)
			(let ((filename (make-temp-file "test")))
				(unwind-protect
						(progn
							(with-temp-file filename
								(insert "0.100000\t0.200000\tFoo.
0.500000\t0.700000\tBar.
0.800000\t1.000000\tBaz."))
							(let ((data (subed-parse-file filename)))
								(expect (length data) :to-equal 3)
								(expect (elt (elt data 0) 3) :to-equal "Foo.")
								(expect (elt (elt data 1) 3) :to-equal "Bar.")
								(expect (elt (elt data 2) 3) :to-equal "Baz.")))
					(delete-file filename)))))

  (describe "Copying region text"
	  (it "copies just the text for the whole buffer."
		  (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-copy-region-text)
			 (expect (current-kill 0) :to-equal "Foo.\nBar.\nBaz.\n")))
	  (it "copies the specified region."
		  (with-temp-srt-buffer
			 (insert mock-srt-data)
			 (subed-copy-region-text
			  (progn (subed-jump-to-subtitle-id 2) (point))
			  (progn (subed-jump-to-subtitle-end 3) (point)))
			 (expect (current-kill 0) :to-equal "Bar.\nBaz.\n"))))

  (describe "Guessing the format"
    (before-each
      (spy-on 'delete-process)
      (spy-on 'make-process)
      (spy-on 'make-network-process :and-return-value "mock client process"))
    (it "works when the generic functions is called."
      (let ((file (make-temp-file "subed-test" nil ".srt"))
            (auto-mode-alist '(("\\.srt\\'" . subed-mode))))
        (find-file file)
        (expect major-mode :to-equal 'subed-srt-mode)
        (subed-mpv-kill)
        (delete-file file)))
    (it "does not cause a loop when the more-specific function is called."
      (let ((file (make-temp-file "subed-test" nil ".srt"))
            (auto-mode-alist '(("\\.srt\\'" . subed-srt-mode))))
        (find-file file)
        (expect major-mode :to-equal 'subed-srt-mode)
        (subed-mpv-kill)
        (delete-file file))))

  (describe "Creating a subtitle that spans the file"
    (it "uses the file duration."
      (let* ((filename (make-temp-file "test-subed-a" nil ".opus"))
             (file (create-sample-media-file
                    :path filename
                    :duration-audio-stream 2)))
        (with-temp-srt-buffer
         (setq subed-mpv-media-file filename)
         (subed-insert-subtitle-for-whole-file)
         (let ((list (subed-subtitle-list)))
           (expect (length list) :to-equal 1)
           (expect (elt (car list) 1) :to-equal 0)
           (expect (elt (car list) 2) :to-be-weakly-greater-than 1900)
           (expect (elt (car list) 2) :to-be-weakly-less-than 2100))) ; some tolerance
        (delete-file filename))))

  (describe "Get duration in milliseconds of a file with a single audio stream"
    (let (;; `duration-audio-stream' is the duration in seconds for
          ;; the media file that is used inside the tests.  When
          ;; `duration-audio-stream' is an integer, ffprobe might
          ;; report a duration that is slightly greater, so we can't
          ;; expect the duration reported by ffprobe to be equal to
          ;; the duration that we passed to ffmpeg when creating the
          ;; sample media file. For this reason, we define the
          ;; variables `duration-lower-boundary' and
          ;; `duration-upper-boundary' to set a tolerance to the
          ;; reported value by ffprobe.
          ;;
          ;; When `duration-audio-stream' changes, the variables
          ;; `duration-lower-boundary' and
          ;; `duration-upper-boundary' should be set accordingly."
          (duration-audio-stream "3")
          (duration-lower-boundary 3000)
          (duration-upper-boundary 4000))
      (describe "audio file"
        (test-subed-extension ".wav")
        (test-subed-extension ".ogg")
        (test-subed-extension ".mp3")
        (test-subed-extension ".opus")
        (test-subed-extension ".m4a"))
      (describe "video format with just audio"
        (test-subed-extension ".mkv")
        (test-subed-extension ".mp4")
        (test-subed-extension ".webm")
        (test-subed-extension ".avi")
        (test-subed-extension ".ts")
        (test-subed-extension ".ogv"))))
  (describe "Get duration in milliseconds of a file with 1 video and 1 audio stream"
    ;; In this group of test cases, we want the duration of the audio
    ;; stream to be shorter than the duration of the video stream, so
    ;; that we can make sure that subed-waveform-ffprobe-duration-ms
    ;; specifically gets the duration of the audio stream.
    (test-subed-extension ".mkv" t)
    (test-subed-extension ".mp4" t)
    (test-subed-extension ".webm" t)
    (test-subed-extension ".avi" t)
    (test-subed-extension ".ts" t)
    (test-subed-extension ".ogv" t))
  )
