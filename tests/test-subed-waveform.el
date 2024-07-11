;; -*- lexical-binding: t; eval: (buttercup-minor-mode) -*-

(require 'subed-waveform)

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
            (duration-ms (subed-waveform-ffprobe-duration-ms filename)))
       (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
       (expect duration-ms :to-be-less-than duration-upper-boundary)
       (delete-file filename))))

(describe "waveform"
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
    (test-subed-extension ".ogv" t)))
