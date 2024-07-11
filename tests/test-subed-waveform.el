;; -*- eval: (buttercup-minor-mode); lexical-binding: t -*-

(require 'subed-waveform)

(cl-defun create-sample-media-file-1-audio-stream (&key
                                                   path
                                                   duration-audio-stream)
  "Create a sample media file with one audio stream

PATH is the absolute path for the output file. It must be a
string.

DURATION is the number of seconds for the media file. It must be
a string."
  (call-process
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
   ;; We use lavfi to create the audio stream
   "-f" "lavfi" "-i" (concat "sine=frequency=1000:duration=" duration-audio-stream)
   path))

(cl-defun create-sample-media-file-1-audio-stream-1-video-stream (&key
                                                                  path
                                                                  duration-video-stream
                                                                  duration-audio-stream)
  "Create a sample media file with 1 audio stream and 1 video stream.

PATH is the absolute path for the output file. It must be a
string.

AUDIO-DURATION is the duration in seconds for the audio
stream. It must be a string.

VIDEO-DURATION is the duration in seconds for the video stream. It
must be a string."
  (call-process
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
   ;; Create the video stream
   "-f" "lavfi" "-i" (concat "testsrc=size=100x100:duration=" duration-video-stream)
   ;; Create the audio stream
   "-f" "lavfi" "-i" (concat "sine=frequency=1000:duration=" duration-audio-stream)
   path))

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
        (it "extension .wav"
          (create-sample-media-file-1-audio-stream
           :path "/tmp/a.wav"
           :duration-audio-stream duration-audio-stream)
          (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.wav")))
            (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
            (expect duration-ms :to-be-less-than duration-upper-boundary)))
        (it "extension .ogg"
          (create-sample-media-file-1-audio-stream
           :path "/tmp/a.ogg"
           :duration-audio-stream duration-audio-stream)
          (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.ogg")))
            (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
            (expect duration-ms :to-be-less-than duration-upper-boundary)))
        (it "extension .mp3"
          (create-sample-media-file-1-audio-stream
           :path "/tmp/a.mp3"
           :duration-audio-stream duration-audio-stream)
          (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.mp3")))
            (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
            (expect duration-ms :to-be-less-than duration-upper-boundary)))
        (it "extension .opus"
          (create-sample-media-file-1-audio-stream
           :path "/tmp/a.opus"
           :duration-audio-stream duration-audio-stream)
          (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.opus")))
            (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
            (expect duration-ms :to-be-less-than duration-upper-boundary)))
        (it "extension .m4a"
          (create-sample-media-file-1-audio-stream
           :path "/tmp/a.m4a"
           :duration-audio-stream duration-audio-stream)
          (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.m4a")))
            (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
            (expect duration-ms :to-be-less-than duration-upper-boundary))))
      (describe "video file"
        (it "extension .mkv"
          (create-sample-media-file-1-audio-stream
           :path "/tmp/a.mkv"
           :duration-audio-stream duration-audio-stream)
          (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.mkv")))
            (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
            (expect duration-ms :to-be-less-than duration-upper-boundary)))
        (it "extension .mp4"
          (create-sample-media-file-1-audio-stream
           :path "/tmp/a.mp4"
           :duration-audio-stream duration-audio-stream)
          (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.mp4")))
            (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
            (expect duration-ms :to-be-less-than duration-upper-boundary)))
        (it "extension .webm"
          (create-sample-media-file-1-audio-stream
           :path "/tmp/a.webm"
           :duration-audio-stream duration-audio-stream)
          (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.webm")))
            (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
            (expect duration-ms :to-be-less-than duration-upper-boundary)))
        (it "extension .avi"
          (create-sample-media-file-1-audio-stream
           :path "/tmp/a.avi"
           :duration-audio-stream duration-audio-stream)
          (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.avi")))
            (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
            (expect duration-ms :to-be-less-than duration-upper-boundary)))
        (it "extension .ts"
          (create-sample-media-file-1-audio-stream
           :path "/tmp/a.ts"
           :duration-audio-stream duration-audio-stream)
          (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.ts")))
            (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
            (expect duration-ms :to-be-less-than duration-upper-boundary)))
        (it "extension .ogv"
          (create-sample-media-file-1-audio-stream
           :path "/tmp/a.ogv"
           :duration-audio-stream duration-audio-stream)
          (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.ogv")))
            (expect duration-ms :to-be-weakly-greater-than duration-lower-boundary)
            (expect duration-ms :to-be-less-than duration-upper-boundary))))))
  (describe "Get duration in milliseconds of a file with 1 video and 1 audio stream"
    ;; In this group of test cases, we want the duration of the audio
    ;; stream to be shorter than the duration of the video stream, so
    ;; that we can make sure that subed-waveform-ffprobe-duration-ms
    ;; specifically gets the duration of the audio stream.
    (let ((duration-video-stream "5")
          (duration-audio-stream "3")
          (duration-audio-stream-lower-boundary 3000)
          (duration-audio-stream-upper-boundary 4000))
      (it "extension .mkv"
        (create-sample-media-file-1-audio-stream-1-video-stream
         :path "/tmp/a.mkv"
         :duration-video-stream duration-video-stream
         :duration-audio-stream duration-audio-stream)
        (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.mkv")))
          (expect duration-ms :to-be-weakly-greater-than duration-audio-stream-lower-boundary)
          (expect duration-ms :to-be-less-than duration-audio-stream-upper-boundary)))
      (it "extension .mp4"
        (create-sample-media-file-1-audio-stream-1-video-stream
         :path "/tmp/a.mp4"
         :duration-video-stream duration-video-stream
         :duration-audio-stream duration-audio-stream)
        (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.mp4")))
          (expect duration-ms :to-be-weakly-greater-than duration-audio-stream-lower-boundary)
          (expect duration-ms :to-be-less-than duration-audio-stream-upper-boundary)))
      (it "extension .webm"
        (create-sample-media-file-1-audio-stream-1-video-stream
         :path "/tmp/a.webm"
         :duration-video-stream duration-video-stream
         :duration-audio-stream duration-audio-stream)
        (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.webm")))
          (expect duration-ms :to-be-weakly-greater-than duration-audio-stream-lower-boundary)
          (expect duration-ms :to-be-less-than duration-audio-stream-upper-boundary)))
      (it "extension .avi"
        (create-sample-media-file-1-audio-stream-1-video-stream
         :path "/tmp/a.avi"
         :duration-video-stream duration-video-stream
         :duration-audio-stream duration-audio-stream)
        (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.avi")))
          (expect duration-ms :to-be-weakly-greater-than duration-audio-stream-lower-boundary)
          (expect duration-ms :to-be-less-than duration-audio-stream-upper-boundary)))
      (it "extension .ts"
        (create-sample-media-file-1-audio-stream-1-video-stream
         :path "/tmp/a.ts"
         :duration-video-stream duration-video-stream
         :duration-audio-stream duration-audio-stream)
        (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.ts")))
          (expect duration-ms :to-be-weakly-greater-than duration-audio-stream-lower-boundary)
          (expect duration-ms :to-be-less-than duration-audio-stream-upper-boundary)))
      (it "extension .ogv"
        (create-sample-media-file-1-audio-stream-1-video-stream
         :path "/tmp/a.ogv"
         :duration-video-stream duration-video-stream
         :duration-audio-stream duration-audio-stream)
        (let ((duration-ms (subed-waveform-ffprobe-duration-ms "/tmp/a.ogv")))
          (expect duration-ms :to-be-weakly-greater-than duration-audio-stream-lower-boundary)
          (expect duration-ms :to-be-less-than duration-audio-stream-upper-boundary))))))
