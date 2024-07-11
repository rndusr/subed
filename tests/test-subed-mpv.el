;; -*- lexical-binding: t; eval: (buttercup-minor-mode) -*-

(load-file "./tests/undercover-init.el")
(require 'subed-mpv)

(describe "Starting mpv"
  (it "passes arguments to make-process."
    (spy-on 'make-process)
    (spy-on 'subed-mpv--socket :and-return-value "/mock/path/to/socket")
    (subed-mpv--server-start "foo" "--bar")
    (expect 'make-process :to-have-been-called-with
            :command (list subed-mpv-executable
                           "--input-ipc-server=/mock/path/to/socket"
                           "--idle" "foo" "--bar")
            :name "subed-mpv-server" :buffer nil :noquery t))
  (it "sets subed-mpv--server-proc on success."
    (spy-on 'make-process :and-return-value "mock process")
    (subed-mpv--server-start)
    (expect subed-mpv--server-proc :to-equal "mock process"))
  (it "signals error on failure."
    (spy-on 'make-process :and-throw-error 'error)
    (expect (subed-mpv--server-start) :to-throw 'error))
  )

(describe "Stopping mpv"
  (before-each
    (setq subed-mpv--server-proc "mock running mpv process")
    (spy-on 'process-live-p :and-return-value t)
    (spy-on 'delete-process))
  (it "kills the mpv process."
    (subed-mpv--server-stop)
    (expect 'delete-process :to-have-been-called-with "mock running mpv process"))
  (it "resets subed-mpv--server-proc."
    (expect subed-mpv--server-proc :not :to-be nil)
    (subed-mpv--server-stop)
    (expect subed-mpv--server-proc :to-be nil))
  )

(describe "Connecting"
  (before-each
    (spy-on 'delete-process))
  (it "resets global status variables."
    (spy-on 'subed-mpv--client-connected-p :and-return-value t)
    (spy-on 'make-network-process :and-return-value "mock client process")
    (spy-on 'process-send-string)
    (spy-on 'subed-mpv--client-send)
    (setq subed-mpv--client-proc "foo"
          subed-mpv-is-playing "baz"
          subed-mpv--client-command-queue '(foo bar baz))
    (subed-mpv--client-connect '(0 0 0))
    (expect subed-mpv--client-proc :to-equal "mock client process")
    (expect subed-mpv-is-playing :to-be nil)
    (expect subed-mpv--client-command-queue :to-be nil))
  (it "correctly calls make-network-process."
    (spy-on 'make-network-process)
    (spy-on 'process-send-string)
    (spy-on 'subed-mpv--socket :and-return-value "/mock/path/to/socket")
    (subed-mpv--client-connect '(0 0 0))
    (expect 'make-network-process :to-have-been-called-with
            :name "subed-mpv-client"
            :family 'local
            :service (subed-mpv--socket)
            :coding '(utf-8 . utf-8)
			:buffer (subed-mpv--client-buffer)
            :filter #'subed-mpv--client-filter
            :noquery t
            :nowait t))
  (describe "tests the connection"
    (it "and sets subed-mpv--client-proc if the test succeeds."
      (spy-on 'make-network-process :and-return-value "mock client process")
      (spy-on 'process-send-string)
      (subed-mpv--client-connect '(0 0 0))
      (expect 'process-send-string :to-have-been-called-with
              "mock client process" (concat subed-mpv--client-test-request "\n"))
      (expect subed-mpv--client-proc :to-equal "mock client process"))
    (it "and resets subed-mpv--client-proc if the test fails."
      (spy-on 'make-network-process :and-return-value "mock client process")
      (spy-on 'process-send-string :and-throw-error 'error)
      (setq subed-mpv--client-proc "foo")
      (subed-mpv--client-connect '(0 0 0))
      (expect subed-mpv--client-proc :to-be nil))
    (it "and tries again if the test fails."
      (spy-on 'make-network-process :and-return-value "mock client process")
      (spy-on 'process-send-string :and-throw-error 'error)
      (subed-mpv--client-connect '(0 0 0))
      ;; FIXME: This seems to be a bug:
      ;; https://github.com/jorgenschaefer/emacs-buttercup/issues/139
      ;; (expect 'process-send-string :to-have-been-called-times 3)
      (expect subed-mpv--client-proc :to-be nil))
    )
  (it "sends queued commands and empties the queue."
    (spy-on 'make-network-process :and-return-value "mock client process")
    (spy-on 'process-send-string)
    (spy-on 'subed-mpv--client-send)
    (setq subed-mpv--client-command-queue '(foo bar baz))
    (subed-mpv--client-connect '(0 0 0))
    (expect 'subed-mpv--client-send :to-have-been-called-with 'foo)
    (expect 'subed-mpv--client-send :to-have-been-called-with 'bar)
    (expect 'subed-mpv--client-send :to-have-been-called-with 'baz)
    (expect subed-mpv--client-command-queue :to-be nil))
  )

(describe "Sending command"
  (before-each
    (spy-on 'delete-process)
    (setq subed-mpv--client-command-queue nil))
  (describe "when mpv process is not running"
    (before-each
      (spy-on 'subed-mpv--server-started-p :and-return-value nil))
    (it "is not queued if not connected."
      (spy-on 'subed-mpv--client-connected-p :and-return-value nil)
      (subed-mpv--client-send '(do this thing))
      (expect subed-mpv--client-command-queue :to-be nil))
    )
  (describe "when mpv process is running"
    (before-each
      (spy-on 'subed-mpv--server-started-p :and-return-value t))
    (it "is queued if not connected."
      (spy-on 'subed-mpv--client-connected-p :and-return-value nil)
      (subed-mpv--client-send '(do this thing))
      (expect subed-mpv--client-command-queue :to-equal '((do this thing)))
      (subed-mpv--client-send '(do something else))
      (expect subed-mpv--client-command-queue :to-equal '((do this thing)
                                                          (do something else))))
    (it "sends command if connected."
      (spy-on 'subed-mpv--client-connected-p :and-return-value t)
      (spy-on 'process-send-string)
      (setq subed-mpv--client-proc "mock client process")
      (subed-mpv--client-send '(do this thing))
      (expect 'process-send-string :to-have-been-called-with
              "mock client process"
              (concat (json-encode (list :command '(do this thing))) "\n"))
      (expect subed-mpv--client-command-queue :to-equal nil))
    (it "disconnects if sending fails even though we're connected."
      (spy-on 'subed-mpv--client-connected-p :and-return-value t)
      (spy-on 'subed-mpv--client-disconnect)
      (spy-on 'process-send-string :and-throw-error 'error)
      (expect (subed-mpv--client-send '(do this thing)) :to-throw 'error)
      (expect 'subed-mpv--client-disconnect :to-have-been-called-times 1)
      (expect subed-mpv--client-command-queue :to-equal nil))
    )
  )

