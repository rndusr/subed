;; -*- lexical-binding: t; eval: (buttercup-minor-mode) -*-

(load-file "./tests/undercover-init.el")

(require 'subed-word-data)

(describe "subed-word-data"
  (describe "Finding approximate matches"
    (it "handles early oopses."
      (let ((result (subed-word-data-find-approximate-match
                     "This is a test"
                     (split-string
                      "This is oops This is a test."
                      " ")
                     "\\<oops\\>")))
        (expect (car result) :to-be-greater-than 0.01)
        (expect (string-join (cdr result) " ") :to-equal "This is oops")))
    (it "handles early oopses in a longer phrase."
      (let ((result (subed-word-data-find-approximate-match
                     "The quick brown fox jumps over the lazy dog"
                     (split-string
                      "The quick, oops, the quick brown fox jumps over the lazy dog and goes all sorts of places"
                      " ")
                     "\\<oops\\>")))
        (expect (car result) :to-be-greater-than 0.01)
        (expect (string-join (cdr result) " ") :to-equal "The quick, oops,")))
    (it "handles misrecognized words."
      (let ((result (subed-word-data-find-approximate-match
                     "Emacs is a text editor."
                     (split-string
                      "Emax is a text editor. More stuff goes here."
                      " "))))
        (expect (car result) :to-be-greater-than 0)
        (expect (string-join (cdr result) " ") :to-equal "Emax is a text editor.")))
    (it "handles split up words."
      (let ((result (subed-word-data-find-approximate-match
                     "Go in to the room."
                     (split-string
                      "Go into the room, more stuff goes here."
                      " "))))
        (expect (car result) :to-be-greater-than 0)
        (expect (string-join (cdr result) " ") :to-equal "Go into the room,")))
    (it "handles exact matches."
      (let ((result (subed-word-data-find-approximate-match
                     "We're lucky if we can get an exact match."
                     (split-string
                      "We're lucky if we can get an exact match. More stuff goes here."
                      " "))))
        (expect (car result) :to-be-less-than 0.001)
        (expect (string-join (cdr result) " ") :to-equal "We're lucky if we can get an exact match.")))

)

  )
