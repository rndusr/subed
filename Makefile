# SPDX-FileCopyrightText: 2019-2020 The subed Authors
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Putting compile before test results in an invalid-function error about subed-for-each-subtitle
all: clean autoloads test compile

clean:
	find . -name "*.elc" -delete
	rm -f subed/subed-autoloads.el
	rm -f coverage/.*.json

test: autoloads test-coverage package-lint checkdoc

test-coverage:
	mkdir -p coverage
	UNDERCOVER_FORCE=true emacs -Q -batch -L . -f package-initialize -f buttercup-run-discover

test-some:
	emacs -Q -batch -L . -f package-initialize -f buttercup-run-discover --pattern "${PATTERN}" --no-skip

test-only:
	emacs -Q -batch -f package-initialize -L . -f buttercup-run-discover

package-lint:
	emacs --no-init-file -f package-initialize --batch \
		  --eval "(require 'package-lint)" \
	      --file package-lint-batch-and-exit \
	      ./subed/subed.el
checkdoc:
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed.el\")"
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed-config.el\")"
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed-mpv.el\")"
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed-srt.el\")"
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed-vtt.el\")"
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed-ass.el\")"

autoloads:
	emacs --quick --batch --eval "(progn (setq generated-autoload-file (expand-file-name \"subed-autoloads.el\" \"subed\") backup-inhibited t) \
	(update-directory-autoloads \"./subed\"))"

compile:
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-config.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-common.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-mpv.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-srt.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-vtt.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-ass.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-debug.el\"))"

test-compile: compile clean

test-emacs:
	emacs -Q -L ./subed --eval "(require 'subed-autoloads)"

watch:
	nodemon -w subed -w tests -e el --exec make test
