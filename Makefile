# Thin wrapper around Eask.  TETHER_DEPS selects the dependency set:
# latest (MELPA snapshots, default) or min (oldest supported releases).
# See SPEC.md §2.2.

EASK ?= eask
EMACS ?= emacs
TETHER_DEPS ?= latest
export TETHER_DEPS

.PHONY: all deps compile lint package-lint checkdoc test surface clean

all: compile lint surface test

# Dependencies of the other set are removed first; both cannot share .eask.
deps:
	@if [ "$$(cat .eask/deps-set 2>/dev/null)" != "$(TETHER_DEPS)" ]; then \
	  $(EASK) clean .eask; mkdir -p .eask; \
	fi
	$(EASK) install-deps --dev
	@echo $(TETHER_DEPS) > .eask/deps-set

# Warnings are errors for tether's code only.  (`eask compile --strict'
# would also fail on Eask's metadata warnings about the extra
# dependencies of the min set.)
compile:
	$(EASK) emacs --batch -L . --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile tether*.el
	rm -f *.elc

lint: package-lint checkdoc

package-lint:
	$(EASK) lint package

checkdoc:
	$(EASK) lint checkdoc

test:
	$(EASK) test ert test/*-test.el

# Needs no dependency: tether-deps.el holds the allowlist.
surface:
	$(EMACS) -Q --batch -L . -l scripts/check-surface.el

clean:
	$(EASK) clean all
