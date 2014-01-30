EMACS   ?= emacs
BATCH   := $(EMACS) -batch -Q -L .
COMPILE := $(BATCH) -f batch-byte-compile
VERSION := $(word 1,$(subst -, ,$(shell git describe)))

EL  = skewer-mode.el
EL += skewer-setup.el
EL += cache-table.el
EL += skewer-bower.el
EL += skewer-css.el
EL += skewer-html.el
EL += skewer-repl.el
ELC = $(EL:.el=.elc)
PKG = skewer-mode-pkg.el

DIST_FILES = $(PKG) $(EL) skewer.js example.html README.md UNLICENSE

.PHONY : all package compile clean

all : package

skewer-mode-$(VERSION).tar : $(DIST_FILES)
	tar -cf $@ --transform "s,^,skewer-mode-$(VERSION)/," $^

package: skewer-mode-$(VERSION).tar

compile: $(ELC)

clean:
	$(RM) *.tar $(ELC)

%.elc: %.el
	$(COMPILE) $<
