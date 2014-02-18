EMACS   ?= emacs
CASK    ?= cask
VIRTUAL := $(CASK) exec $(EMACS)
BATCH   := $(VIRTUAL) -batch -Q -L .

PACKAGE := skewer-mode
VERSION := $(shell $(CASK) version)

EL  = skewer-mode.el
EL  = skewer-bosh.el
EL  = skewer-websocket.el
EL += skewer-setup.el
EL += cache-table.el
EL += skewer-bower.el
EL += skewer-css.el
EL += skewer-html.el
EL += skewer-repl.el
ELC = $(EL:.el=.elc)

DIST_FILES = $(EL) skewer.js example.html README.md

.PHONY : all package compile clean

all : compile

.cask : Cask
	cask install
	touch .cask

$(PACKAGE)-$(VERSION).tar : $(DIST_FILES)
	tar -cf $@ --transform "s,^,$(PACKAGE)-$(VERSION)/," $^

package: $(PACKAGE)-$(VERSION).tar

compile: .cask $(ELC)

clean:
	$(RM) *.tar $(ELC)

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<
