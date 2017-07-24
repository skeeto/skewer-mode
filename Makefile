## Use LDFLAGS to add (-L) simple-httpd and js2-mode to the load-path

.POSIX:
.SUFFIXES: .el .elc

EMACS   = emacs
LDFLAGS = -L ../simple-httpd -L ../js2-mode
BATCH   = $(EMACS) -Q -batch -L . $(LDFLAGS)
COMPILE = $(BATCH) -f batch-byte-compile
VERSION = 1.7.0

EL = skewer-mode.el skewer-setup.el cache-table.el \
     skewer-bower.el skewer-css.el skewer-html.el skewer-repl.el
ELC = $(EL:.el=.elc)
PKG = skewer-mode-pkg.el
DIST = $(PKG) $(EL) skewer.js example.html README.md UNLICENSE

compile: $(ELC)
all: compile package
package: skewer-mode-$(VERSION).tar

skewer-mode-$(VERSION): $(DIST)
	mkdir -p $@
	cp $(DIST) $@/
	touch $@/

skewer-mode-$(VERSION).tar: skewer-mode-$(VERSION)
	tar cf $@ skewer-mode-$(VERSION)/

compile: $(ELC)

clean:
	rm -rf skewer-mode-$(VERSION) skewer-mode-$(VERSION).tar $(ELC)

.el.elc:
	$(COMPILE) $<
