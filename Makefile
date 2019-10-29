# Clone the dependencies of this package in sibling directories:
#   $ git clone https://github.com/mooz/js2-mode ../js2-mode
#   $ git clone https://github.com/skeeto/emacs-web-server ../simple-httpd
#
# Or set LDFLAGS to point at these packages elsewhere:
#     $ make LDFLAGS='-L path/to/js2-mode -L path/to/simple-httpd'
.POSIX:
.SUFFIXES: .el .elc

EMACS   = emacs
LDFLAGS = -L ../simple-httpd -L ../js2-mode
BATCH   = $(EMACS) -Q -batch -L . $(LDFLAGS)
COMPILE = $(BATCH) -f batch-byte-compile
VERSION = 1.8.0

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

run: compile
	$(EMACS) -Q -L . $(LDFLAGS) \
		 -l skewer-mode.elc -l skewer-setup.elc -f skewer-setup \
		 --eval "(setf initial-scratch-message nil)" \
		 -f js2-mode -f run-skewer

clean:
	rm -rf skewer-mode-$(VERSION) skewer-mode-$(VERSION).tar $(ELC)

.el.elc:
	$(COMPILE) $<
