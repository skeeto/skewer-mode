# Skewer -- live Emacs JavaScript interaction

Provides live interaction with JavaScript running in a browser.
Expressions are sent on-the-fly from an editing buffer to be evaluated
in the browser, just like Emacs does with an inferior Lisp process in
Lisp modes.

* Watch the [**demo video** on YouTube](http://youtu.be/4tyTgyzUJqM)
  ([webm](http://nullprogram.s3.amazonaws.com/skewer/demo.webm))

**Skewer is available from [MELPA][melpa]**, which will install the
dependencies for you. This package and its dependencies are pure
Elisp, meaning setup is a breeze and the whole thing is highly
portable.

Dependencies:

 * [simple-httpd][simple-httpd] (available on MELPA)
 * [js2-mode][js2-mode] (available on ELPA)

## Usage

### Quick version

If Skewer was installed from MELPA, skip to step 3.

 1. Place dependencies in your `load-path` or load them directly
 2. Load `skewer-mode.el`
 3. `M-x run-skewer` to attach a browser to Emacs
 4. From a `js2-mode` buffer, send forms to the browser to evaluate

The keybindings for evaluating expressions in the browser are just
like the Lisp modes. These are provided by the minor mode
`skewer-mode`.

 * <kbd>C-x C-e</kbd>: Evaluate the form before the point and display
   the result in the minibuffer. If given a prefix argument, insert
   the result into the current buffer.
 * <kbd>C-M-x</kbd>:   Evaluate the top-level form around the point.
 * <kbd>C-c C-k</kbd>: Load the current buffer.
 * <kbd>C-c C-z</kbd>: Select the REPL buffer.

The result of the expression is echoed in the minibuffer.

Note: `run-skewer` uses `browse-url` to launch the browser. This may
require further setup depending on your operating system and personal
preferences.

Multiple browsers and browser tabs can be attached to Emacs at once.
JavaScript forms are sent to all attached clients simultaneously, and
each will echo back the result individually. Use `list-skewer-clients`
to see a list of all currently attached clients.

Sometimes Skewer's long polls from the browser will timeout after a
number of hours of inactivity. If you find the browser disconnected
from Emacs for any reason, use the browser's console to call
`skewer()` to reconnect. This avoids a page reload, which would lose
any fragile browser state you might care about.

### Manual version

To Skewer your own document rather than the provided blank one,

 1. Load the dependencies
 2. Load `skewer-mode.el`
 3. Start the HTTP server (`httpd-start`)
 4. Put your HTML document under the root (`httpd-root`)
 5. Include jQuery and `/skewer` as scripts (see `example.html`)
 6. Visit the document from your browser

If your document isn't a static page but is instead hosted by its own
server, you can still Skewer the page. See the proxy below.

### Browser Support

Skewer is known to work properly with Firefox, Chrome, Safari, Opera,
and Internet Explorer 9.

## REPL

A REPL into the browser can be created with `M-x skewer-repl`. This
should work just like a console within the browser. Messages can be
logged to this REPL with `skewer.log()` (just like `console.log()`).

Results of expressions evaluated in the REPL are printed more
verbosely than in the minibuffer, when possible. This may help in
debugging.

Use `M-x skewer-repl-toggle-strict-mode` to toggle strict evaluation
for expressions in the REPL. However, be aware of the
[consequences of using strict mode][strict-mode].

## Strict mode

Evaluation can be done in [strict mode][strict-mode] but,
unfortunately, because strict mode `eval` is neutered the results are
completely at odds with Skewer. It's not possible to create new global
bindings in strict mode, so functions and variables defined in strict
mode evaluations can't be accessed by Skewer again later. If you want
to redefine loaded code in Skewer, make sure you disable strict mode.

However, you *can* use strict *within* your functions since this
doesn't effect Skewer's top-level `eval`.

## Transparent Proxy

To work around the same origin policy, Skewer can also be a proxy for
another site, where it will automatically inject it's own HTML. This
is experimental and a bit flaky right now. See `skewer-proxy.el`.

[CORS][cors] is a *much* better alternative if you can use it. It's
supported by Skewer (in theory).

## Motivation

I wanted something like [swank-js][swank-js] but without all the
painful setup. Having already written an Emacs web server I was
halfway there. It took relatively little code to accomplish.

I also didn't want to rely a browser-specific feature, like MozRepl or
WebKit's remote debugger ([kite][kite]).

The name refers to the idea that Emacs is *skewering* the browser from
server-side.

[simple-httpd]: https://github.com/skeeto/emacs-http-server
[js2-mode]: http://code.google.com/p/js2-mode/
[melpa]: http://melpa.milkbox.net/
[swank-js]: https://github.com/swank-js/swank-js
[strict-mode]: https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Functions_and_function_scope/Strict_mode
[cors]: http://en.wikipedia.org/wiki/Cross-origin_resource_sharing
[kite]: https://github.com/jscheid/kite
