# skewer-mode

Provides live interaction with JavaScript in a browser. Expressions
can be sent on-the-fly from a buffer to be evaluated in the browser,
just like Emacs does with an inferior Lisp process in Lisp modes.

Dependencies:

 * [simple-httpd][simple-httpd] (available on [MELPA][melpa])
 * [js2-mode][js2-mode] (available on ELPA)

## Usage

### Quick version

 1. Place dependencies in your `load-path` or load them directly
 2. Load `skewer-mode.el`
 3. `M-x run-skewer` to attach a browser to Emacs
 4. From a `js2-mode` buffer, send forms to the browser to evaluate

The keybindings for evaluating expressions in the browser are just
like the Lisp modes. These are provided by the minor mode
`skewer-mode`.

 * C-x C-e -- `skewer-eval-last-expression`
 * C-M-x   -- `skewer-eval-defun`
 * C-c C-k -- `skewer-load-buffer`

The result of the expression is echoed in the minibuffer.

Note: `run-skewer` uses `browse-url` to launch the browser. This may
require further setup depending on your operating system and personal
preferences.

### Manual version

To skewer your own document rather than the provided blank one,

 1. Load the dependencies
 2. Load `skewer-mode.el`
 3. Start the HTTP server (`httpd-start`)
 4. Put your HTML document under the root (`httpd-root`)
 5. Include jQuery and `/skewer` as scripts (see `example.html`)
 6. Visit the document from your browser

If your document isn't a static page but is instead hosted by its own
server, you can still skewer the page. See the proxy below.

## REPL

With `skewer-repl.el` loaded, a REPL into the browser can be created
with `M-x skewer-repl`. This should work just like a REPL in console
within the browser.

## Transparent Proxy

To work around the same origin policy, skewer can also be a proxy for
another site, where it will automatically inject it's own HTML. This
is experimental and a bit flaky right now. See `skewer-proxy.el`.

## Rationalization

I wanted something like [swank-js][swank-js] but without all the
painful setup. Having already written an Emacs web server I was
halfway there. It took relatively little code to accomplish.

The name refers to the idea that Emacs is *skewering* the browser from
server-side.

[simple-httpd]: https://github.com/skeeto/emacs-http-server
[js2-mode]: http://code.google.com/p/js2-mode/
[melpa]: http://melpa.milkbox.net/
[swank-js]: https://github.com/swank-js/swank-js
