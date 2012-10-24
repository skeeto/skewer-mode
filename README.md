# skewer-mode

It's like SLIME and Swank for JavaScript in the browser.

Dependencies:

 * [simple-httpd][simple-httpd] (available on [MELPA][melpa])
 * [js2-mode][js2-mode] (available on ELPA)

## Usage

 1. Start the HTTP server (`httpd-start`)
 2. Put your HTML document in the root (`httpd-root`)
 3. Include jQuery and `/skewer` as scripts (see `example.html`)
 4. Visit the document from a browser

As a shortcut, the above can mostly be done with `M-x run-skewer`. A
browser will be launched and connected to Emacs for use as a
JavaScript REPL.

With `skewer-mode` enabled in the buffer, these commands will evaluate
the JavaScript expression around the point, like the various Lisp
modes.

 * C-x C-e   --  `skewer-eval-last-expression`
 * C-M-x     --  `skewer-eval-defun`
 * C-c C-k   --  `skewer-load-buffer`

The result of the expression is echoed in the minibuffer.

With `skewer-mode` enabled in a buffer, typing `C-x C-e`
(`skewer-eval-last-expression`) or `C-M-x` (`skewer-eval-defun`) will
evaluate the JavaScript expression before the point in the visiting
browser, like the various Lisp modes. The result of the expression is
echoed in the minibuffer. Once loaded, `skewer-mode` is automatically
enabled in `js-mode` buffers.

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
