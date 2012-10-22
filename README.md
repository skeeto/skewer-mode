# skewer-mode

It's like SLIME and Swank for JavaScript in the browser. Requires
[simple-httpd][simple-httpd].

## Usage

 1. Start the HTTP server (`httpd-start`)
 2. Put your HTML document in the root (`httpd-root`)
 3. Include jQuery and `/skewer` as scripts (see `example.html`)
 4. Visit the document from a browser (probably http://localhost:8080/)

With `skewer-mode` enabled in a buffer, typing `C-x C-e`
(`skewer-eval-last-expression`) or `C-M-x` (`skewer-eval-defun`) will
evaluate the JavaScript expression before the point in the visiting
browser, like the various Lisp modes. The result of the expression is
echoed in the minibuffer. Once loaded, `skewer-mode` is automatically
enabled in `js-mode` buffers.

## Rationalization

I wanted something like [swank-js][swank-js] but without all the
painful setup. Having already written an Emacs web server I was
halfway there.

[simple-httpd]: https://github.com/skeeto/emacs-http-server
[swank-js]: https://github.com/swank-js/swank-js
