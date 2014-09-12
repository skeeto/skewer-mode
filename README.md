# Skewer: live web development with Emacs

Provides live interaction with JavaScript, CSS, and HTML in a web
browser. Expressions are sent on-the-fly from an editing buffer to be
evaluated in the browser, just like Emacs does with an inferior Lisp
process in Lisp modes.

* Watch the [**demo video** on YouTube](http://youtu.be/4tyTgyzUJqM)
  ([webm](http://nullprogram.s3.amazonaws.com/skewer/demo.webm))

**Skewer is available from [MELPA][melpa]**, which will install the
dependencies for you. This package and its dependencies are pure
Elisp, meaning setup is a breeze, the whole thing is highly portable,
and it works with many browsers.

Dependencies:

 * [simple-httpd][simple-httpd] (available on MELPA)
 * [js2-mode][js2-mode] (available on ELPA)

Skewer requires Emacs 24.3 or later.

## Usage

### Quick version

If Skewer was installed from MELPA, skip to step 3.

 1. Put this repository directory in your `load-path`
 2. Load skewer-mode.el
 3. M-x `run-skewer` to attach a browser to Emacs
 4. From a `js2-mode` buffer with `skewer-mode` minor mode enabled,
    send forms to the browser to evaluate

The function `skewer-setup` can be used to configure all of mode hooks
(previously this was the default). This can also be done manually like
so,

```el
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
```

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

Additionally, `css-mode` and `html-mode` get similar sets of bindings
for modifying the CSS rules and HTML on the current page.

#### CSS

 * <kbd>C-x C-e</kbd>: Load the declaration at the point.
 * <kbd>C-M-x</kbd>:   Load the entire rule around the point.
 * <kbd>C-c C-k</kbd>: Load the current buffer as a stylesheet.

#### HTML

 * <kbd>C-M-x</kbd>:   Load the HTML tag immediately around the point.

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

To skewer your own document rather than the provided blank one,

 1. Load the dependencies
 2. Load `skewer-mode.el`
 3. Start the HTTP server (`httpd-start`)
 4. Include "http://localhost:8080/skewer" as a script
    (see example.html and check your `httpd-port`)
 5. Visit the document from your browser

Skewer fully supports CORS so the document need not be hosted by Emacs
itself. A Greasemonkey userscript is provided, *Skewer Everything*,
for injecting Skewer into any arbitrary page you're visiting without
needing to modify the page on the host. More information below.

**Don't copy `skewer.js` anywhere or use it directly**. Emacs hosts
this script itself, manipulating it in memory before it reaches the
browser. Always access it through the servlet on the Emacs webserver
as `/skewer`.

### Browser Support

Skewer is known to work properly with Firefox, Chrome, Safari, Opera,
and IE8+. Except for CSS and HTML, Skewer will work in IE7 when
`document.querySelector` and `JSON` are polyfilled. If you find any
other JavaScript-supported browser that doesn't work with Skewer,
please report it.

## REPL

A REPL into the browser can be created with `M-x skewer-repl`, or
<kbd>C-c C-z</kbd>. This should work like a console within the
browser. Messages can be logged to this REPL with `skewer.log()` (like
`console.log()`).

Results of expressions evaluated in the REPL are printed more
verbosely than in the minibuffer, when possible. This may help in
debugging.

## Skewering with CORS

Skewer supports [Cross-origin Resource Sharing (CORS)][cors]. This
means you can Skewer a document hosted from any server without needing
any special changes on that server, except for including `/skewer` as
a script in that document.

If you don't control the server from which you want to skewer pages --
such that you can't add the Skewer's script -- the provided
Greasemonkey userscript (`.user.js`) can be used to inject it into any
page you visit. Note that this userscript will assume you're running
the Skewer server at http://localhost:8080/ (simple-httpd's default
port). If this isn't true, you need to edit the top of the userscript.

The script isn't actually injected until you switch the toggle in the
top-right corner, the red/green triangle.

Alternatively, the following bookmarklet will load skewer on demand:

```js
javascript:(function(){var d=document;var s=d.createElement('script');s.src='http://localhost:8080/skewer';d.body.appendChild(s);})()
```

## bower

Also provided are some functions for loading libraries from the bower
infrastructure on the fly. This is accessed with `skewer-bower-load`.
For example, I often find it useful to load jQuery when skewering a
page that doesn't have jQuery installed.

Note: to use this **bower does *not* need to be installed**, only git.
It's just the bower infrastructure being used. Unfortunately this
infrastructure is a mess right now; many packages are in some sort of
broken state -- missing dependencies, missing metadata, broken
metadata, or an invalid repository URL. Some of this is due to
under-specification of the metadata by the bower project.

## Motivation

I wanted something like [swank-js][swank-js] but without all the
painful setup. Having already written an Emacs web server I was
halfway there. It took relatively little code to accomplish.

I also didn't want to rely a browser-specific feature, like MozRepl or
WebKit's remote debugger ([kite][kite]).

The name refers to the idea that Emacs is *skewering* the browser from
server-side.

[simple-httpd]: https://github.com/skeeto/emacs-http-server
[js2-mode]: https://github.com/mooz/js2-mode
[melpa]: http://melpa.milkbox.net/
[swank-js]: https://github.com/swank-js/swank-js
[cors]: http://en.wikipedia.org/wiki/Cross-origin_resource_sharing
[kite]: https://github.com/jscheid/kite
