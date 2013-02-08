// ==UserScript==
// @name         Skewer Everything
// @description  Add a toggle button to run Skewer on the current page
// @lastupdated  2013-01-24
// @version      1.0.2
// @license      Public Domain
// @include      /^https?:///
// @require      http://code.jquery.com/jquery-latest.min.js
// ==/UserScript==

var host = 'http://localhost:8080';

var toggle = $('<div/>').bind('click', inject).css({
    "width": '0px',
    "height": '0px',
    "border-style": 'solid',
    "border-width": '0 12px 12px 0',
    "border-color": 'transparent #F00 transparent transparent',
    "position": 'absolute',
    "right": 0,
    "top": 0,
    "z-index": 2147483647
});

var injected = false;

function inject() {
    if (!injected) {
        $('body').append($('<script/>').attr({src: host + '/skewer'}));
        toggle.css('border-right-color', '#0F0');
    } else {
        /* break skewer to disable it */
        if (unsafeWindow.skewer) { // Greasemonkey
            unsafeWindow.skewer.fn = null;
        } else if (unsafeWindow.window.skewer) { // Tampermonkey
            unsafeWindow.window.skewer.fn = null;
        }
        toggle.css('border-right-color', '#F00');
    }
    injected = !injected;
    GM_setValue('auto.' + location, injected);
}

/* Don't use on iframes. */
if (window.top === window.self) {
    $('body').append(toggle);
    if (GM_getValue('auto.' + location)) {
        inject();
    }
}
