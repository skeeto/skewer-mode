// ==UserScript==
// @name         Skewer Everything
// @description  Add a toggle button to run Skewer on the current page
// @lastupdated  2013-01-24
// @version      1.1
// @license      Public Domain
// @include      /^https?:///
// ==/UserScript==

var host = 'http://localhost:8080';

var toggle = document.createElement('div');
toggle.onclick = inject;
toggle.style.width = '0px';
toggle.style.height = '0px';
toggle.style.borderStyle = 'solid';
toggle.style.borderWidth = '0 12px 12px 0';
toggle.style.borderColor = 'transparent #F00 transparent transparent';
toggle.style.position = 'absolute';
toggle.style.right = 0;
toggle.style.top = 0;
toggle.style.zIndex = 214748364;

var injected = false;

function inject() {
    if (!injected) {
        var script = document.createElement('script');
        script.src = host + '/skewer';
        document.body.appendChild(script);
        toggle.style.borderRightColor = '#0F0';
    } else {
        /* break skewer to disable it */
        if (unsafeWindow.skewer) { // Greasemonkey
            unsafeWindow.skewer.fn = null;
        } else if (unsafeWindow.window.skewer) { // Tampermonkey
            unsafeWindow.window.skewer.fn = null;
        }
        toggle.style.borderRightColor = '#F00';
    }
    injected = !injected;
    GM_setValue('auto.' + location, injected);
}

/* Don't use on iframes. */
if (window.top === window.self) {
    document.body.appendChild(toggle);
    if (GM_getValue('auto.' + location)) {
        inject();
    }
}
