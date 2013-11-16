// Reload any stylesheet imported from `path` by removing then re-inserting its
// link tag.
// Return false if no stylesheet was found, or true if it was.
skewer.reloadStylesheet = function(path) {
    'use strict';

    var endsWith = function(str, suffix) {
        return str.indexOf(suffix, str.length - suffix.length) !== -1;
    }

    // Find the <link> tag importing the stylesheet at `path` (if any).
    var link_elts = document.getElementsByTagName('link');

    var target_link = null;
    var cur_href = null;
    var max_href_len = 0;

    for (var i = 0; i < link_elts.length; i++) {
        cur_href = link_elts[i].getAttribute('href');
        if (endsWith(path, cur_href) && cur_href.length > max_href_len) {
            target_link = link_elts[i];
            max_href_len = cur_href.length;
        }
    }

    if (target_link === null) {
        return false;
    }

    // Swap the original link tag in and out of the DOM to force a stylesheet
    // reload from disk.
    var placeholder = document.createElement('link');
    target_link.parentNode.replaceChild(placeholder, target_link);
    placeholder.parentNode.replaceChild(target_link, placeholder);

    return true;
}
