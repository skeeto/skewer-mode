/**
 * @fileOverview Live browser interaction with Emacs
 * @requires jQuery
 * @version 1.1
 */

/**
 * Makes a request to Emacs for something to evaluate. Once done it
 * sends the results back and queues itself for another request.
 * @namespace Holds all of Skewer's functionality.
 */
function skewer() {
    function callback(request) {
        var result = {
            type: "eval",
            id: request.id,
            callback: request.callback,
            strict: request.strict
        };
        var start = new Date();
        try {
            var prefix = request.strict ? '"use strict";\n' : "";
            var value = (eval, eval)(prefix + request.eval); // global eval
            result.value = skewer.safeStringify(value, request.verbose);
            result.status = "success";
        } catch (error) {
            result.value = error.toString();
            result.status = "error";
            result.error = {"name": error.name, "stack": error.stack,
                            "type": error.type, "message": error.message,
                            "eval": request.eval};
        }
        result.time = (new Date() - start) / 1000;
        result = JSON.stringify(result);
        $.post(skewer.host + "/skewer/post", result, callback, 'json');
    };
    $.get(skewer.host + "/skewer/get", callback, 'json');
}

/**
 * Host of the skewer script (CORS support).
 * @type string
 */
skewer.host = $('script[src$="/skewer"]').prop('src').match(/\w+:\/\/[^/]+/)[0];

/**
 * Stringify a potentially circular object without throwing an exception.
 * @param object The object to be printed.
 * @param {boolean} verbose Enable more verbose output.
 * @returns {string} The printed object.
 */
skewer.safeStringify = function (object, verbose) {
    "use strict";
    var circular = "#<Circular>";
    var seen = [];

    var stringify = function(obj) {
        if (obj === true) {
            return "true";
        } else if (obj === false) {
            return "false";
        } else if (obj === undefined) {
            return "undefined";
        } else if (obj === null) {
            return "null";
        } else if (typeof obj === "number") {
            return obj.toString();
        } else if (obj instanceof Array) {
            if (seen.indexOf(obj) >= 0) {
                return circular;
            } else {
                seen.push(obj);
                return "[" + obj.map(function(e) {
                    return stringify(e);
                }).join(", ") + "]";
            }
        } else if (typeof obj === "string") {
            return JSON.stringify(obj);
        } else if (typeof obj === "function") {
            if (verbose)
                return obj.toString();
            else
                return "Function";
        } else {
            if (verbose) {
                if (seen.indexOf(obj) >= 0)
                    return circular;
                else
                    seen.push(obj);
                var pairs = [];
                for (var key in obj) {
                    if (obj.hasOwnProperty(key)) {
                        var pair = JSON.stringify(key) + ":";
                        pair += stringify(obj[key]);
                        pairs.push(pair);
                    }
                }
                return "{" + pairs.join(',') + "}";
            } else {
                try {
                    return obj.toString();
                } catch (error) {
                    return ({}).toString();
                }
            }
        }
    };

    try {
        return stringify(object);
    } catch (error) {
        return skewer.safeStringify(object, false);
    }
};

/**
 * Log an object to the Skewer REPL in Emacs (console.log).
 * @param message The object to be logged.
 */
skewer.log = function(message) {
    "use strict";
    var log = {type: "log", callback: "skewer-post-log",
               value: skewer.safeStringify(message, true)};
    $.post(skewer.host + "/skewer/post", JSON.stringify(log));
};

/**
 * Report an error event to the REPL.
 * @param event A jQuery event object.
 */
skewer.error = function(event) {
    "use strict";
    var log = {
        type: "error",
        callback: "skewer-post-log",
        value: event.originalEvent.message
    };
    $.post(skewer.host + "/skewer/post", JSON.stringify(log));
};

/* Add the event listener. This doesn't work correctly in Firefox. */
$(window).bind('error', skewer.error);

$(document).ready(skewer);
