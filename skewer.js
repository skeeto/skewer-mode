/**
 * @fileOverview Live browser interaction with Emacs
 * @requires jQuery
 * @version 1.1
 */

/**
 * Connects to Emacs and waits for a request. After handling the
 * request it sends back the results and queues itself for another
 * request.
 * @namespace Holds all of Skewer's functionality.
 */
function skewer() {
    function callback(request) {
        var result = skewer.fn[request.type](request);
        if (result) {
            result = JSON.stringify($.extend({
                id: request.id,
                type: request.type,
                status: 'success',
                value: ''
            }, result));
            $.post(skewer.host + "/skewer/post", result, callback, 'json');
        } else {
            $.get(skewer.host + "/skewer/get", callback, 'json');
        }
    };
    $.get(skewer.host + "/skewer/get", callback, 'json');
}

/**
 * Handlers accept a request object from Emacs and return either a
 * logical false (no response) or an object to return to Emacs.
 * @namespace Request handlers.
 */
skewer.fn = {};

/**
 * Handles an code evaluation request from Emacs.
 * @param request The request object sent by Emacs
 * @returns The result object to be returned to Emacs
 */
skewer.fn.eval = function(request) {
    var result = {
        strict: request.strict
    };
    var start = Date.now();
    try {
        var prefix = request.strict ? '"use strict";\n' : "";
        var value = (eval, eval)(prefix + request.eval); // global eval
        result.value = skewer.safeStringify(value, request.verbose);
    } catch (error) {
        result = skewer.errorResult(error, result, request);
    }
    result.time = (Date.now() - start) / 1000;
    return result;
};

/**
 * A keep-alive and connecton testing handler.
 * @param request The request object sent by Emacs
 * @returns The result object to be returned to Emacs
 */
skewer.fn.ping = function(request) {
    return {
        type: 'pong',
        date: Date.now() / 1000,
        value: request.eval
    };
};

/**
 * Host of the skewer script (CORS support).
 * @type string
 */
(function() {
    var src = $('script[src$="/skewer"]').prop('src');
    if (src) {
        skewer.host = src.match(/\w+:\/\/[^/]+/)[0];
    } else {
        skewer.host = '';  // default to the current host
    }
}());

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
    var log = {
        type: "log",
        value: skewer.safeStringify(message, true)
    };
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
        value: event.originalEvent.message
    };
    $.post(skewer.host + "/skewer/post", JSON.stringify(log));
};

/**
 * Prepare a result when an error occurs evaluating Javascript code.
 * @param error The error object given by catch.
 * @param result The resutl object to return to Emacs.
 * @param request The request object from Emacs.
 * @return The result object to send back to Emacs.
 */
skewer.errorResult = function(error, result, request) {
    "use strict";
    return $.extend({}, result, {
        value: error.toString(),
        status: 'error',
        error: {
            name: error.name,
            stack: error.stack,
            type: error.type,
            message: error.message,
            eval: request.eval
        }
    });
};

/* Add the event listener. This doesn't work correctly in Firefox. */
$(window).bind('error', skewer.error);

$(document).ready(skewer);
