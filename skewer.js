/**
 * @fileOverview Live browser interaction with Emacs
 * @version 1.4
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
            result = skewer.extend({
                id: request.id,
                type: request.type,
                status: 'success',
                value: ''
            }, result);
            skewer.postJSON(skewer.host + "/skewer/post", result, callback);
        } else {
            skewer.getJSON(skewer.host + "/skewer/get", callback);
        }
    };
    skewer.getJSON(skewer.host + "/skewer/get", callback);
}

/**
 * Get a JSON-encoded object from a server.
 * @param {String} url The location of the remote server
 * @param {Function} [callback] The callback to receive a response object
 */
skewer.getJSON = function(url, callback) {
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function() {
        if (xhr.readyState === 4 && xhr.status === 200) {
            callback(JSON.parse(xhr.responseText));
        }
    };
    xhr.open('GET', url, true);
    xhr.send();
};

/**
 * Send a JSON-encoded object to a server.
 * @param {String} url The location of the remote server
 * @param {Object} object The object to transmit to the server
 * @param {Function} [callback] The callback to receive a response object
 */
skewer.postJSON = function(url, object, callback) {
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function() {
        if (callback && xhr.readyState === 4 && xhr.status === 200) {
            callback(JSON.parse(xhr.responseText));
        }
    };
    xhr.open('POST', url, true);
    xhr.setRequestHeader("Content-Type", "text/plain"); // CORS
    xhr.send(JSON.stringify(object));
};

/**
 * Add the properties other objects to a target object (jQuery.extend).
 * @param {Object} target The object to receive new properties
 * @param {...Object} objects Source objects for properties
 * @returns The target object
 */
skewer.extend = function(target) {
    for (var i = 1; i < arguments.length; i++) {
        var object = arguments[i];
        for (var key in object) {
            if (object.hasOwnProperty(key)) {
                target[key] = object[key];
            }
        }
    }
    return target;
};

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
        var value = [eval][0](prefix + request.eval); // global eval
        result.value = skewer.safeStringify(value, request.verbose);
    } catch (error) {
        result = skewer.errorResult(error, result, request);
    }
    result.time = (Date.now() - start) / 1000;
    return result;
};

/**
 * Load a hosted script named by the request.
 * @param request The request object sent by Emacs
 * @returns The result object to be returned to Emacs
 */
skewer.fn.script = function(request) {
    var script = document.createElement('script');
    script.src = skewer.host + request.eval;
    document.body.appendChild(script);
    return {value: JSON.stringify(request.eval)};
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
 * Establish a new stylesheet with the provided value.
 */
skewer.fn.css = function(request) {
    var style = document.createElement('style');
    style.type = 'text/css';
    if (style.styleSheet) { // < IE9
        style.styleSheet.cssText = request.eval;
    } else {
        style.appendChild(document.createTextNode(request.eval));
    }
    document.body.appendChild(style);
    return {};
};

/**
 * Host of the skewer script (CORS support).
 * @type string
 */
(function() {
    var script = document.querySelector('script[src$="/skewer"]');
    if (script) {
        skewer.host = script.src.match(/\w+:\/\/[^/]+/)[0];
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
        } else if (Object.prototype.toString.call(obj) === '[object Date]') {
            if (verbose)
                return JSON.stringify(obj);
            else
                return obj.toString();
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
    skewer.postJSON(skewer.host + "/skewer/post", log);
};

/**
 * Report an error event to the REPL.
 * @param event An error event object.
 */
skewer.error = function(event) {
    "use strict";
    var log = {
        type: "error",
        value: event.message
    };
    skewer.postJSON(skewer.host + "/skewer/post", log);
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
    return skewer.extend({}, result, {
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

if (window.addEventListener) {
    window.addEventListener('error', skewer.error);
    window.addEventListener('load', skewer);
} else { // < IE9
    window.attachEvent('onerror', skewer.error);
    window.attachEvent('onload', skewer);
}
