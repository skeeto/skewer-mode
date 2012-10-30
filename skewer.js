function skewer() {
    $.get("/skewer/get", function (str) {
        var request = JSON.parse(str);
        var result = {type: "eval", id: request.id,
                      callback: request.callback, strict: request.strict};
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
        $.post("/skewer/post", JSON.stringify(result), skewer);
    }, "text");
}

skewer.safeStringify = function (object, verbose) {
    var circular = "#<Circular>";
    var seen = [];

    function stringify(obj) {
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
                var output = "{";
                for (key in obj) {
                    output += JSON.stringify(key) + ":";
                    output += stringify(obj[key]);
                    output += ",";
                }
                return output.slice(0, -1) + "}";
            } else {
                return "Object";
            }
        }
    }

    try {
        return stringify(object);
    } catch (error) {
        return skewer.safeStringify(object, false);
    }
};

skewer.log = function(message) {
    var log = {type: "log", callback: "skewer-post-log",
               value: skewer.safeStringify(message, true)};
    $.post("/skewer/post", JSON.stringify(log));
};

$("document").ready(skewer);
