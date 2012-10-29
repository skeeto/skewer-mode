function skewer() {
    $.get("/skewer/get", function (str) {
        var request = JSON.parse(str);
        var result = {type: "eval", id: request.id,
                      callback: request.callback};
        try {
            var value = (eval, eval)(request.eval); // global eval
            result.value = skewer.safeStringify(value);
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

skewer.safeStringify = function (obj, seen) {
    seen = seen || [];
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
            return "[ Circular ]";
        } else {
            seen.push(obj);
            return "[" + obj.map(function(e) {
                return skewer.safeStringify(e, seen);
            }).join(", ") + "]";
        }
    } else if (typeof obj === "string") {
        return JSON.stringify(obj);
    } else if (typeof obj === "function") {
        return "Function";
    } else {
        return "Object";
    }
};

skewer.log = function(message) {
    var log = {type: "log", callback: "skewer-post-log",
               value: JSON.stringify(message)};
    $.post("/skewer/post", JSON.stringify(log));
};

$("document").ready(skewer);
