function skewer() {
    function safeStringify(obj, seen) {
        seen = seen || [];
        if (obj === true) {
            return "true";
        } else if (obj === false) {
            return "false";
        } else if (obj == undefined) {
            return "undefined";
        } else if (typeof obj == "number") {
            return obj.toString();
        } else if (obj instanceof Array) {
            if (seen.indexOf(obj) >= 0) {
                return "[ Circular ]";
            } else {
                seen.push(obj);
                return "[" + obj.map(function(e) {
                    return safeStringify(e, seen);
                }).join(", ") + "]";
            }
        } else if (typeof obj == "string") {
            return JSON.stringify(obj);
        } else if (typeof obj == "function") {
            return obj.toString();
        } else {
            return "Object";
        }
    }

    $.get("/skewer/get", function (str) {
        var request = JSON.parse(str);
        var result = {"id": request.id, "callback": request.callback};
        try {
            var value = (eval, eval)(request.eval); // global eval
            result.value = safeStringify(value);
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

$("document").ready(skewer);
