function skewer() {
    $.get("/skewer/get", function (code) {
        var result;
        try {
            result = (eval, eval)(code); // global eval
        } catch (err) {
            result = "error: " + err.message;
        }
        if (result == undefined) result = "undefined";
        $.post("/skewer/post", result.toString(), skewer);
    }, "text");
}

$("document").ready(skewer);
