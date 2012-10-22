function skewer() {
    $.get("/skewer/get", function (code) {
        var result = eval(code) || "undefined";
        $.post("/skewer/post", result.toString(), skewer);
    }, "script");
}

$("document").ready(skewer);
