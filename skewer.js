function skewer() {
    $.get("/skewer/get", function (code) {
        $.post("/skewer/post", eval(code).toString(), skewer);
    }, "script");
}

$("document").ready(skewer);
