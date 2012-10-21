function foo() {
    $.get("/skewer/get", function (code) {
        $.post("/skewer/post", eval(code).toString());
        setTimeout(foo, 100);
    }, "script");
}

$("document").ready(function () {
    foo();
});
