function skewer() {
    $.get("/skewer/get", function (str) {
        var request = JSON.parse(str);
        var result = {"id": request.id};
        try {
            result.value = (eval, eval)(request.eval); // global eval
            result.status = "success";
        } catch (err) {
            result.value = err.message;
            result.status = "error";
        }
        if (result.value == undefined) result.value = "undefined";
        $.post("/skewer/post", JSON.stringify(result), skewer);
    }, "text");
}

$("document").ready(skewer);
