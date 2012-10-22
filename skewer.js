function skewer() {
    $.get("/skewer/get", function (str) {
        var request = JSON.parse(str);
        var result = {"id": request.id};
        try {
            value = (eval, eval)(request.eval); // global eval
            result.status = "success";
        } catch (err) {
            value = err.message;
            result.status = "error";
        }
        if (value == undefined)
            result.value = "undefined"
        else
            result.value = value.toString();
        $.post("/skewer/post", JSON.stringify(result), skewer);
    }, "text");
}

$("document").ready(skewer);
