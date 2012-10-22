function skewer() {
    $.get("/skewer/get", function (str) {
        var request = JSON.parse(str);
        var result = {"id": request.id};
        try {
            value = (eval, eval)(request.eval); // global eval
            result.status = "success";
        } catch (error) {
            value = error;
            result.status = "error";
            result.error = {"name": error.name, "stack": error.stack,
                            "type": error.type, "message": error.message};
        }
        if (value == undefined)
            result.value = "undefined"
        else
            result.value = value.toString();
        $.post("/skewer/post", JSON.stringify(result), skewer);
    }, "text");
}

$("document").ready(skewer);
