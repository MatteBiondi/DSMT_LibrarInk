$(document).ready(() => {
    $("#header").on('click', () => {
        sessionStorage.removeItem("page");
        sessionStorage.removeItem("search");
        sessionStorage.removeItem("keyword");
    });

    let notifications = JSON.parse(sessionStorage.getItem("notifications"));
    if (notifications == null)
        notifications = [];

    for(const notification of notifications){
        let counter = $("#notification-counter");
        counter.attr("data-counter", parseInt(counter.attr("data-counter")) + 1);
        counter.text(counter.attr("data-counter"));
        $("#notification-items").append(notification["elem"]);
    }

    $("#logout").on("click", () => sessionStorage.clear());
})
