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
        $(`#${notification["id"]}-body`).on("click", (event) => show_detail(
            event, {"main": () => remove_notification(notification["id"], true)})
        );
    }

    if(notifications.length === 0){
        add_notification_placeholder()
    }

    $("#logout").on("click", () => sessionStorage.clear());
})
