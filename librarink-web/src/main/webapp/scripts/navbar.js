// Code in charge of handling

// Code on document ready
$(document).ready(() => {
    // Remove any session information in case of click to come back to homepage
    $("#header").on('click', () => {
        // Searching will be performed without any memory about previous searching actions
        sessionStorage.removeItem("page");
        sessionStorage.removeItem("search");
        sessionStorage.removeItem("keyword");
    });

    // Get notifications from session storage
    let notifications = JSON.parse(sessionStorage.getItem("notifications"));
    if (notifications == null)
        notifications = [];

    // For each notification visualize them in notification section on navbar top right corner
    for(const notification of notifications){
        let counter = $("#notification-counter");
        counter.attr("data-counter", parseInt(counter.attr("data-counter")) + 1);
        counter.text(counter.attr("data-counter"));
        $("#notification-items").append(notification["elem"]);
        // In case of click, show book details and remove notification
        $(`#${notification["id"]}-body`).on("click", (event) => show_detail(
            event, {"main": () => remove_notification(notification["id"], true)})
        );
    }

    // If no notification, shows a placeholder
    if(notifications.length === 0){
        add_notification_placeholder()
    }

    // In case of logout clear any data stored into session storage
    $("#logout").on("click", () => sessionStorage.clear());
})
