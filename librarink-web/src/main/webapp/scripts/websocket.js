const WS_SERVER = "172.18.0.28";
const WS_PORT = 5000;
const WS_ENDPOINT = "update";
const WS_WARNING_MSG = "We are experiencing some troubles, the information may not be updated.\n" +
                        "We apologize for the inconvenience"
const KEEP_ALIVE_INTERVAL = 90 * 1000;
const KEEP_ALIVE_MSG ="keep-alive";

const UPDATE_WS = new WebSocket(`ws://${WS_SERVER}:${WS_PORT}/${WS_ENDPOINT}`);

UPDATE_WS.onopen = async () => {
    console.log("[WS] WebSocket opened");
    let wishlist = await load_local_wishlist();
    track_books([], wishlist);
}

UPDATE_WS.onmessage = (event) => update(event);

UPDATE_WS.onclose = () => {console.log("[WS] WebSocket closed"); show_message("warning", WS_WARNING_MSG);}

setInterval(() => {
        if (UPDATE_WS.readyState === WebSocket.OPEN)
            UPDATE_WS.send(KEEP_ALIVE_MSG)
    },
    KEEP_ALIVE_INTERVAL
)

function track_books(displayed_books, wishlist){

    console.log(`[WS] Displayed books: [${displayed_books}]`);
    console.log(`[WS] Wishlist: [${wishlist}]`);

    // Track books
    if (UPDATE_WS.readyState === WebSocket.OPEN)
        UPDATE_WS.send(JSON.stringify(Array.prototype.concat(displayed_books,wishlist)));
    else
        console.log("[WS] WebSocket is closed");
}

async function update (event){
    console.log(event.data)
    let notification = JSON.parse(event.data)
    let wishlist = await load_local_wishlist()

    // Book copy update
    if (notification.header === "update") {
        let counter = $("#" + notification.body["isbn"]).find("#copies_counter")
        if (counter.length){
            console.log("Update displayed book")
            counter.text(update_counter(notification.body["operation"], counter.text()))
        }
        // Wishlist notification
        if (wishlist != null && wishlist.includes(notification.body["isbn"])) {
            console.log("Update wishlist")
            if (notification.body['operation'] === "add"){
                let counter = $("#notification-counter");
                counter.attr("data-counter", parseInt(counter.attr("data-counter")) + 1)
                counter.text(counter.attr("data-counter"))
                let time = new Date()
                let timestamp = time.getUTCHours() + ":" + time.getUTCMinutes() + ":" + time.getUTCSeconds()
                let notification_text = `New copy of ${ notification.body["isbn"] }`
                let id = Math.random();
                let notification_elem = build_notification(id, timestamp, notification_text, notification.body["isbn"])
                $("#notification-items").append(notification_elem)
                let notifications = JSON.parse(sessionStorage.getItem("notifications")); //TODO: Load local
                if (notifications == null)
                    notifications = [];
                notifications = notifications.concat({id: id, elem: notification_elem})
                sessionStorage.setItem("notifications", JSON.stringify(notifications))
            }
        }
    } else if (notification.header === "info") {
        console.log(notification.body)
        UPDATE_WS.close()
    }
}

function update_counter(operation, old_value){ //TODO
    if (typeof old_value != "number")
        old_value = parseInt(old_value)

    old_value = isNaN(old_value) ? 0:old_value

    switch (operation){
        case "add" : return old_value + 1
        case "sub": return old_value - 1
        case "reset": return 0
    }
}