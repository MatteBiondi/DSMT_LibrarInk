const WS_SERVER = "172.18.0.29";
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

    // Send book isbn to track
    if (UPDATE_WS.readyState === WebSocket.OPEN)
        UPDATE_WS.send(JSON.stringify(Array.prototype.concat(displayed_books, wishlist)));
    else
        console.log("[WS] WebSocket is closed");
}

async function update (event){
    let notification = JSON.parse(event.data)
    let wishlist = await load_local_wishlist()

    // Book copy update
    if (notification.header === "update") {
        let counter = $("#" + notification.body["isbn"]).find("#copies_counter")
        // Update book counter if book details page is opened
        if (counter.length){
            counter.text(update_counter(notification.body["operation"], counter.text()))
        }
        // Wishlist notification
        if (wishlist != null && wishlist.includes(notification.body["isbn"])) {
            if (notification.body['operation'] === "add" && notification["body"]["first_copy"] === true){

                let response =  await $.post(
                    "request/async",
                    {"request": "book_title", "isbn": notification.body["isbn"]}, "json");

                if(response["result"] !== "succeed")
                    return;

                // Build notification element
                let time = new Date()
                let timestamp = time.toLocaleString('us-US',{hour:'2-digit', minute:'2-digit' ,second:'2-digit'})
                let notification_text = `New copy of <b>${ response["title"]}</b>`
                let id = Math.random().toString(16).slice(2);
                let notification_elem = build_notification(id, timestamp, notification_text, notification.body["isbn"])

                // Update session data
                update_notifications(id, notification_elem);

                // Update elements
                $("#no-notifications").remove();
                $("#notification-items").append(notification_elem)

                // Add handlers
                $(`#${id}-body`).on("click", (event) => show_detail(
                    event, {"main": () => remove_notification(id, true)})
                )

                // Update notification counter
                let counter = $("#notification-counter")
                counter.attr("data-counter", parseInt(counter.attr("data-counter")) + 1)
                counter.text(counter.attr("data-counter"))

            }
        }
    } else if (notification.header === "info") {
        UPDATE_WS.close()
    }
}

function update_counter(operation, old_value){
    if (typeof old_value != "number")
        old_value = parseInt(old_value)

    old_value = isNaN(old_value) ? 0:old_value
    let reserve_btn = $("#reserve-btn");
    switch (operation){
        case "add" :
            if (old_value === 0){
                reserve_btn.prop("disabled", false);
            }
            return old_value + 1
        case "sub":
            if(old_value === 1 && reserve_btn[0].dataset.reserved !== "true"){
                reserve_btn.prop("disabled", true);
            }
            return old_value - 1
        case "reset":
            return 0
    }
}