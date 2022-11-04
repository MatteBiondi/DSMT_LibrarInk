let socket;
let notifications;
const erlang_server = "localhost"

$(document).ready(() => {
    notifications = JSON.parse(sessionStorage.getItem("notifications"));
    if (notifications == null)
        notifications = [];

    socket = new WebSocket("ws://" + erlang_server + ":5000/update")
    setInterval(() => {if (socket.readyState === 1) socket.send("keep-alive")}, 90 * 1000)
    socket.onopen = () => track_books()

    socket.onmessage = (event) => update(event)

    socket.onerror = () => warning_message()

})

async function track_books(){

    if (socket.readyState !== WebSocket.OPEN)
        return;

    // Load displayed books
    let displayed_books = $(".detailed").toArray().flatMap((book => book.id === "" ? []:[book.id]))

    // Load wishlist
    let wishlist = JSON.parse(sessionStorage.getItem("wishlist"))
    if (wishlist == null){
        wishlist = await load_wishlist();
    }

    console.log(`Displayed books: [${displayed_books}]`)
    console.log(`Wishlist: [${wishlist}]`)

    // Track books
    socket.send(JSON.stringify(displayed_books.concat(wishlist)))

}

async function update (event){
    console.log(event.data)
    let notification = JSON.parse(event.data)
    let wishlist = JSON.parse(sessionStorage.getItem("wishlist"))

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
                notifications = notifications.concat({id: id, elem: notification_elem})
                sessionStorage.setItem("notifications", JSON.stringify(notifications))
            }
        }
    } else if (notification.header === "info") {
        console.log(notification.body)
        socket.close()
    }
}

function update_counter(operation, old_value){
    if (typeof old_value != "number")
        old_value = parseInt(old_value)

    old_value = isNaN(old_value) ? 0:old_value

    switch (operation){
        case "add" : return old_value + 1
        case "sub": return old_value - 1
        case "reset": return 0
    }
}

function warning_message(){
    show_message("warning", "We are experiencing some troubles, the information may not be updated.\nWe apologize" +
        " for the inconvenience")
}