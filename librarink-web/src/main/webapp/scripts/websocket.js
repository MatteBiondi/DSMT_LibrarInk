let socket

$(document).ready(() => {
    socket = new WebSocket("ws://" + location.hostname + ":5000/update")
    setInterval(() => {if (socket.readyState === 1) socket.send("keep-alive")}, 90 * 1000)
    socket.onopen = () => track_books()

    socket.onmessage = (event) => update(event)

    socket.onerror = () => warning_message()

    socket.onclose = () => warning_message()
})

async function track_books(){

    // Load displayed books
    let displayed_books = $(".detailed").toArray().flatMap((book => book.id === "" ? []:[book.id]))

    // Load wishlist
    let wishlist = JSON.parse(sessionStorage.getItem("wishlist"))
    if (wishlist == null){
        wishlist = await $.get("request/async", {request: "load_wishlist"}, "json") //TODO: which endpoint ?
        //sessionStorage.setItem("wishlist", JSON.stringify(wishlist)) //TODO: uncomment in release
    }

    console.log(`Displayed books: [${displayed_books}]`)
    console.log(`Wishlist: [${wishlist}]`)

    // Track books
    socket.send(JSON.stringify(displayed_books.concat(wishlist)))
}

function update (event){
    console.log(event.data)
    let notification = JSON.parse(event.data)
    let wishlist = JSON.parse(sessionStorage.getItem("wishlist"))

    // Book copy update
    if (notification.header === "update") {
        let counter = $("#" + notification.body["isbn"]).find("#copies_counter")
        if (counter != null){
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
                let notification_text = "New copy available" //TODO: which book ?
                $("#notification-items").append(build_notification(timestamp, notification_text))
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
    alert("We are experiencing some troubles, the information may not be updated.\nWe apologize for the inconvenience")
}

function build_notification(timestamp, text){
    return`
        <li>
            <div class="toast fade show" role="alert" aria-live="assertive" aria-atomic="true">
                <div class="toast-header">
                    <svg class="bd-placeholder-img rounded me-2" width="20" height="20" 
                            xmlns="http://www.w3.org/2000/svg" aria-hidden="true" 
                            preserveAspectRatio="xMidYMid slice" focusable="false">
                        <rect width="100%" height="100%" fill="#007aff"></rect>
                    </svg>
                    <strong class="me-auto">Wishlist</strong>
                    <small>${ timestamp }</small>
                    <button type="button" class="btn-close" onclick="remove_notification()" 
                            data-bs-dismiss="toast" aria-label="Close">
                    </button>
                </div>
                <a href="wishlsist"><div class="toast-body">${ text }</div></a>
            </div>
        </li>`
}

function remove_notification(){
    let counter = $("#notification-counter");
    counter.attr("data-counter", Math.max(parseInt(counter.attr("data-counter")) - 1, 0))
    if(counter.attr("data-counter") === "0")
        counter.text("")
    else
        counter.text(counter.attr("data-counter"))
}