const socket = new WebSocket('ws://' + location.hostname + ':5000/update')
setInterval(function(){
    if (socket.readyState === 1) socket.send("keep-alive")
}, 90 * 1000)

socket.onopen = async function (){

    // Load displayed books
    let displayed_books = $(".detailed").toArray().flatMap((book => book.id === '' ? []:[book.id]))

    // Load wishlist
    let wishlist = JSON.parse(sessionStorage.getItem("wishlist"))
    if (wishlist == null){
        wishlist = await $.get("request/async", {request: "load_wishlist"}, "json")
        //sessionStorage.setItem("wishlist", JSON.stringify(wishlist)) //TODO: uncomment in release
    }

    console.log(`Displayed books: [${displayed_books}]`)
    console.log(`Wishlist: [${wishlist}]`)

    // Track books
    socket.send(JSON.stringify(displayed_books.concat(wishlist)))
}

socket.onmessage = function (event){
    console.log(event.data)
    let notification = JSON.parse(event.data)
    let wishlist = JSON.parse(sessionStorage.getItem("wishlist"))

    if (notification.type === 'update') {
        let counter = $("#" + notification.data.isbn).find("#copies_counter")
        if (counter != null){
            console.log("Update displayed book")
            counter.text(update_counter(notification.data.operation, counter.text()))
        }
        if (wishlist != null && wishlist.includes(notification.data.isbn)) {
            console.log("Update wishlist")
            if (notification.data.operation === 'add'){//TODO: if old copy number was 0
                alert("New copy available !") //TODO: notification
            }
        }
    } else if (notification.type === 'info') {
        console.log(notification.data)
        socket.close()
    }
}

socket.onerror = function (event){
   //alert("We are experiencing some troubles, the information may not be updated.\nWe apologize for the
    // inconvenience")
}

socket.onclose = function (){
    alert("We are experiencing some troubles, the information may not be updated.\nWe apologize for the inconvenience")
    console.log("Websocket closed")
}

function update_counter(operation, old_value){
    if (typeof old_value != "number")
        old_value = parseInt(old_value)
    switch (operation){
        case "add" : return old_value + 1
        case "sub": return old_value - 1
        case "reset": return 0
    }
}
