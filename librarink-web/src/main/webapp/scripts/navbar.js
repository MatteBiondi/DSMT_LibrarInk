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
})

// Utilities functions //TODO move to util.js file

function build_notification(id, timestamp, text, isbn){
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
                    <button type="button" class="btn-close" onclick="remove_notification(${id})" 
                            data-bs-dismiss="toast" aria-label="Close">
                    </button>
                </div>
                <a href="book_detail?isbn=${isbn}"><div class="toast-body">${ text }</div></a>
            </div>
        </li>`
}

function remove_notification(id){
    let notifications = JSON.parse(sessionStorage.getItem("notifications"));
    notifications = notifications.filter(elem => elem["id"] !== id);
    console.log(notifications)
    console.log(id)
    sessionStorage.setItem("notifications", JSON.stringify(notifications));

    let counter = $("#notification-counter");
    counter.attr("data-counter", Math.max(parseInt(counter.attr("data-counter")) - 1, 0))
    if(counter.attr("data-counter") === "0")
        counter.text("")
    else
        counter.text(counter.attr("data-counter"))
}

function show_message(type, text){
    $(".wrapper").append(`
        <div class="alert alert-${type} alert-dismissible fade show" role="alert">
             <i id="search-clear" class="bi bi-x-circle"></i>
            <div>${text}</div>
            <button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
        </div>`
    );

    let alert_elem = $('.alert');
    setTimeout(() => {alert_elem.alert("close"); alert_elem.parent().remove(alert_elem)}, 2000);
}

async function load_reservations(){
    let reserved_books = await $.post(
        "request/async",
        {"request": "read_reservations"}
    );
    //sessionStorage.setItem("reserved_books", JSON.stringify(reserved_books)) //TODO: uncomment in release

    return reserved_books;
}

async function load_wishlist(){
    let wishlist = await $.post(
        "request/async",
        {"request": "load_wishlist"}, "json")
    //sessionStorage.setItem("wishlist", JSON.stringify(wishlist)) //TODO: uncomment in release

    return wishlist;
}

async function load_grades(){
    let grades = await $.post(
        "request/async",
        {"request": "load_grades"}, "json")
    //sessionStorage.setItem("grades", JSON.stringify(grades)) //TODO: uncomment in release

    return grades;
}

async function load_local_reservations(){
    // Load wishlist
    let reserved_books = JSON.parse(sessionStorage.getItem("reserved_books"))
    if (reserved_books == null){
        reserved_books = await load_wishlist();
    }
    return reserved_books;
}

async function load_local_wishlist(){
    // Load wishlist
    let wishlist = JSON.parse(sessionStorage.getItem("wishlist"))
    if (wishlist == null){
        wishlist = await load_wishlist();
    }
    return wishlist;
}

async function load_local_grades(){
    // Load wishlist
    let grades = JSON.parse(sessionStorage.getItem("grades"))
    if (grades == null){
        grades = await load_grades();
    }
    return grades;
}