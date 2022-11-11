
function build_notification(id, timestamp, text, isbn){
    return`
        <li id="${id}">
            <div class="toast fade show" role="alert" aria-live="assertive" aria-atomic="true">
                <div class="toast-header">
                    <svg class="bd-placeholder-img rounded me-2" width="20" height="20" 
                            xmlns="http://www.w3.org/2000/svg" aria-hidden="true" 
                            preserveAspectRatio="xMidYMid slice" focusable="false">
                        <rect width="100%" height="100%" fill="#007aff"></rect>
                    </svg>
                    <strong class="me-auto">Wishlist</strong>
                    <small>${ timestamp }</small>
                    <button type="button" class="btn-close" onclick="remove_notification('${id}')" 
                            data-bs-dismiss="toast" aria-label="Close">
                    </button>
                </div>
                <div id="${id}-body" class="toast-body" data-isbn="${isbn }">${ text }</div>
            </div>
        </li>`
}

function remove_notification(id, remove_elem){
    //Update session data
    let notifications = JSON.parse(sessionStorage.getItem("notifications"));
    notifications = notifications.filter(elem => elem["id"] !== id);

    sessionStorage.setItem("notifications", JSON.stringify(notifications));

    let counter = $("#notification-counter");
    counter.attr("data-counter", Math.max(parseInt(counter.attr("data-counter")) - 1, 0))
    if(counter.attr("data-counter") === "0")
        counter.text("")
    else
        counter.text(counter.attr("data-counter"))

    if(remove_elem !== undefined){
        $(`#${id}`).remove();
    }

    // Add no notifications message
    if(notifications.length === 0){
        add_notification_placeholder();
    }
}

function update_notifications(id, elem){
    let notifications = JSON.parse(sessionStorage.getItem("notifications"));
    if (notifications == null)
        notifications = [];

    notifications = Array.prototype.concat(
        notifications,
        [{id: id, elem: elem}]
    )

    sessionStorage.setItem("notifications", JSON.stringify(notifications))
}

function add_notification_placeholder(){
    $("#notification-items").append(`
    <li id="no-notifications">
        <div class="toast fade show" role="alert" aria-live="assertive" aria-atomic="true">
            <div class="toast-header">
                <svg class="bd-placeholder-img rounded me-2" width="20" height="20" 
                        xmlns="http://www.w3.org/2000/svg" aria-hidden="true" 
                        preserveAspectRatio="xMidYMid slice" focusable="false">
                    <rect width="100%" height="100%" fill="#007aff"></rect>
                </svg>
                <strong class="me-auto">No notifications</strong>
            </div>
        </div>
    </li>`);
}

function show_message(type, text){
    $(".wrapper").prepend(`
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
        {"request": "read_reservations"}, "json"
    );

    reserved_books = reserved_books.flatMap((reservation) => reservation["isbn"]);
    sessionStorage.setItem("reserved_books", JSON.stringify(reserved_books))

    return reserved_books;
}

async function load_wishlist(){
    let wishlist = await $.post(
        "request/async",
        {"request": "load_wishlist"}, "json")
    sessionStorage.setItem("wishlist", JSON.stringify(wishlist))

    return wishlist;
}

async function load_grades(){
    let grades = await $.post(
        "request/async",
        {"request": "load_grades"}, "json")
    sessionStorage.setItem("grades", JSON.stringify(grades))

    return grades;
}

async function load_local_reservations(){
    // Load wishlist
    let reserved_books = JSON.parse(sessionStorage.getItem("reserved_books"))
    if (reserved_books == null){
        reserved_books = await load_reservations();
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

function add_local_reserved_book(newBook){
    let reserved_books = JSON.parse(sessionStorage.getItem("reserved_books"))
    if(reserved_books == null){
        reserved_books = [];
    }
    reserved_books = Array.prototype.concat(reserved_books, newBook);
    sessionStorage.setItem("reserved_books", JSON.stringify(reserved_books));

    return reserved_books;
}

function remove_local_reserved_book(oldBook){
    let reserved_books = JSON.parse(sessionStorage.getItem("reserved_books"))
    if(reserved_books == null){
        reserved_books = [];
    }
    reserved_books = reserved_books.filter((book) => book !== oldBook);

    sessionStorage.setItem("reserved_books", JSON.stringify(reserved_books));

    return reserved_books;
}

function add_local_wishlist(newBook){
    let wishlist = JSON.parse(sessionStorage.getItem("wishlist"))
    if(wishlist == null){
        wishlist = [];
    }
    wishlist = Array.prototype.concat(wishlist, newBook);
    sessionStorage.setItem("wishlist", JSON.stringify(wishlist));

    return wishlist;
}

function remove_local_wishlist(oldBook){
    let wishlist = JSON.parse(sessionStorage.getItem("wishlist"))
    if(wishlist == null){
        wishlist = [];
    }
    wishlist = wishlist.filter((book) => book !== oldBook);
    sessionStorage.setItem("wishlist", JSON.stringify(wishlist));

    return wishlist;
}

function update_local_grades(grade){
    let grades = JSON.parse(sessionStorage.getItem("grades"))
    if(grades == null){
        grades = [];
    }
    let index = grades.findIndex((oldGrade) => oldGrade["isbn"] === grade["isbn"]);
    if(index !== -1){
        grades[index]["grade"] = grade["grade"];
    }
    else {
        grades = Array.prototype.concat(grades, grade);
    }
    sessionStorage.setItem("grades", JSON.stringify(grades));

    return grades;
}