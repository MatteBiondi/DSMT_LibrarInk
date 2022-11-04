
let reserved_books;
let wishlist;

$(document).ready(async () => {
    let reserve_btn = $("#reserve-btn");
    let wishlist_btn = $("#wishlist-btn");
    let isbn = $(".detailed")[0].id;

    // Load reserved books
    reserved_books = JSON.parse(sessionStorage.getItem("reserved_books"));
    if (reserved_books == null){
        reserved_books = await load_reservations()
    }

    // Load wishlist
    wishlist = JSON.parse(sessionStorage.getItem("wishlist"))
    if (wishlist == null){
        wishlist = await load_wishlist();
    }

    console.log(reserved_books);
    console.log(wishlist);

    // Check if book is already reserved
    if(reserved_books.find(reservation => reservation["isbn"] ===  isbn)){
        reserve_btn.on("click", () => cancel_reservation(reserve_btn, wishlist_btn));
        reserve_btn.text("Cancel reservation").css("color", "white");
        wishlist_btn.prop('disabled', true);
        wishlist_btn.text("Add to wishlist").css("color", "white");;
    }
    else{
        reserve_btn.on("click", () => reserve(reserve_btn, wishlist_btn));
        reserve_btn.text("Reserve").css("color", "white");;
        // Check if there are available copies
        if (available_copies() === 0){
            reserve_btn.prop('disabled', true);
        }

        // Check if book is already in wishlist
        if(wishlist.find(book => book ===  isbn)){
            wishlist_btn.on("click", () => remove_wishlist(wishlist_btn));
            wishlist_btn.text("Remove from wishlist").css("color", "white");;
        }
        else{
            wishlist_btn.on("click", () => add_wishlist(wishlist_btn));
            wishlist_btn.text("Add to wishlist").css("color", "white");;
        }
    }
});

async function reserve(reserve_btn, wishlist_btn){
    let isbn = $(".detailed")[0].id;

    // Send reservation request
    let response = await $.post(
        `request/async`,
        {"request":"write_reservation","isbn": isbn},
    );

    // Show message and update buttons
    if (response["result"] === 'succeed') {

        // Remove from wishlist, if present
        if(wishlist.find(book => book ===  isbn)){
            await remove_wishlist(wishlist_btn);
        }

        // Update buttons
        reserve_btn.off();
        reserve_btn.on("click", () => cancel_reservation(reserve_btn, wishlist_btn));
        reserve_btn.text("Cancel reservation");
        wishlist_btn.prop('disabled', true);

        // Update reserved book list
        reserved_books = await load_reservations();

        // Show message
        console.log(reserved_books);
        show_message("success", "Book reserved");
    }
    else {
        console.log(response);
        show_message("error", "Something went wrong");
    }
}

async function cancel_reservation(reserve_btn, wishlist_btn){
    // Cancel reservation
    let response = await $.post(
        `request/async`,
        {"request":"cancel_reservation","isbn": $(".detailed")[0].id},
    );

    // Update reserved book list
    reserved_books = await load_reservations();

    if (response["result"] === 'succeed'){
        // Update buttons
        reserve_btn.off();
        reserve_btn.on("click", () => reserve(reserve_btn, wishlist_btn));
        reserve_btn.text("Reserve")
        wishlist_btn.prop('disabled', false);

        // Show message
        console.log(reserved_books);
        show_message("success", "Reservation canceled");
    }
    else {
        show_message("error", "Something went wrong");
    }
}

async function add_wishlist(wishlist_btn){
    let response = await $.post(
        `request/async`,
        {"request":"add_wishlist","isbn": $(".detailed")[0].id},
    );

    if (response["result"] === 'succeed') {
        // Update buttons handlers
        wishlist_btn.off();
        wishlist_btn.text("Remove from wishlist");
        wishlist_btn.on("click", () => remove_wishlist(wishlist_btn));

        // Update wishlist
        wishlist = await load_wishlist();
        await track_books();

        // Show message
        console.log(wishlist);
        show_message("success", "Book added to wishlist");
    }
    else {
        show_message("error", "Something went wrong");
    }
}

async function remove_wishlist(wishlist_btn){
    let response = await $.post(
        `request/async`,
        {"request":"remove_wishlist","isbn": $(".detailed")[0].id},
    );
    if (response["result"] === 'succeed') {
        // Update buttons handlers
        wishlist_btn.off();
        wishlist_btn.text("Add to wishlist");
        wishlist_btn.on("click", () => add_wishlist(wishlist_btn));

        // Update wishlist
        wishlist = await load_wishlist();
        await track_books();

        // Show message
        console.log(wishlist);
        show_message("success", "Book removed from wishlist");
    }
    else {
        show_message("error", "Something went wrong");
    }
}

function available_copies(){
    let counter = parseInt($("#copies_counter").text());
    return isNaN(counter) ? 0:counter;
}