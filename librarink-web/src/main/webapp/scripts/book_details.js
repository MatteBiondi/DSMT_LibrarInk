
let reserved_books;
let wishlist;
let grades;

$(document).ready(async () => {
    let reserve_btn = $("#reserve-btn");
    let wishlist_btn = $("#wishlist-btn");
    let isbn = $(".detailed")[0].id;

    // Load reserved books
    reserved_books = await load_local_reservations();

    // Load wishlist
    wishlist = await load_local_wishlist();

    // Check if book is already reserved
    if(reserved_books.find(reservation => reservation["isbn"] ===  isbn)){
        reserve_btn.on("click", () => cancel_reservation(reserve_btn, wishlist_btn));
        reserve_btn.text("Cancel reservation").css("color", "white");
        wishlist_btn.prop('disabled', true);
        wishlist_btn.text("Add to wishlist").css("color", "white");
    }
    else{
        reserve_btn.on("click", () => reserve(reserve_btn, wishlist_btn));
        reserve_btn.text("Reserve").css("color", "white");
        // Check if there are available copies
        if (available_copies() === 0){
            reserve_btn.prop('disabled', true);
        }

        // Check if book is already in wishlist
        if(wishlist.find(book => book ===  isbn)){
            wishlist_btn.on("click", () => remove_wishlist(wishlist_btn));
            wishlist_btn.text("Remove from wishlist").css("color", "white");
        }
        else{
            wishlist_btn.on("click", () => add_wishlist(wishlist_btn));
            wishlist_btn.text("Add to wishlist").css("color", "white");
        }
    }

    // Old grade
    grades = await load_local_grades();
    let old_grade = grades.filter((grade) => grade["isbn"] === isbn)[0]
    if (old_grade !== undefined){
        for(let i = 1; i <= old_grade["grade"]; ++i){
            $(`.s${i}`).css("color", "gold");
        }
    }

    // Rate book handlers
    $(".star").on("click", (event) => rate_book(event.currentTarget.dataset.grade));

    console.log(`[BOOK_DETAILS] Reservations: ${JSON.stringify(reserved_books)}`);
    console.log(`[BOOK_DETAILS] Wishlist: ${JSON.stringify(wishlist)}`);
    console.log(`[BOOK_DETAILS] Grades: ${JSON.stringify(grades)}`);

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

async function rate_book(grade){
    try{
        let response = await $.post(
            `request/async`,
            {"request":"rate_book","isbn": $(".detailed")[0].id, "grade":grade},
        );
        grade = parseInt(grade);
        if (response["result"] === 'succeed') {
            // Update stars color
            for(let i = 1; i <= grade; ++i){
                $(`.s${i}`).css("color", "gold");
            }
            for(let j = grade + 1; j <= $(".star").length; ++j){
                $(`.s${j}`).css("color", "gray");
            }

            // Update grades
            grades = await load_grades();

            // Show message
            console.log(grades);
            show_message("success", "Book rated");
        }
        else {
            show_message("error", "Something went wrong");
        }
    }
    catch (e) {
        show_message("error", "Something went wrong");
    }
}

function available_copies(){
    let counter = parseInt($("#copies_counter").text());
    return isNaN(counter) ? 0:counter;
}
