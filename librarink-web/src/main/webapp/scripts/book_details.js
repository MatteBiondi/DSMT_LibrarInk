const TIMEOUT_INTERVAL = 1000 * 5;

let reserved_books;
let wishlist;
let grades;
let book_detail;

$(document).ready(() => book_detail = new bootstrap.Modal("#book-detail"));

function show_detail(elem){
    let isbn = elem.currentTarget.id;
    let aborted = false;

    // Response timeout, trigger error if no response in time
    let timeout = setTimeout(() => {
        aborted = true;
        show_message("danger", "Server not responding")
    }, TIMEOUT_INTERVAL);

    // Requests
    $.get(
        `book_detail?isbn=${isbn}`,
        'text/html'
    ).then(
        async (details) => {
            $("#book-detail-body").html(details);

            let response = await load_details(timeout);

            if (response["result"] === "success" && !aborted){
                book_detail.show();
                track_books([isbn], response["wishlist"]);
            }},
        () => show_message("danger", "Something went wrong"));

}

async function load_details(timeout){//TODO check errors on await
    try{
        let reserve_btn = $("#reserve-btn");
        let wishlist_btn = $("#wishlist-btn");
        let displayed_isbn = $(".detailed")[0].id;


        reserved_books = await load_local_reservations();   // Load reserved books
        wishlist = await load_local_wishlist();             // Load wishlist
        grades = await load_local_grades();                 // Load old grades

        clearTimeout(timeout);

        // Check if book is already reserved
        if(reserved_books.find(reservation => reservation["isbn"] ===  displayed_isbn)){
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
            if(wishlist.find(book => book ===  displayed_isbn)){
                wishlist_btn.on("click", () => remove_wishlist(wishlist_btn));
                wishlist_btn.text("Remove from wishlist").css("color", "white");
            }
            else{
                wishlist_btn.on("click", () => add_wishlist(wishlist_btn));
                wishlist_btn.text("Add to wishlist").css("color", "white");
            }
        }

        let old_grade = grades.filter((grade) => grade["isbn"] === displayed_isbn)[0]
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

        return {result:"success", "reserved_books": reserved_books, "wishlist": wishlist, "grades": grades};
    }
    catch (e) {
        return {result:"error"};
    }
}

async function reserve(reserve_btn, wishlist_btn){
    // Send reservation request
    try{
        let displayed_isbn = $(".detailed")[0].id;
        let response = await $.post(
            `request/async`,
            {"request":"write_reservation","isbn": displayed_isbn},
        );

        // Show message and update buttons
        if (response["result"] === 'succeed') {

            // Remove from wishlist, if present
            if(wishlist.find(book => book ===  displayed_isbn)){
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
            show_message("danger", response["response"]);
        }
    }
    catch (e){
        show_message("danger", "Something went wrong");
    }
}

async function cancel_reservation(reserve_btn, wishlist_btn){
    // Cancel reservation
    try{
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
            show_message("danger", response["response"]);
        }
    }
    catch (e){
        show_message("danger", "Something went wrong");
    }
}

async function add_wishlist(wishlist_btn){
    try{
        let displayed_isbn = $(".detailed")[0].id;
        let response = await $.post(
            `request/async`,
            {"request":"add_wishlist","isbn": displayed_isbn},
        );

        if (response["result"] === 'succeed') {
            // Update buttons handlers
            wishlist_btn.off();
            wishlist_btn.text("Remove from wishlist");
            wishlist_btn.on("click", () => remove_wishlist(wishlist_btn));

            // Update wishlist
            wishlist = await load_wishlist();
            track_books([displayed_isbn], wishlist);

            // Show message
            console.log(wishlist);
            show_message("success", "Book added to wishlist");
        }
        else {
            show_message("danger", response["response"]);
        }
    }
    catch (e){
        show_message("danger", "Something went wrong");

    }
}

async function remove_wishlist(wishlist_btn){
    try{
        let displayed_isbn = $(".detailed")[0].id;
        let response = await $.post(
            `request/async`,
            {"request":"remove_wishlist","isbn": displayed_isbn},
        );
        if (response["result"] === 'succeed') {
            // Update buttons handlers
            wishlist_btn.off();
            wishlist_btn.text("Add to wishlist");
            wishlist_btn.on("click", () => add_wishlist(wishlist_btn));

            // Update wishlist
            wishlist = await load_wishlist();
            await track_books([displayed_isbn], wishlist);

            // Show message
            console.log(wishlist);
            show_message("success", "Book removed from wishlist");
        }
        else {
            show_message("danger", "Something went wrong");
        }
    }
    catch (e){
        show_message("danger", "Something went wrong");

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
            show_message("danger", response["response"]);
        }
    }
    catch (e) {
        show_message("danger", "Something went wrong");
    }
}

function available_copies(){
    let counter = parseInt($("#copies_counter").text());
    return isNaN(counter) ? 0:counter;
}
