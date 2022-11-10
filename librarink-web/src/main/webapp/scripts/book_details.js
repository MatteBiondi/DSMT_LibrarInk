const TIMEOUT_INTERVAL = 1000 * 2.5;

let reserved_books;
let wishlist;
let grades;
let book_detail;

$(document).ready(() => book_detail = new bootstrap.Modal("#book-detail"));

async function show_detail(event, callback){
    let isbn = event.currentTarget.id;
    let aborted = false;

    // Response timeout, trigger error if no response in time
    let timeout = setTimeout(() => {
        aborted = true;
        show_message("danger", "Server not responding")
    }, TIMEOUT_INTERVAL);

    try{
        // Send parallel async request
        let page_promise = $.get(
            `book_detail?isbn=${isbn}`,
            'text/html'
        );
        let reserved_books_promise = load_local_reservations();
        let wishlist_promise = load_local_wishlist();
        let grades_promise = load_local_grades();

        // Wait for all responses
        let detail_page = await page_promise;
        reserved_books = await reserved_books_promise;
        wishlist = await wishlist_promise;
        grades = await grades_promise;

        console.log(`[BOOK_DETAILS] Reservations: ${JSON.stringify(reserved_books)}`);
        console.log(`[BOOK_DETAILS] Wishlist: ${JSON.stringify(wishlist)}`);
        console.log(`[BOOK_DETAILS] Grades: ${JSON.stringify(grades)}`);

        clearTimeout(timeout);

        if (!aborted){
            // Update and show style elements
            update_details(detail_page);
            book_detail.show();

            // Track displayed book copy counter
            track_books([isbn], wishlist);

            if(callback !== undefined){
                callback();
            }
        }
    }
    catch (e) {
        clearTimeout(timeout);
        show_message("danger", "Something went wrong");
    }
}

function update_details(detail_page){
    $("#book-detail-body").html(detail_page);

    let reserve_btn = $("#reserve-btn");
    let wishlist_btn = $("#wishlist-btn");
    let displayed_isbn = $(".detailed")[0].id;

    // Check if book is already reserved
    if(reserved_books.find((book) => book ===  displayed_isbn)){
        reserve_btn.on("click", () => cancel_reservation(reserve_btn, wishlist_btn));
        reserve_btn.text("Cancel reservation");
        wishlist_btn.prop('disabled', true);
        wishlist_btn.text("Add to wishlist");
    }
    else{
        reserve_btn.on("click", () => reserve(reserve_btn, wishlist_btn));
        reserve_btn.text("Reserve");
        // Check if there are available copies
        if (available_copies() === 0){
            reserve_btn.prop('disabled', true);
        }

        // Check if book is already in wishlist
        if(wishlist.find((book) => book ===  displayed_isbn)){
            wishlist_btn.on("click", () => remove_wishlist(wishlist_btn, true));
            wishlist_btn.text("Remove from wishlist");
        }
        else{
            wishlist_btn.on("click", () => add_wishlist(wishlist_btn));
            wishlist_btn.text("Add to wishlist");
        }
    }

    let old_grade = grades.filter((grade) => grade["isbn"] === displayed_isbn)[0]
    if (old_grade !== undefined){
        for(let i = 1; i <= old_grade["grade"]; ++i){
            $(`.s${i}`).addClass(`grade grade${i}`);
        }
    }

    // Rate book handlers
    $(".star").on("click", (event) => rate_book(event.currentTarget.dataset.grade));
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
            if(wishlist.find((book) => book ===  displayed_isbn)){
                remove_wishlist(wishlist_btn, false);
            }

            // Update buttons
            reserve_btn.off();
            reserve_btn.on("click", () => cancel_reservation(reserve_btn, wishlist_btn));
            reserve_btn.text("Cancel reservation");
            wishlist_btn.prop('disabled', true);

            // Update reserved book list
            reserved_books = add_local_reserved_book(displayed_isbn);

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
    try{
        let displayed_book = $(".detailed")[0].id;
        // Cancel reservation
        let response = await $.post(
            `request/async`,
            {"request":"cancel_reservation","isbn": displayed_book},
        );

        // Update reserved book list
        reserved_books = remove_local_reserved_book(displayed_book);

        if (response["result"] === 'succeed'){
            // Update buttons
            reserve_btn.off();
            reserve_btn.on("click", () => reserve(reserve_btn, wishlist_btn));
            reserve_btn.text("Reserve")
            wishlist_btn.prop('disabled', false);

            // Show message
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
            wishlist_btn.on("click", () => remove_wishlist(wishlist_btn, true));

            // Update wishlist
            wishlist = add_local_wishlist(displayed_isbn);
            track_books([displayed_isbn], wishlist);

            // Show message
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

async function remove_wishlist(wishlist_btn, notification){
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
            wishlist = remove_local_wishlist(displayed_isbn);
            track_books([displayed_isbn], wishlist);

            // Show message
            if(notification)
                show_message("success", "Book removed from wishlist");
        }
        else {
            show_message("danger", response["response"]);
        }
    }
    catch (e){
        show_message("danger", "Something went wrong");

    }
}

async function rate_book(grade){
    try{
        let displayed_isbn = $(".detailed")[0].id;
        let response = await $.post(
            `request/async`,
            {"request":"rate_book","isbn": displayed_isbn, "grade":grade},
        );
        grade = parseInt(grade);
        if (response["result"] === 'succeed') {
            // Update stars color
            for(let i = 1; i <= grade; ++i){
                $(`.s${i}`).addClass("grade");

            }
            for(let j = grade + 1; j <= $(".star").length; ++j){
                $(`.s${j}`).removeClass("grade");
            }

            // Update grades
            grades = update_local_grades({isbn: displayed_isbn, grade: grade});
            $.post(
                `request/async`,
                {"request":"compute_rating","isbn": displayed_isbn},
            ).then((response) => $("#user-rating").text(parseFloat(response["rating"]).toFixed(1)));
            // Show message
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
