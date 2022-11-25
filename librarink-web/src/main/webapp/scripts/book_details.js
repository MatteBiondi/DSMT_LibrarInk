const TIMEOUT_INTERVAL = 1000 * 3;

let reserved_books;
let wishlist;
let grades;
let loans;
let book_detail;

$(document).ready(() => book_detail = new bootstrap.Modal("#book-detail"));

async function show_detail(event, callbacks){
    let isbn = event.currentTarget.dataset.isbn;
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
        let loans_promise = load_local_loans();

        // Wait for all responses
        let detail_page = await page_promise;
        reserved_books = await reserved_books_promise;
        wishlist = await wishlist_promise;
        grades = await grades_promise;
        loans = await  loans_promise;

        // console.log(`[BOOK_DETAILS] Reservations: ${JSON.stringify(reserved_books)}`);
        // console.log(`[BOOK_DETAILS] Wishlist: ${JSON.stringify(wishlist)}`);
        // console.log(`[BOOK_DETAILS] Loans: ${JSON.stringify(loans)}`);
        // console.log(`[BOOK_DETAILS] Grades: ${JSON.stringify(grades)}`);

        clearTimeout(timeout);

        if (!aborted){
            // Update and show style elements
            update_details(detail_page, callbacks);
            book_detail.show();

            // Track displayed book copy counter
            track_books([isbn], wishlist);

            if(callbacks !== undefined && callbacks["main"] !== undefined){
                callbacks["main"]();
            }
        }
    }
    catch (exception) {
        clearTimeout(timeout);
        if(!aborted){
            show_error_message(exception);
        }
    }
}

function update_details(detail_page, callbacks){
    $("#book-detail-body").html(detail_page);
    let reserve_btn = $("#reserve-btn");
    let wishlist_btn = $("#wishlist-btn");
    let details = $(".detailed", detail_page)[0];
    if(details === undefined)
        throw "unexpected error";

    let displayed_isbn = details.id;

    if(loans.find(loan => loan["isbn"] === displayed_isbn)){
        // Book already borrowed
        reserve_btn.remove()
        wishlist_btn.remove();
        let loan_date = new Date(                                               // Build JS date
            loans
                .find(loan => loan["isbn"] === displayed_isbn)["start_date"]    // Find string date
                .split(" ")                                                     // Split string date by whitespace
                .filter(elem => elem !== "CET")                                 // Remove CET token
                .join(" ")                                                      // Join tokens
        );
        loan_date.setDate(loan_date.getDate() + 30);
        $(".left_column").append(`
            <div class="date"><h3>Book lent until:  </h3><span>${loan_date.toDateString()}</span> </div>`
        )
    }
    else {
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
    }

    let old_grade = grades.filter((grade) => grade["isbn"] === displayed_isbn)[0]
    if (old_grade !== undefined){
        for(let i = 1; i <= old_grade["grade"]; ++i){
            $(`.s${i}`).addClass(`grade grade${i}`);
        }
    }

    // Rate book handlers
    $(".star").on("click", (event) => rate_book(event.currentTarget.dataset.grade));

    let old_reserved_books = reserved_books.slice();
    let old_wishlist = wishlist.slice()
    let modal_close = $("#modal-close");
    let modal_xl =  $(".modal-xl");
    let modal_dialog = $(".modal-dialog");

    // Book detail popup page handling
    modal_close.off("click");
    modal_xl.off("click");

    modal_dialog.off("click");

    modal_close.on("click", () => update_user_page(
        callbacks,
        {"reserved_books": old_reserved_books, "wishlist": old_wishlist})
    )
    modal_xl.on("click", () => update_user_page(
        callbacks,
        {"reserved_books": old_reserved_books, "wishlist": old_wishlist})
    )
    modal_dialog.on("click", (event) => event.stopPropagation());
}

async function reserve(reserve_btn, wishlist_btn){
    // Send reservation request
    try{
        let displayed_isbn = $(".detailed")[0].id;
        reserve_btn[0].dataset.reserved = "true";
        let response = await $.post(
            `request/async`,
            {"request":"write_reservation","isbn": displayed_isbn},
        );

        // Show message and update buttons
        if (response["result"] === 'succeed') {

            // Update buttons
            reserve_btn.off();
            reserve_btn.on("click", () => cancel_reservation(reserve_btn, wishlist_btn));
            reserve_btn.text("Cancel reservation");
            wishlist_btn.prop('disabled', true);
            reserve_btn.prop("data-reserved", "true");

            // Update reserved book list
            reserved_books = add_local_reserved_book(displayed_isbn);

            show_message("success", "Book reserved");

            // Remove from wishlist, if present
            if(wishlist.find((book) => book ===  displayed_isbn)){
                remove_wishlist(wishlist_btn, false);
            }
        }
        else {
            show_message("danger", response["response"]);
        }
    }
    catch (exception){
        show_error_message(exception);
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
            reserve_btn.text("Reserve");
            wishlist_btn.on("click", () => add_wishlist(wishlist_btn, true));
            wishlist_btn.prop('disabled', false);

            // Show message
            show_message("success", "Reservation canceled");
        }
        else {
            show_message("danger", response["response"]);
        }
    }
    catch (exception){
        show_error_message(exception);
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
    catch (exception){
        show_error_message(exception);
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
    catch (exception){
        show_error_message(exception);

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
    catch (exception) {
        show_error_message(exception);
    }
}

function available_copies(){
    let counter = parseInt($("#copies_counter").text());
    return isNaN(counter) ? 0:counter;
}

function update_user_page(callbacks, old_books){
    let reservation_cancelled = old_books["reserved_books"].filter(isbn => !reserved_books.includes(isbn))[0];
    let wishlist_removed = old_books["wishlist"].filter(isbn => !wishlist.includes(isbn))[0];
    let reservation_added = reserved_books.filter(isbn => !old_books["reserved_books"].includes(isbn))[0];
    let wishlist_added = wishlist.filter(isbn => !old_books["wishlist"].includes(isbn))[0];

    if(callbacks !== undefined){
        if(callbacks["insert_into_res"] !== undefined && reservation_added !== undefined){
            callbacks["insert_into_res"](reservation_added);
        }
        if(callbacks["insert_into_wish"] !== undefined && wishlist_added !== undefined){
            callbacks["insert_into_wish"](wishlist_added);
        }
        if(callbacks["remove_from_res"] !== undefined && reservation_cancelled !== undefined){
            callbacks["remove_from_res"](reservation_cancelled);
        }
        if(callbacks["remove_from_wish"] !== undefined && wishlist_removed !== undefined){
            callbacks["remove_from_wish"](wishlist_removed);
        }
    }

}

function show_error_message(exception){
    if(exception !== undefined && Object.prototype.toString.call(exception) === "[object String]"){
        show_message("danger", "Something went wrong: " +
            exception.charAt(0).toUpperCase() + exception.slice(1));
    }
    else{
        show_message("danger", "Something went wrong")
    }
}