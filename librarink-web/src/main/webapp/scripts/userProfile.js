// This code is in charge of handling any user visualization for wishlist, pending loans and pending reservations

// *********** Constant section ***********
const CALLBACK_OBJ = {
    remove_from_res : remove_from_carousel.bind(null, "reservations"),
    remove_from_wish: remove_from_carousel.bind(null, "wishlist"),
    insert_into_res : insert_into_carousel.bind(null, "reservations"),
    insert_into_wish: insert_into_carousel.bind(null, "wishlist")
}
// ****************************************


// Code on document ready
$(document).ready(() => {
        // Put carousels that will contain books
        put_carousel("wishlist");
        put_carousel("loans");
        put_carousel("reservations");

        // Load books into relative carousel
        load_books_image("wishlist");
        load_books_image("loans");
        load_books_image("reservations");
    }
)

// *********** Functions section ***********
// This function is in charge of create the skeleton of the carousel
function put_carousel(type){
    let carousel = `
        <!-- Carousel wrapper -->
        <div
                id="${type}CarouselMultiItem"
                class="carousel slide carousel-dark text-center"
                data-bs-ride="carousel"
        >
        
            <!-- Controls -->
            <div class="d-flex justify-content-center mb-4">
                <!-- Previous carousel row -->
                <button
                        class="carousel-control-prev position-relative"
                        type="button"
                        data-bs-target="#${type}CarouselMultiItem"
                        data-bs-slide="prev"
                >
                    <span class="carousel-control-prev-icon" aria-hidden="true"></span>
                    <span class="visually-hidden">Previous</span>
                </button>
                
                <!-- Next carousel row -->
                <button
                        class="carousel-control-next position-relative"
                        type="button"
                        data-bs-target="#${type}CarouselMultiItem"
                        data-bs-slide="next"
                >
                    <span class="carousel-control-next-icon" aria-hidden="true"></span>
                    <span class="visually-hidden">Next</span>
                </button>
            </div>
            
            
            <!-- Inner content -->
            <div class="carousel-inner py-4" id="${type}Gallery">
                <!-- FILLED BY AJAX REQUEST -->
            </div>
            
            
        </div>
    `;

    $("#"+type+"Row").append(carousel);
}


// This function is in charge of load books image for the carousel type passed as parameter
async function load_books_image(type) {
    let isbn_list = [];
    let images_url_array = [];

    // Get books for the selected carousel
    try{
        if (type === "reservations")
            isbn_list = await load_local_reservations();
        else if (type === "wishlist")
            isbn_list = await load_local_wishlist();
        else if (type === "loans"){
            let lent_books = await load_local_loans();
            isbn_list = lent_books.map((book) => book.isbn);
        }

        // Load image for that books
        for (const book_isbn of isbn_list){
            let image_url = await $.post(
                "request/async",
                {
                    "request": "load_image_url",
                    "isbn": book_isbn
                }, "json")
            images_url_array.push(image_url);
        }
    }catch(exception){
        show_message("danger", "Something went wrong");
    }

    // Shows images in the carousel
    gui_book_elements(images_url_array, type);
}


// This function is in charge of putting images in carousel
function gui_book_elements(url_list, type) {
    let num_rows = Math.ceil(url_list.length/4);
    let row_status = "active";

    // Create as many rows as needed and put the first one as "active"
    for (let i = 0; i < num_rows; i++){
        if (i>0)
            row_status = "";

        // New gallery row
        let new_row=`
            <div class='carousel-item ${row_status}'>
                <div class='container'>
                    <div class='row' id='${type}-gallery-row-${i}'>
                    </div>
                </div>
            </div>`;

        // Append row to gallery
        $("#"+type+"Gallery").append(new_row);

        // Append books to the current row
        for (const book of url_list.slice(4*i, 4*(i+1))) {
            let isbn = book["isbn"];
            let url = book["url"];
            let new_element = `
                <div id='${type}-book-${isbn}' class="col-lg-3">
                    <div id='${isbn}' class="card img-container book-container" data-isbn="${isbn}"> 
                       <img src='${url}' alt='Book cover image for ${isbn}' class='card-img-top book-img'>
                    </div>
                </div>`
            $(`#${type}-gallery-row-${i}`).last().append(new_element);
        }
    }

    // Details handler
    $(".book-container").on(
        'click', (event) => show_detail(event, CALLBACK_OBJ)
    )
}

// Callback function in case of remove from wishlist or cancel reservation
function remove_from_carousel(type,isbn){
    let list_books_elems = $("[id^="+type+"-book]");
    let book_elem = $("#"+type+"-book-"+isbn);
    let index = list_books_elems.index(book_elem);

    let last_index = list_books_elems.length-1;
    if(index === last_index)
        // If it's the last book of the carousel, just remove it
        $(book_elem).remove();
    else
        // Otherwise replace it with the last one
        $(list_books_elems[index]).replaceWith($(list_books_elems[last_index]));

    // If the last row is empty, remove it
    let last_carousel_item = $("#"+type+"Gallery").children().last();
    if(last_carousel_item.find(".row").children().length === 0) {
        if ($(last_carousel_item).hasClass("active"))
            $("#"+type+"Gallery").children().first().addClass("active");
        last_carousel_item.remove();
    }
}

//Callback function in case of insert into wishlist or reservation
function insert_into_carousel(new_type,isbn){
    let old_type = (new_type === "reservations")?"wishlist":"reservations";
    let book_elem=$("#"+old_type+"-book-"+isbn).clone();
    let new_gallery= $("#"+new_type+"Gallery");
    let new_gallery_num_item = $(new_gallery).children().length;

    $(book_elem).attr("id", new_type+"-book-"+isbn);

    // Take last carousel-item of the new_type gallery
    let last_carousel_item = null;
    if( new_gallery_num_item > 0)
        last_carousel_item = $(new_gallery).children().last();

    // If it is full => create new one and append to gallery
    if( last_carousel_item===null || last_carousel_item.find(".row").children().length === 4){
        let row_status = (last_carousel_item===null)?"active":"";
        let new_row=`
            <div class='carousel-item ${row_status}'>
                <div class='container'>
                    <div class='row' id='${new_type}-gallery-row-${new_gallery_num_item}'>
                    </div>
                </div>
            </div>`;
        $(new_gallery).append(new_row);
        last_carousel_item = $(new_gallery).children().last();
    }

    // Append book to last carousel item and define details handler
    book_elem.find(".book-container").on(
        'click', (event) => show_detail(event, CALLBACK_OBJ)
    )
    book_elem.appendTo($(last_carousel_item.find(".row")));
}