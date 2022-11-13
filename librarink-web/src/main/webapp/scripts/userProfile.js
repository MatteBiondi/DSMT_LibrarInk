callback_obj = {
    remove_from_res : remove_from_carousel.bind(null, "reservations"),
    remove_from_wish: remove_from_carousel.bind(null, "wishlist"),
    insert_into_res : insert_into_carousel.bind(null, "reservations"),
    insert_into_wish: insert_into_carousel.bind(null, "wishlist")
}

$(document).ready(() => {
        put_carousel("wishlist");
        put_carousel("loans");
        put_carousel("reservations");
        load_books_image("wishlist");
        load_books_image("loans");
        load_books_image("reservations");
    }
)

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
                <button
                        class="carousel-control-prev position-relative"
                        type="button"
                        data-bs-target="#${type}CarouselMultiItem"
                        data-bs-slide="prev"
                >
                    <span class="carousel-control-prev-icon" aria-hidden="true"></span>
                    <span class="visually-hidden">Previous</span>
                </button>
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
            <!-- Inner -->
            <div class="carousel-inner py-4" id="${type}Gallery">
                <!-- FILLED BY AJAX REQUEST -->
            </div>
            <!-- Inner -->
        </div>
        <!-- Carousel wrapper -->
    `;
    $("#"+type+"Row").append(carousel);
}

async function load_books_image(type) {
    let isbn_list = [];
    if (type === "reservations")
        isbn_list = await load_local_reservations();
    else if (type === "wishlist")
        isbn_list = await load_local_wishlist();
    else if (type === "loans"){
        let lent_books = await load_local_loans();
        isbn_list = lent_books.map((book) => book.isbn);
    }

    let images_url_array = [];
    for (const book_isbn of isbn_list){
        let image_url = await $.post(
            "request/async",
            {
                "request": "load_image_url",
                "isbn": book_isbn
            }, "json")
        images_url_array.push(image_url);
    }

    gui_book_elements(images_url_array, type);
}

function gui_book_elements(url_list, type) {
    console.log(url_list);
    let num_rows = Math.ceil(url_list.length/4);
    let row_status = "active";
    for (let i = 0; i < num_rows; i++){
        //new gallery row
        if (i>0)
            row_status = "";

        let new_row=`
            <div class='carousel-item ${row_status}'>
                <div class='container'>
                    <div class='row' id='${type}-gallery-row-${i}'>
                    </div>
                </div>
            </div>`;
        $("#"+type+"Gallery").append(new_row);

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

    $(".book-container").on(
        'click', (event) => show_detail(event, callback_obj)
    )
}

//Callback in case of remove from wishlist or cancel reservation
function remove_from_carousel(type,isbn){
    let list_books_elems = $("[id^="+type+"-book]");
    let book_elem = $("#"+type+"-book-"+isbn);
    let index = list_books_elems.index(book_elem);

    let last_index = list_books_elems.length-1;
    if(index === last_index)
        $(book_elem).remove();
    else
       $(list_books_elems[index]).replaceWith($(list_books_elems[last_index]));

    let last_carousel_item = $("#"+type+"Gallery").children().last();

    if(last_carousel_item.find(".row").children().length === 0) {
        if ($(last_carousel_item).hasClass("active"))
            $("#"+type+"Gallery").children().first().addClass("active");
        last_carousel_item.remove();
    }
}

//Callback in case of insert into wishlist or reservation
function insert_into_carousel(new_type,isbn){
    let old_type = (new_type === "reservations")?"wishlist":"reservations";
    let book_elem=$("#"+old_type+"-book-"+isbn).clone();
    let new_gallery= $("#"+new_type+"Gallery");
    let new_gallery_num_item = $(new_gallery).children().length;

    $(book_elem).attr("id", new_type+"-book-"+isbn);

    //take last carousel-item of the new_type gallery
    let last_carousel_item = null;
    if( new_gallery_num_item > 0)
        last_carousel_item = $(new_gallery).children().last();
    //if it is full => create new one and append to gallery
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
    // append book to last carousel item
    book_elem.find(".book-container").on(
        'click', (event) => show_detail(event, callback_obj)
    )
    book_elem.appendTo($(last_carousel_item.find(".row")));
}