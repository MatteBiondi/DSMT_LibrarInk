$(document).ready(() => {
        put_carousel("wishlist");
        put_carousel("reservations");
        load_books_image("wishlist");
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

    let callback_obj = {
        remove_from_res : remove_from_carousel.bind(null, "reservations"),
        remove_from_wish: remove_from_carousel.bind(null, "wishlist"),
        insert_into_res : insert_into_carousel.bind(null, "reservations"),
        insert_into_wish: insert_into_carousel.bind(null, "wishlist")
    }

    $(".book-container").on(
        'click', (event) => show_detail(event, callback_obj)
    )
}

//Callback in case of remove from wishlist or cancel reservation
function remove_from_carousel(type,isbn){
    /*
    let list_books_elems = $("[id^="+type+"-book]");
    let book_elem = $("#"+type+"-"+isbn);
    let index = list_books_elems.index(book_elem);

    for (let i = index; i<list_books_elems.length - 1; i++){
        list_books_elems[i].replaceWith(list_books_elems[i+1]);
    }

    if($("#"+type+"Gallery").lastChild.find(".row").empty())
        $("#"+type+"Gallery").lastChild.remove();
    */
    console.log("Removed");
}

//Callback in case of insert into wishlist or reservation
function insert_into_carousel(type,isbn){
    //sposto da un posto all'altro
    console.log("Inserted");
}