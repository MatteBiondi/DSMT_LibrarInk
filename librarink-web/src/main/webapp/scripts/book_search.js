// Code in charge of searching books and load their content on the page

// *********** Constant section ***********
const DEFAULT_SEARCH = "Title";
const DEFAULT_KEYWORD = "";
// ****************************************

// Code on document ready
$(document).ready(() => {
    // Get search element of the page
    let search_items = $(".search-item");
    let search_text = $("#search-text");
    let search_clear = $("#search-clear");

    document.documentElement.style.scrollBehavior = 'auto';

    // Get information stored on session
    let search = sessionStorage.getItem("search");
    let keyword =  sessionStorage.getItem("keyword");

    // Init session items
    if(search == null || keyword == null){
        sessionStorage.setItem("search", DEFAULT_SEARCH)
        sessionStorage.setItem("keyword", DEFAULT_KEYWORD)
        search_text.attr("placeholder",`${DEFAULT_SEARCH}:`)
        search_text.val(DEFAULT_KEYWORD)
    }
    else {// Reload session items
        search_text.attr("placeholder",`${search}:`)
        search_text.val(keyword)
    }


    // Define event handlers
    search_items.on(
        "click",
        (elem) => {
            search_text.attr("placeholder", `${elem.currentTarget.innerText}:`);
            sessionStorage.setItem("search", elem.currentTarget.innerText);
            search_input();
        });

    search_clear.on(
        "click",
        () => {
            search_text.val("");
            sessionStorage.setItem("keyword", "");
            load_books(sessionStorage.getItem("search"), "", 1).catch(
                () => show_message("danger", "Something went wrong")
            )
        }
    );

    search_text.on(
        "input",
        () => search_input()
    );

    // Async load book list
    load_books(
        sessionStorage.getItem("search"),
        sessionStorage.getItem("keyword"),
        sessionStorage.getItem("page")
    ).catch(() => show_message("danger", "Something went wrong"))

});

// *********** Functions section ***********

// This function is in charge of loading in the homepage the list of books thumbnails for the books
// that result from searching action
async function load_books(search, keyword, page){
    // If text is different from "", then shows the "x" to clear search
    $("#search-clear").css("display", $("#search-text").val() !== "" ? "inherit":"none");

    try{
        // Perform search request and get the resulted list of book
        let book_list = await $.get(
            `homepage?search=${search}&keyword=${keyword}&page=${page}`,
            'text/html'
        );

        // Update html elements
        $("#book-list").html(book_list);
        $(".page-link").on(
            'click', (event) => {
                sessionStorage.setItem("page", event.currentTarget.getAttribute('data-offset'));
                load_books(
                    sessionStorage.getItem("search"),
                    sessionStorage.getItem("keyword"),
                    event.currentTarget.getAttribute('data-offset')
                ).then(() => window.scrollTo(0, 0));
            }
        )

        // Details handler
        $(".thumbnail").on("click",(elem) => show_detail(elem));
    }
    catch (e){
        show_message("danger", "Something went wrong");
    }
}

// This function is in charge of perform search operation by means of load_books function in case of text
// typing or click on search item
function search_input(){
    let search_text = $("#search-text")

    // Removes events handlers
    search_text.off("input");

    // Save value on session storage
    sessionStorage.setItem("keyword", search_text.val());

    // Load new books list and insert again events handlers
    load_books(sessionStorage.getItem("search"), search_text.val(), 1)
        .then(
            () => {},
            () => alert("Something went wrong")
        )
        .finally(() => search_text.on("input", search_input))
}
