const DEFAULT_SEARCH = "Title";
const DEFAULT_KEYWORD = "";

$(document).ready(() => {
    let search_items = $(".search-item");
    let search_text = $("#search-text");
    let search_clear = $("#search-clear");
    let search;
    let keyword;

    search = sessionStorage.getItem("search");
    keyword =  sessionStorage.getItem("keyword");

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
        });

    search_clear.on(
        "click",
        () => {
            search_text.val("");
            sessionStorage.setItem("keyword", "");
            load_books(sessionStorage.getItem("search"), "", 1).then(
                () => {},
                () => alert("Something went wrong"))
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
    ).then(() => {}, () => alert("Something went wrong"))
});

async function load_books(search, keyword, page){
    $("#search-clear").css("display", $("#search-text").val() !== "" ? "inherit":"none");
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
            );
        }
    )
}

function search_input(){
    let search_text = $("#search-text")
    search_text.off("input");
    sessionStorage.setItem("keyword", search_text.val());
    load_books(sessionStorage.getItem("search"), search_text.val(), 1)
        .then(
            () => {},
            () => alert("Something went wrong")
        )
        .finally(() => search_text.on("input", search_input))
}
/*
* $(".dropdown-menu").append('<li><div class="toast" role="alert" aria-live="assertive" aria-atomic="true"><div class="toast-header"><strong class="me-auto">Bootstrap</strong><small>11 mins ago</small><button type="button" class="btn-close" data-bs-dismiss="toast" aria-label="Close"></button></div><div class="toast-body">Hello, world! This is a toast message.</div></div></li>')*/