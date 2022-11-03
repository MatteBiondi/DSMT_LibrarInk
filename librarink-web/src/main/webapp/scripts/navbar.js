$(document).ready(() => {
    $("#header").on('click', () => {
        sessionStorage.removeItem("page");
        sessionStorage.removeItem("search");
        sessionStorage.removeItem("keyword");
    })
})