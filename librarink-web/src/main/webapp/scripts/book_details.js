
$(document).ready(() => {
    let book_isbn = $(".detailed")[0].id;
    let reserve_btn = $("#reserve-btn");
    let wishlist_btn = $("#wishlist-btn");

    // TODO: disable button if already reserved or no copies available

    if(available_copies() > 0)
        reserve_btn.on("click", () => reserve())
    else
        reserve_btn.prop('disabled', true);

    wishlist_btn.on("click", () => add_wishlist())

});

async function reserve(){
    let result = await $.post(
        `request/async`,
        {"request":"write_reservation","isbn": $(".detailed")[0].id},
    );

    console.log(result);

}

async function add_wishlist(){
    let result = await $.post(
        `request/async`,
        {"request":"add_wishlist","isbn": $(".detailed")[0].id},
    );
    await track_books(); //TODO: update wishlist tracking

    console.log(result);
}

function available_copies(){
    let counter = parseInt($("#copies_counter").text());
    return isNaN(counter) ? 0:counter;
}