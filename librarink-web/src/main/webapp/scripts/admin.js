let popup_page;

$(document).ready(() => {
    popup_page = new bootstrap.Modal("#popup")
    $("#add-book-copy").on("click", async () => show_popup(await $.get("adminAddBookCopy")));
    $("#add-new-loan").on("click", async () => show_popup(await $.get("adminAddLoan")));
})

// Utilities
function show_popup(page){
    $("#popup-body").html(page);
    popup_page.show();
    let autocomplete_inputs = $('.basicAutoComplete');
    autocomplete_inputs.autoComplete();
    autocomplete_inputs.on("input", (ev) => {
        if( $(ev.target).val().length < 3)
            return;
        let hints = $(ev.target).siblings(".bootstrap-autocomplete");
        if(hints.length === 1){
           $(hints[0]).css("display", "block");
           $(hints[0]).on(
                "click",
                (ev) =>$(ev.target).closest("ul").css("display", "none")
            );
        }
    });
    $(".modal-content").on("click", () => $(".bootstrap-autocomplete").css("display", "none"));
    $("#add-loan-isbn").on("blur",(ev) =>  menuSelectListId("add-loan-id", $(ev.target).val()))
    $("#add-book-isbn").on("blur",(ev) =>  menuSelectListId("add-book-id", $(ev.target).val()))
    $(".add-book-opt").on("change", (ev) => {
        $("#add-book-id").prop("disabled", $(ev.target).val() === "add")
    })
}

function show_message(type, text){
    $(".wrapper").prepend(`
        <div class="alert alert-${type} alert-dismissible fade show" role="alert">
            <div>${text}</div>
            <button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>
        </div>`
    );

    let alert_elem = $('.alert');
    setTimeout(() => {alert_elem.alert("close"); alert_elem.parent().remove(alert_elem)}, 2000);
}

async function menuSelectListId(selectID, Isbn) {
    let listOfId = await requestID(Isbn);
    deleteAllChildOfAnElement(selectID);
    populateOptionListId(selectID,listOfId);
}

async function requestID(Isbn) {
    return await $.post(
        "request/async",
        {
            request: "available_copy_ids",
            isbn: Isbn
        },
        "json"
    );
}

function deleteAllChildOfAnElement(idElement) {
    const myNode = document.getElementById(idElement);
    while (myNode.firstChild) {
        myNode.removeChild(myNode.lastChild);
    }
}

function populateOptionListId(selectID,listOfId) {
    let menuElement=document.getElementById(selectID);
    for(let i=0;i<listOfId.length;i++) {
        let option = document.createElement("option");
        option.value = listOfId[i];
        option.text = listOfId[i];
        menuElement.add(option);
    }
}

// Add/Remove book copies
async function submitAddDeleteCopyBookPage(isbn_ID,bookID_ID,radioBox_ID) {
    let radioBoxValue=document.querySelector('input[name="'+radioBox_ID+'"]:checked').value;
    if(radioBoxValue==="add") {
        await submitAddCopyBookPage(isbn_ID);
    }
    else if(radioBoxValue==="remove") {
        await submitDeleteCopyBookPage(isbn_ID, bookID_ID);
    }
    else {
        show_message("danger", "Something went wrong");
    }
}

async function submitAddCopyBookPage(isbn_ID) {
    let isbn = document.getElementById(isbn_ID).value;
    if (isbn === '') {
        show_message("danger","One or more fields are empty");
    }
    else {
        let message = await $.post("request/async", {
            request: "write_copy",
            isbn: isbn },"json"
        );

        if(message.hasOwnProperty("response") && message.response==='ok') {
            show_message("success","New copy added");
        }
        else{
            show_message("danger", "Something went wrong");
        }
        popup_page.hide();
    }
}

async function submitDeleteCopyBookPage(isbn_ID,bookID_ID) {
    let isbn = document.getElementById(isbn_ID).value;
    let bookID = document.getElementById(bookID_ID).value;
    if (isbn === '' || bookID === '') {
        show_message("danger","One or more fields are empty");
    } else {
        let message = await $.post("request/async", {
                request: "delete_copy",
                isbn: isbn,
                id: bookID
            }, "json"
        );
        if(message.hasOwnProperty("response") && message.response==='ok') {
            show_message("success","Copy removed");
        }
        else{
            show_message("danger", "Something went wrong");
        }
        popup_page.hide();
    }
}

//Add loan manually
async function submitAddLoanPage(isbn_ID,userEmail_ID,bookID_ID) {
    let isbn=document.getElementById(isbn_ID).value;
    let userEmail=document.getElementById(userEmail_ID).value;
    let bookID=document.getElementById(bookID_ID).value;

    if(isbn === '' || userEmail === ''|| bookID === '') {
        show_message("danger","One or more fields are empty");
    }
    else {
        try{
            let response =
                await $.post("/librarink-web/adminAddLoan", {
                    User: userEmail,
                    ISBN: isbn,
                    IDBook:bookID
                }, "json");
            show_message(response["result"]==="success"?"success":"danger", response["response"])

            if(response["result"] === "success"){
                myCreateFunctionSingleElement("loan_table", response["loan"]);
            }
        }
        catch (e) {
            show_message("danger", "Something went wrong")
        }
        popup_page.hide();
    }
}

function myCreateFunctionSingleElement(tableName, element) {
    let value = `${element["isbn"]};${element["id"]};${element["user"]}`
    let checkbox = "<input type='checkbox' name='loan' class='loan_checkbox' value='" + value + "'/>"
    $(`#${tableName}`).DataTable().row.add([
        checkbox,
        element["id"],
        element["isbn"],
        element["user"],
        buildDate(new Date(element["start_date"]))
    ]).draw()
}

function myDeleteFunction(tableName) {
    if(tableName === "reservation_table") {
        $( ".reservation_checkbox" ).each(function(){
            if($(this).is(':checked')) {
                $(this).closest('tr')[0].id = "remove";
                $(`#${tableName}`).DataTable().row("#remove").remove().draw(false);
            }
        });
    }
    else if(tableName === "loan_table") {
        $( ".loan_checkbox" ).each(function(){
            if($(this).is(':checked')) {
                $(this).closest('tr')[0].id = "remove";
                $(`#${tableName}`).DataTable().row("#remove").remove().draw(false);
            }
        });
    }
}