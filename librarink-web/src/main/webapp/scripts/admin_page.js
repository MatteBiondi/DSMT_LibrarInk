
$(document).ready(() => {

    let current_page = sessionStorage.getItem("page");
    let nav_reservation_tab_btn = $("#nav-reservation-tab");
    let nav_loan_tab_btn = $("#nav-loan-tab");

    const nav_loan_tab = new bootstrap.Tab('#nav-loan-tab');

    // Select which buttons to show
    showButtons(current_page);

    // Load book IDs
    preloadId()

    // Select active tab
    if(current_page != null && current_page === "loan"){
        nav_loan_tab.show();
        nav_loan_tab_btn.blur()
    }

    //Adjust columns size
    $('button[data-bs-toggle="tab"]').on('shown.bs.tab', () =>
        $($.fn.dataTable.tables(true)).DataTable().columns.adjust()
    );

    // Set handlers
    nav_reservation_tab_btn.on("click",() => {
        sessionStorage.setItem("page", "reservation")
        showButtons("reservation")
    });

    nav_loan_tab_btn.on("click",() => {
        sessionStorage.setItem("page", "loan")
        showButtons("loan")
    });

    $('#reservation_table').DataTable({
        scrollY: '75%',
        scrollCollapse: true,
        paging: false,
        fixedHeader: true,
        ordering: false
    });

    $('#loan_table').DataTable({
        scrollY: '75%',
        scrollCollapse: true,
        paging: false,
        fixedHeader: true,
        ordering:false
    });

    $(".paginate_button").on("click", preloadId)
})

function preloadId(){
    $(".book-id-selection").each((pos, elem) => menuSelectListId(elem.id, elem.dataset.isbn));
}

// Confirm reservation\End loan
async function submitRequest(formName, button, tableName, checkboxName) {
    let reservation = '';
    let loan = '';
    let sep = '';
    let response;

    if(formName === "reservation"){// Confirm/Delete reservation
        $(".reservation_checkbox").each(function() {
            if($(this).is(':checked')) {
                let listOfParam;
                listOfParam=$(this).val().split(";");
                if(document.getElementById('reservation' + listOfParam[0] + listOfParam[1]).value === '') {
                    show_message("danger","Select book ID");
                    return;
                }
                reservation = reservation + sep +
                    $( this ).val() + ";" +
                    document.getElementById('reservation'+listOfParam[0]+listOfParam[1]).value;
                sep = ',';
            }
        });
        if(reservation === ''){
            show_message("danger", "No reservation selected");
            return;
        }
        // Send async request
        response = await $.post("/librarink-web/admin",
            {
                button: button,
                reservation: reservation,
                separator:sep
            },
            "json"
        );
        //Update tables if necessary
        if(button === "ConfirmReservation") {
            if (response["result"] === "success"){
                for (loan of response["loans"] ){
                    myCreateFunctionSingleElement("loan_table", loan);
                }
            }
        }
    }
    else { // End loan
        $('.loan_checkbox').each(function() {
            if($( this ).is(':checked')) {
                loan = loan + sep + $(this).val();
                sep = ',';
            }
        });

        if(loan === ''){
            show_message("danger", "No loan selected");
            return;
        }

        response = await $.post("/librarink-web/admin",
            {
                button: button,
                loan: loan,
                separator: sep
            },
            "json"
        );

    }
    if(response["result"] === "success"){  // Update target table
        myDeleteFunction(tableName, formName, checkboxName);
    }

    // Show message
    show_message(response["result"] === "success" ? "success":"danger", response["response"]);
}

// Utilities
function getValue(inputIdField){
    return document.getElementById(inputIdField).value;
}

function showButtons(page){
    let confirm_reservation = $("#confirm-reservation-btn")
    let delete_reservation = $("#delete-reservation-btn")
    let end_loan = $("#end-loan-btn")

    if(page != null && page === "loan"){
        confirm_reservation.attr("hidden","true")
        delete_reservation.attr("hidden","true")
        end_loan.removeAttr("hidden")
    }
    else {
        confirm_reservation.removeAttr("hidden")
        delete_reservation.removeAttr("hidden")
        end_loan.attr("hidden","true")
    }
}

function buildDate(date){
    let mm = date.getMonth() + 1; // getMonth() is zero-based
    let dd = date.getDate();
    let hh = date.getHours();
    let MM = date.getMinutes();
    let ss = date.getSeconds();

    return [
        date.getFullYear() + ' ',
        (mm>9 ? '' : '0') + mm + '-',
        (dd>9 ? '' : '0') + dd,
        ' ',
        (hh>9 ? '':'0') + hh + ':',
        (MM>9 ? '':'0') + MM + ':',
        (ss>9 ? '':'0') + ss
    ].join('');
}
