let current_page;

$(document).ready(() => {
    // Tabs
    current_page = sessionStorage.getItem("history-page");

    let nav_reservation_tab_btn = $("#nav-history-reservation-tab");
    let nav_history_loan_tab_btn = $("#nav-history-loan-tab");

    const nav_history_loan_tab = new bootstrap.Tab('#nav-history-loan-tab');

    if(current_page != null && current_page === "loan"){
        nav_history_loan_tab.show();
        nav_history_loan_tab_btn.blur()
    }

    //Adjust columns size
    $('button[data-bs-toggle="tab"]').on('shown.bs.tab', () =>
        $($.fn.dataTable.tables(true)).DataTable().columns.adjust()
    );

    nav_reservation_tab_btn.on("click",() => sessionStorage.setItem("history-page", "reservation"));
    nav_history_loan_tab_btn.on("click",() => sessionStorage.setItem("history-page", "loan"));

    $('#history-reservation-table').DataTable({
        scrollCollapse: true,
        scrollY: '75%',
        order: [[2, 'desc']],
    });

    $('#history-loan-table').DataTable({
        scrollCollapse: true,
        scrollY: '75%',
        order: [[3, 'desc']],
    });
})