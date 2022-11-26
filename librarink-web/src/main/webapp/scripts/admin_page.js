

    function myDeleteFunction(tableName,formName,checkBoxName) {
    let formElementHTMLCollectionOfElement=document.forms[formName];
    let allOption=formElementHTMLCollectionOfElement.elements[checkBoxName];
    let selectedOptions=[];
    allOption.forEach((element)=>{
    if (element.checked) {
    selectedOptions.push(element.value);
    element.parent().parent().remove();//with the first .parent()
    // i get the <td> element with the second element i get the <tr> element
}

});

}

    //submit a request and update a table
    async function submitRequest(formName,button,tableName,checkboxName)
    {

        let reservation = '';
        let loan = '';
        let sap = '';
        if(formName=="reservation"){
        $( ".reservation_checkbox" ).each(function() {
        if($( this ).is(':checked')){
        let listOfParam=[];
        listOfParam=$(this).val().split(";");
        $(this).value=$(this).val() + ";" +';'+document.getElementById('reservation'+listOfParam[0]+listOfParam[1]);
        reservation = reservation+''+sap+''+$( this ).val();
        sap = ',';
    }

    });
        let loanList = await $.post("<%= request.getContextPath()%>/admin", {
        button: button,
        reservation: reservation
    })
        let loanListJson = JSON.parse(loanList);

        for (let i = 0; i < reservationListJson.length; i++)
        myCreateFunctionSingleElement("loan_table",loanListJson[i],"loan");
    }
        else
    {
        $( '.loan_checkbox' ).each(function() {
        if($( this ).is(':checked')){
        loan = loan+''+sap+''+$( this ).val();
        sap = ',';
    }

    });
        let loanList = await $.post("/admin", {
        button: button,
        loan: loan }
        )
        /*let loanListJson = JSON.parse(loanList);

        for (let i = 0; i < loanListJson.length; i++)
            myCreateFunctionSingleElement("loan_table",loanListJson[i],"loan");*/
    }
        //Delete the row updated.
        myDeleteFunction(tableName,formName,checkboxName);

    }
    async function menuListId(inputField,datalist,Isbn)
    {
        let listOfId=requestID(Isbn);
        populateList(inputField,datalist,listOfId);

    }
    async function requestID(Isbn) {
    let idList = await $.post("request/async", {
    request: "available_copy_ids",
    isbn: Isbn })
    let jsonObj = JSON.parse('[' + idList + ']');
    let listdata=[];
    for (let i=0;i<jsonObj.length();i++){

    //Adding each element of JSON array into ArrayList
    listdata.add(jsonObj.get(i).id);//id è il valore?
}
    return listdata;
}
    function populateList(inputField,datalist,bookIdArray)
    {
        const bookIDInput=document.getElementById(inputField);
        const bookIdDataList=document.getElementById(datalist);
        bookIdArray.forEach(bookIdElement=>{
        let option = document.createElement("option");
        option.innerHTML = bookIdElement;
        bookIdDataList.appendChild(option);
    }
        );

    }
    //add a single element at the table
    function myCreateFunctionSingleElement(nameTable,value,typeValue) {
    let table = document.getElementById(nameTable);
    let element = value;


    if(typeValue=="reservation") {
    let row = table.insertRow(0);
    let checkbox = row.insertCell(0);
    let ISBN = row.insertCell(1);
    let UserID = row.insertCell(2);
    let StartTime = row.insertCell(3);
    let EndTime = row.insertCell(4);
    checkbox.innerHTML="<input type='checkbox' name = 'reservation' value="+element.user+"+';'+"+element.isbn+"+';'+"+element.startDate+"+';'/>"
    ISBN.innerHTML = element.isbn;
    ISBN.innerHTML = element.isbn;
    UserID.innerHTML = element.user;
    StartTime.innerHTML = element.startDate;
    EndTime.innerHTML = element.stopDate;
}
    else{
    let row = table.insertRow(0);
    let checkbox = row.insertCell(0);
    let ISBN = row.insertCell(2);
    let Id = row.insertCell(1);
    let UserID = row.insertCell(3);
    let StartTime = row.insertCell(4);
    let EndTime = row.insertCell(5);
    checkbox.innerHTML="<input type='checkbox' name = 'loan' value="+element.isbn+"+';'+"+element.id+"+';'+"+element.user+"+';'+loanIndex />"
    ISBN.innerHTML = element.isbn;
    UserID.innerHTML = element.user;
    Id.innerHTML = element.id;
    StartTime.innerHTML = element.startDate;
    EndTime.innerHTML = element.stopDate;
}

}
