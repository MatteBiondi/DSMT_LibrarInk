

function myDeleteFunction(tableName,formName,checkBoxName)
{
    if(tableName=="reservation_table")
    {
        $( ".reservation_checkbox" ).each(function(){
            if($( this ).is(':checked'))
            {
                $(this).closest('tr').remove();
            }
     });
    }
    else if(tableName=="loan_table")
    {
        $( ".loan_checkbox" ).each(function(){
            if($( this ).is(':checked'))
            {
                $(this).closest('tr').remove();
            }
        });
    }


}
function resetFormField(idForm)
{
    console.log(idForm);
    document.getElementById(idForm).reset();
}
async function submitAddCopyBookPage(isbn_ID)
{
    let isbn=document.getElementById(isbn_ID).value;
    let message = await $.post("request/async", {
        request: "write_copy",
        isbn: isbn },"json"
    );
    console.log(message);
    //TODO add a message
    document.getElementById(isbn_ID).value='';
    if(message.hasOwnProperty("response"))
    {
        if(message.response=='ok')
        {
            alert("the copy is added correctly");
        }
        else
        {
            alert("there is a problem with the request");
        }
    }



}
async function submitAddDeleteCopyBookPage(isbn_ID,bookID_ID,radioBox_ID)
{
    let radioBoxValue=document.querySelector('input[name="'+radioBox_ID+'"]:checked').value;
    if(radioBoxValue=="add")
    {
        await submitAddCopyBookPage(isbn_ID);
    }
    else if(radioBoxValue=="remove")
    {
        await submitDeleteCopyBookPage(isbn_ID, bookID_ID);
    }
    else
    {
        alert("request not implemented");
    }
}
async function submitDeleteCopyBookPage(isbn_ID,bookID_ID) {
    let isbn = document.getElementById(isbn_ID).value;
    let bookID = document.getElementById(bookID_ID).value;
    if (isbn == '' || bookID == '') {
        alert("one or more fields are empty, please fill them all");
    } else {
        let message = await $.post("request/async", {
                request: "delete_copy",
                isbn: isbn,
                id: bookID
            }, "json"
        );
        if (message.hasOwnProperty('result')) {
            alert("result: " + message.result, +" response: " + message.response);
            document.getElementById(isbn_ID).value = '';
            document.getElementById(bookID_ID).value = '';
        }
    }
}
async function submitAddLoanPage(isbn_ID,userEmail_ID,bookID_ID,idForm)
{
    let isbn=document.getElementById(isbn_ID).value;
    let userEmail=document.getElementById(userEmail_ID).value;
    let bookID=document.getElementById(bookID_ID).value;
    if(isbn==''||userEmail==''||bookID=='')
    {
        alert("one or more fields are empty, please fill them all");
    }
    else
    {
        let result= await $.post("/librarink-web/adminAddLoan", {
            User: userEmail,
            ISBN: isbn,
            IDBook:bookID
        },"json");
        let jsonResult=JSON.parse(result);
        if(jsonResult.hasOwnProperty('result')){
            alert("result: " +jsonResult.result,+" response: "+ jsonResult.response);
            document.getElementById(isbn_ID).value='';
            document.getElementById(userEmail_ID).value='';
            document.getElementById(bookID_ID).value='';
        }
    }

}
//submit a request and update a table
async function submitRequest(formName,button,tableName,checkboxName)
{

    let reservation = '';
    let loan = '';
    let sap = '';
    console.log(formName);
    if(formName=="reservation"){
        console.log("reservation path");
        $( ".reservation_checkbox" ).each(function() {
        if($( this ).is(':checked'))
        {
            let listOfParam=[];
            listOfParam=$(this).val().split(";");
            console.log(document.getElementById('reservation'+listOfParam[0]+listOfParam[1]).value);
            //$(this).value=$(this).val() + ";" +document.getElementById('reservation'+listOfParam[0]+listOfParam[1]).value;
            reservation = reservation+''+sap+''+$( this ).val()+";"+document.getElementById('reservation'+listOfParam[0]+listOfParam[1]).value;
            console.log(reservation);
            sap = ',';
        }

    });
        console.log("request:"+{
            button: button,
            reservation: reservation,
            separator:sap
        });
        let loanList = await $.post("/librarink-web/admin", {
        button: button,
        reservation: reservation,
        separator:sap
    },"json");
        if(button=="ConfirmReservation")
        {
            let loanListJson = JSON.parse(loanList);
            for (let i = 0; i < loanListJson.length; i++)
            {
                console.log("element to add:"+loanListJson[i]);
                myCreateFunctionSingleElement("loan_table",loanListJson[i],"loan");
            }
            if(loanListJson.hasOwnProperty('result')){
                alert("result:"+loanListJson.result,+"/n"+"response: "+ loanListJson.response);
            }


        }

}
    else
    {
        console.log("loan path");
        $( '.loan_checkbox' ).each(function()
        {
            if($( this ).is(':checked'))
            {
                loan = loan+''+sap+''+$( this ).val();
                sap = ',';
            }
        });
        let loanMessage = await $.post("/librarink-web/admin", {
            button: button,
            loan: loan,
            separator: sap},"json");
        let loanMessageJson = JSON.parse(loanMessage);
        alert("result:"+loanMessageJson.result,+"/n"+"response: "+ loanMessageJson.response);
    /*let loanListJson = JSON.parse(loanList);

    for (let i = 0; i < loanListJson.length; i++)
        myCreateFunctionSingleElement("loan_table",loanListJson[i],"loan");*/
}
    //Delete the row updated.
    myDeleteFunction(tableName,formName,checkboxName);

}
async function menuSelectListId(selectID,Isbn)
{
    let listOfId= await requestID(Isbn);
    deleteAllChildOfAnElement(selectID);
    populateOptionListId(selectID,listOfId);

}
function populateOptionListId(selectID,listOfId)
{
    let menuElement=document.getElementById(selectID);
    for(let i=0;i<listOfId.length;i++) {
        let option = document.createElement("option");
        option.text = listOfId[i];
        menuElement.add(option);
    }
}
async function menuListId(inputField,datalist,Isbn)
{
    let listOfId= await requestID(Isbn);
    deleteAllChildOfAnElement(datalist);
    populateList(inputField,datalist,listOfId);

}
async function requestID(Isbn) {
let idList = await $.post("request/async", {
    request: "available_copy_ids",
    isbn: Isbn },"json"
    )
    console.log(typeof idList);
    console.log(idList);
    return idList;
}
function getValue(inputIdField)
{
    console.log("ISBN: ");
    console.log("ISBN: "+document.getElementById(inputIdField).value);
    return document.getElementById(inputIdField).value;
}
function populateList(inputField,datalist,bookIdArray)
{
    const bookIDInput=document.getElementById(inputField);
    const bookIdDataList=document.getElementById(datalist);
    bookIdArray.forEach(bookIdElement=>{
        let option = document.createElement("option");
        option.innerHTML = bookIdElement;
        bookIdDataList.appendChild(option);

    });
}
function deleteAllChildOfAnElement(idElement)
{

    const myNode = document.getElementById(idElement);
    while (myNode.firstChild)
    {
        myNode.removeChild(myNode.lastChild);
    }

}
        //add a single element at the table
function myCreateFunctionSingleElement(nameTable,value,typeValue) {
    let table = document.getElementById(nameTable);
    let element = value;

    if(typeValue=="reservation")
    {

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
    else
    {
        let row = table.insertRow(2);
        let checkbox = row.insertCell(0);
        let Id = row.insertCell(1);
        let ISBN = row.insertCell(2);
        let UserID = row.insertCell(3);
        let StartTime = row.insertCell(4);
        let EndTime = row.insertCell(5);
        checkbox.innerHTML="<input type='checkbox' name = 'loan' value="+element.isbn+"+';'+"+element.id+"+';'+"+element.user+"+'; />"
        ISBN.innerHTML = element.isbn;
        UserID.innerHTML = element.user;
        Id.innerHTML = element.copyId;
        StartTime.innerHTML = new Date(element.startDate).toLocaleString();
        if(element.stopDate!=null) {
            EndTime.innerHTML = new Date(element.stopDate);
        }
        else
        {
            EndTime.innerHTML="null";
        }
    }

}
