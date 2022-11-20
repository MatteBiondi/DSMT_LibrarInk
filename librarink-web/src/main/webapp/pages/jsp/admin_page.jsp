<%@ page import="it.unipi.dsmt.librarink.LoanDTO" %>
<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.librarink.ReservationDTO" %>
<%@ page import="java.util.Iterator" %><%--
  Created by IntelliJ IDEA.
  User: tummi
  Date: 24/09/2022
  Time: 10:00
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<!DOCTYPE html>
<html>
<head>
    <style>
        table, th, td {
            border: 1px solid black;
            border-collapse: collapse;
        }
    </style>
    <title>admin_page</title>
</head>
<%
    List<LoanDTO> listLoan = (List<LoanDTO>) request.getAttribute("loanList");
    Iterator<LoanDTO> loanDTOIterator=listLoan.iterator();

    List<ReservationDTO> listReservation = (List<ReservationDTO>) request.getAttribute("reservationList");
    Iterator<ReservationDTO> reservationDTOIterator=listReservation.iterator();
    
%>
<body>



<form ACTION="<%= request.getContextPath()%>/admin" id="reservation">
    <table style="width:100%" id="reservation_table">

        <tr>
            <th colspan="5">Pending Reservation</th>

        </tr>
        <tr>
            <td>ck</td>
            <td>ISBN</td>
            <td>User ID</td>
            <td>Start Time</td>
            <td>End Time</td>
            <td>Book ID</td>

        </tr>
        <%
            while(loanDTOIterator.hasNext() ||reservationDTOIterator.hasNext()){%>
        <tr>
            <%
                if(reservationDTOIterator.hasNext()){
                    ReservationDTO reservationDTO=reservationDTOIterator.next();%>
            <td><input type="checkbox" name = "reservation" class="reservation_checkbox" value=<%=
            reservationDTO.getUser()+";"+reservationDTO.getIsbn()+";"+reservationDTO.getStartDate()%> /></td>
                <td><%=reservationDTO.getIsbn()%></td>
                <td><%=reservationDTO.getUser()%></td>
                <td><%=reservationDTO.getStartDate()%></td>
                <td><%=reservationDTO.getStopDate()%></td>
                <td><label for="<%="reservation"+reservationDTO.getUser()+reservationDTO.getIsbn()%>">First name:</label>
                    <input type="text" id="<%="reservation"+reservationDTO.getUser()+reservationDTO.getIsbn()%>" name="bookID"><br><br></td>
            <%}%>

            <%if(loanDTOIterator.hasNext()){
            loanDTOIterator.next();}%>
        </tr>
        <%}%>
    </table>
</form>
<form ACTION="<%= request.getContextPath()%>/admin" id="loan">
    <table style="width:100%"id="loan_table">
        <tr>
            <th colspan="6">Active Loan</th>
        </tr>
        <tr>
            <td>ck</td>
            <td>Book ID</td>
            <td>ISBN</td>
            <td>User ID</td>
            <td>Start Time</td>
            <td>End Time</td>
        </tr>
        <%while(loanDTOIterator.hasNext() ||reservationDTOIterator.hasNext()){%>
        <tr>
            <%if(reservationDTOIterator.hasNext()){
                reservationDTOIterator.next();}%>
            <%if(loanDTOIterator.hasNext()){
                LoanDTO loanDTO=loanDTOIterator.next();%>
            <td><input type="checkbox" name = "loan"  class="loan_checkbox" value=<%=loanDTO.getIsbn()+";"+loanDTO.getId()+";"+loanDTO.getUser()+";"%> /></td>
            <td><%=loanDTO.getId()%></td>
            <td><%=loanDTO.getIsbn()%></td>
            <td><%=loanDTO.getUser()%></td>
            <td><%=loanDTO.getStartDate()%></td>
            <td><%=loanDTO.getStopDate()%></td>
            <%}%>

        </tr>
        <%}%>
    </table>
</form>
<button type="submit" form="reservation" name="button" value="ConfirmReservation"onclick="submitRequest('reservationTable','ConfirmReservation','reservation_table','reservation')">Confirm Reservation</button>
<button type="submit" form="reservation" name="button" value="DeleteReservation" onclick="submitRequest('reservationTable','DeleteReservation','reservation_table','reservation')">Delete Reservation</button>
<button type="submit" form="loan" name="button" value="EndLoan" onclick="submitRequest('loanTable','EndLoan','loan_table','loan')">End Loan</button>
<a href="<%= request.getContextPath()%>/adminHistory">History table</a>
<button type="">NewLoan</button>
<script>

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
            let loanList = await $.post("<%= request.getContextPath()%>/admin", {
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
</script>
</body>
</html>
