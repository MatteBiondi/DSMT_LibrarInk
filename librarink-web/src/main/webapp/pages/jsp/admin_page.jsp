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


<%--@declare id="reservation"--%><%--@declare id="loan"--%>
<form ACTION="src/main/java/it/unipi/dsmt/servlet/AdminPageServlet.java" id="reservation">
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

        </tr>
        <%  int indexReservation=0;
            while(loanDTOIterator.hasNext() ||reservationDTOIterator.hasNext()){%>
        <tr>
            <%
                if(reservationDTOIterator.hasNext()){
                    ++indexReservation;
                    ReservationDTO reservationDTO=reservationDTOIterator.next();%>
            <td><input type="checkbox" name = "reservation" value=<%=
            reservationDTO.getUser()+";"+reservationDTO.getIsbn()+";"+reservationDTO.getStartDate()+";"+indexReservation%> /></td>
                <td><%=reservationDTO.getIsbn()%></td>
                <td><%=reservationDTO.getUser()%></td>
                <td><%=reservationDTO.getStartDate()%></td>
                <td><%=reservationDTO.getStopDate()%></td>
            <%}%>

            <%if(loanDTOIterator.hasNext()){
            loanDTOIterator.next();}%>
        </tr>
        <%}%>
    </table>
</form>
<form ACTION="src/main/java/it/unipi/dsmt/servlet/AdminPageServlet.java" id="loan">
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
        <% int loanIndex=0;
            while(loanDTOIterator.hasNext() ||reservationDTOIterator.hasNext()){%>
        <tr>
            <%if(reservationDTOIterator.hasNext()){
                ++loanIndex;
                reservationDTOIterator.next();}%>
            <%if(loanDTOIterator.hasNext()){
                LoanDTO loanDTO=loanDTOIterator.next();%>
            <td><input type="checkbox" name = "loan"  value=<%=loanDTO.getIsbn()+";"+loanDTO.getId()+";"+loanDTO.getUser()+";"+loanIndex%> /></td>
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
<button type="submit" form="reservation" name="button" value="ConfirmReservation"onclick="">Confirm Reservation</button>
<button type="submit" form="reservation" name="button" value="DeleteReservation" onclick="myDeleteFunction('reservationTable','reservation','reservation')">Delete Reservation</button>
<button type="submit" form="loan" name="button" value="EndLoan">End Loan</button>
<a href="default.asp" target="librarink-web/src/main/java/it/unipi/dsmt/servlet/AdminPageHistoryServlet.java">History table</a>
<button type="">NewLoan</button>
<script>
    function myDeleteFunction(tableName,formName,checkBoxName) {
        var formElementHTMLCollectionOfElement=document.forms[formName];
        var allOption=formElementHTMLCollectionOfElement.elements[checkBoxName];
        var selectedOptions=[];
        var indexes=[];
        allOption.forEach((element)=>{
            if (element.checked) {
                selectedOptions.push(element.value);
            }

        });
        selectedOptions.forEach((element)=>{
            indexes.push(element.split(";").pop())
        });
        let i;
        for(i = 0; i<indexes.length; i++)
        {
            document.getElementById(tableName).deleteRow(i);
        }

    }
    //toDO update loanTable
</script>
</body>
</html>
