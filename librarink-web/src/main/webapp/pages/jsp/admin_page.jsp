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
</head>
<%
    List<LoanDTO> listLoan = (List<LoanDTO>) request.getAttribute("loanList");
    Iterator<LoanDTO> loanDTOIterator=listLoan.iterator();

    List<ReservationDTO> listReservation = (List<ReservationDTO>) request.getAttribute("reservationList");
    Iterator<ReservationDTO> reservationDTOIterator=listReservation.iterator();
%>
<body>


<%--@declare id="reservation"--%><%--@declare id="loan"--%><form ACTION="",id="reservation">
    <table style="width:100%">
        <tr>
            <th colspan="5">Pending Reservation</th>
            <th colspan="5">Active Loan</th>
        </tr>
        <tr>
            <td>ck</td>
            <td>ISBN</td>
            <td>User ID</td>
            <td>Start Time</td>
            <td>End Time</td>
            <td>ck</td>
        </tr>
        <%  while(loanDTOIterator.hasNext() ||reservationDTOIterator.hasNext()){%>
        <tr>
            <%if(reservationDTOIterator.hasNext()){

            ReservationDTO reservationDTO=reservationDTOIterator.next();%>
            <td><input type="checkbox" name = "reservation" value=<%=
            reservationDTO.getUser()+reservationDTO.getIsbn()+reservationDTO.getStartDate()%> /></td>
                <td><%=reservationDTO.getIsbn()%></td>
                <td><%=reservationDTO.getUser()%></td>
                <td><%=reservationDTO.getStartDate()%></td>
                <td><%=reservationDTO.getStopDate()%></td>
            <%}
            else{%>
                <td></td>
                <td></td>
                <td></td>
                <td></td>
                <td></td>
            <%}%>
            <%if(loanDTOIterator.hasNext()){
            loanDTOIterator.next();}%>
        </tr>
        <%}%>
    </table>
</form>
<form ACTION="",id="loan">
    <table style="width:100%">
        <tr>
            <th colspan="5">Pending Reservation</th>
            <th colspan="5">Active Loan</th>
        </tr>
        <tr>

            <td>Loan ID</td>
            <td>ISBN</td>
            <td>User ID</td>
            <td>Start Time</td>
            <td>End Time</td>
        </tr>
        <%  while(loanDTOIterator.hasNext() ||reservationDTOIterator.hasNext()){%>
        <tr>
            <%if(reservationDTOIterator.hasNext()){

                reservationDTOIterator.next();}%>
            <%if(loanDTOIterator.hasNext()){
                LoanDTO loanDTO=loanDTOIterator.next();%>
            <td><input type="checkbox" name = "loan" value=<%=loanDTO.getId()%> /></td>
            <td><%=loanDTO.getIsbn()%></td>
            <td><%=loanDTO.getUser()%></td>
            <td><%=loanDTO.getStartDate()%></td>
            <td><%=loanDTO.getStartDate()%></td>
            <%}
            else{%>
            <td></td>
            <td></td>
            <td></td>
            <td></td>
            <td></td>
            <%}%>
        </tr>
        <%}%>
    </table>
</form>
<button type="submit" form="reservation" value="ConfirmReservation">Confirm Reservation</button>
<button type="submit" form="reservation" value="DeleteReservation">Delete Reservation</button>
<button type="submit" form="loan" value="EndLoan">End Loan</button>
<button type="">History</button>
<button type="">NewLoan</button>
</body>
</html>
