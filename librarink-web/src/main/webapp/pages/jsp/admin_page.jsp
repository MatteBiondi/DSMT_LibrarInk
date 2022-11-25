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
                    <input type="text" onclick="menuListId(<%="reservation"+reservationDTO.getUser()+reservationDTO.getIsbn()%>
                            ,<%="IdList"+reservationDTO.getUser()+reservationDTO.getIsbn()%>,<%=reservationDTO.getIsbn()%>)"
                           list="<%="IdList"+reservationDTO.getUser()+reservationDTO.getIsbn()%>" id="<%="reservation"+reservationDTO.getUser()+reservationDTO.getIsbn()%>" name="bookID" />
                    <datalist id="<%="IdList"+reservationDTO.getUser()+reservationDTO.getIsbn()%>"></datalist><br><br></td>
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
<script src="${pageContext.request.contextPath}/scripts/admin_page.js"></script>
</body>
</html>
