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
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.2.1/dist/css/bootstrap.min.css"
          rel="stylesheet" integrity="sha384-iYQeCzEYFbKjA/T2uDLTpkwGzCiq6soy8tYaI1GyVh/UjpbCx/TYkiZhlZB6+fzT"
          crossorigin="anonymous">
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.9.1/font/bootstrap-icons.css"
          integrity="sha384-xeJqLiuOvjUBq3iGOjvSQSIlwrpqjSHXpduPd6rQpuiM3f5/ijby8pCsnbu5S81n"
          crossorigin="anonymous">
    <script src="https://code.jquery.com/jquery-3.6.1.js"
            integrity="sha256-3zlB5s2uwoUzrXK3BT7AX3FyvojsraNFxCc2vC/7pNI="
            crossorigin="anonymous">
    </script>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.2.1/dist/js/bootstrap.bundle.min.js"></script>
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
            while(reservationDTOIterator.hasNext()){%>
        <tr>
            <%
                if(reservationDTOIterator.hasNext()){
                    ReservationDTO reservationDTO=reservationDTOIterator.next();%>
            <td><input type="checkbox" name = "reservation" class="reservation_checkbox" value=<%=
            reservationDTO.getUser()+";"+reservationDTO.getIsbn()%> /></td>
                <td><%=reservationDTO.getIsbn()%></td>
                <td><%=reservationDTO.getUser()%></td>
                <td><%=reservationDTO.getStartDate()%></td>
                <td><%=reservationDTO.getStopDate()%></td>
                <td><label for="<%="reservation"+reservationDTO.getUser()+reservationDTO.getIsbn()%>"></label>
                    <select name="IDList"  id="<%="reservation"+reservationDTO.getUser()+reservationDTO.getIsbn()%>" onfocus="menuSelectListId('<%="reservation"+reservationDTO.getUser()+reservationDTO.getIsbn()%>','<%=reservationDTO.getIsbn()%>')">

                    </select>
            <%}%>


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
        <%while(loanDTOIterator.hasNext()){%>
        <tr>
            <%if(loanDTOIterator.hasNext()){
                LoanDTO loanDTO=loanDTOIterator.next();%>
            <td><input type="checkbox" name = "loan"  class="loan_checkbox" value=<%=loanDTO.getIsbn()+";"+loanDTO.getCopyId()+";"+loanDTO.getUser()%> /></td>
            <td><%=loanDTO.getCopyId()%></td>
            <td><%=loanDTO.getIsbn()%></td>
            <td><%=loanDTO.getUser()%></td>
            <td><%=loanDTO.getStartDate()%></td>
            <td><%=loanDTO.getStopDate()%></td>
            <%}%>

        </tr>
        <%}%>
    </table>
</form>
<button type="button" form="reservation" name="button" value="ConfirmReservation"onclick="submitRequest('reservation','ConfirmReservation','reservation_table','reservation_checkbox')">Confirm Reservation</button>
<button type="button" form="reservation" name="button" value="DeleteReservation" onclick="submitRequest('reservation','DeleteReservation','reservation_table','reservation_checkbox')">Delete Reservation</button>
<button type="button" form="loan" name="button" value="EndLoan" onclick="submitRequest('loan','EndLoan','loan_table','loan_checkbox')">End Loan</button>
<a href="<%= request.getContextPath()%>/adminHistory">History table</a>
<a href="<%= request.getContextPath()%>/adminAddLoan">New Loan</a>
<a href="<%= request.getContextPath()%>/adminAddBookCopy">Add book copy</a>
<a href="<%= request.getContextPath()%>/admin">Refresh</a>
<script src="${pageContext.request.contextPath}/scripts/admin_page.js"></script>

</body>
</html>
