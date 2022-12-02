<%@ page import="it.unipi.dsmt.librarink.LoanDTO" %>
<%@ page import="java.util.List" %>
<%@ page import="java.util.Iterator" %>
<%@ page import="it.unipi.dsmt.librarink.HistoryReservationDTO" %>
<%@ page import="it.unipi.dsmt.librarink.HistoryLoanDTO" %>
<%--
  Created by IntelliJ IDEA.
  User: tummi
  Date: 03/11/2022
  Time: 17:05
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>Title</title>
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
    List<HistoryLoanDTO> listLoan = (List<HistoryLoanDTO>) request.getAttribute("loanHistoryList");
    Iterator<HistoryLoanDTO> loanDTOIterator = listLoan.iterator();
    List<HistoryReservationDTO> listReservation = (List<HistoryReservationDTO>)
            request.getAttribute("reservationHistoryList");
    Iterator<HistoryReservationDTO> reservationDTOIterator=listReservation.iterator();
%>
<body>
<table style="width:100%" >

    <tr>
        <th colspan="4">History Reservation</th>

    </tr>
    <tr>
        <td>ISBN</td>
        <td>User ID</td>
        <td>Start Time</td>
        <td>End Time</td>

    </tr>
    <%  while( reservationDTOIterator.hasNext()){%>
    <tr>
        <%if (reservationDTOIterator.hasNext()){

            HistoryReservationDTO reservationDTO=reservationDTOIterator.next();%>
        <td><%=reservationDTO.getIsbn()%></td>
        <td><%=reservationDTO.getUser()%></td>
        <td><%=reservationDTO.getStartDate()%></td>
        <td><%=reservationDTO.getEndDate()%></td>
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
<table style="width:100%">
    <tr>
        <th colspan="5">History Loan</th>
    </tr>
    <tr>
        <td>Book ID</td>
        <td>ISBN</td>
        <td>User ID</td>
        <td>Start Time</td>
        <td>End Time</td>
    </tr>
    <%  while(loanDTOIterator.hasNext()){%>
    <tr>
        <%if(loanDTOIterator.hasNext()){
            HistoryLoanDTO loanDTO = loanDTOIterator.next();%>

        <td><%=loanDTO.getCopyId()%></td>
        <td><%=loanDTO.getIsbn()%></td>
        <td><%=loanDTO.getUser()%></td>
        <td><%=loanDTO.getStartDate()%></td>
        <td><%=loanDTO.getEndDate()%></td>
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

</body>
</html>
