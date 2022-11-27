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
    <%  while(loanDTOIterator.hasNext() || reservationDTOIterator.hasNext()){%>
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
        <%if(loanDTOIterator.hasNext()){
            loanDTOIterator.next();}%>
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
    <%  while(loanDTOIterator.hasNext() || reservationDTOIterator.hasNext()){%>
    <tr>
        <%if(reservationDTOIterator.hasNext()){

            reservationDTOIterator.next();}%>
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
