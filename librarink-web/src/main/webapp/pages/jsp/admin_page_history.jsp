<%@ page import="it.unipi.dsmt.librarink.Librarink_history_loanDTO" %>
<%@ page import="java.util.List" %>
<%@ page import="java.util.Iterator" %>
<%@ page import="it.unipi.dsmt.librarink.Librarink_history_reservationDTO" %><%--
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
    List<Librarink_history_loanDTO> listLoan = (List<Librarink_history_loanDTO>) request.getAttribute("loanHistoryList");
    Iterator<Librarink_history_loanDTO> loanDTOIterator=listLoan.iterator();

    List<Librarink_history_reservationDTO> listReservation = (List<Librarink_history_reservationDTO>) request.getAttribute("reservationHistoryLis");
    Iterator<Librarink_history_reservationDTO> reservationDTOIterator=listReservation.iterator();
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
    <%  while(loanDTOIterator.hasNext() ||reservationDTOIterator.hasNext()){%>
    <tr>
        <%if(reservationDTOIterator.hasNext()){

            Librarink_history_reservationDTO reservationDTO=reservationDTOIterator.next();%>
        <td><%=reservationDTO.getIsbn()%></td>
        <td><%=reservationDTO.getUser_email()%></td>
        <td><%=reservationDTO.getStart_date()%></td>
        <td><%=reservationDTO.getEnd_date()%></td>
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
    <%  while(loanDTOIterator.hasNext() ||reservationDTOIterator.hasNext()){%>
    <tr>
        <%if(reservationDTOIterator.hasNext()){

            reservationDTOIterator.next();}%>
        <%if(loanDTOIterator.hasNext()){
            Librarink_history_loanDTO loanDTO=loanDTOIterator.next();%>

        <td><%=loanDTO.getId_copy()%></td>
        <td><%=loanDTO.getIsbn()%></td>
        <td><%=loanDTO.getUser_email()%></td>
        <td><%=loanDTO.getStart_date()%></td>
        <td><%=loanDTO.getEnd_date()%></td>
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
