<%@ page contentType="text/html;charset=UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<jsp:useBean id="reservationList" scope="request" type="java.util.List"/>
<jsp:useBean id="loanList" scope="request" type="java.util.List"/>
<jsp:useBean id="table" scope="request" type="java.lang.String"/>
<jsp:useBean id="df" scope="request" type="java.text.SimpleDateFormat"/>

<nav id="nav-tabs">
    <div class="nav nav-tabs" id="nav-tab" role="tablist">
        <div class="button-tab">
            <c:choose>
                <c:when test="${table.equals(\"active\")}">
                    <button class="nav-link active" id="nav-reservation-tab" data-bs-toggle="tab"
                            data-bs-target="#nav-reservation" type="button" role="tab">Pending Reservations</button>
                    <button class="nav-link" id="nav-loan-tab" data-bs-toggle="tab"
                            data-bs-target="#nav-loan" type="button" role="tab"
                           >Active Loans</button>
                </c:when>
                <c:when test="${table.equals(\"history\")}">
                    <button class="nav-link active" id="nav-history-reservation-tab" data-bs-toggle="tab"
                            data-bs-target="#nav-history-reservation" type="button" role="tab">History Reservations</button>
                    <button class="nav-link" id="nav-history-loan-tab" data-bs-toggle="tab"
                            data-bs-target="#nav-history-loan" type="button" role="tab">History Loans</button>
                </c:when>
            </c:choose>
        </div>
        <div id="active-commands">
            <c:if test="${table.equals(\"active\")}">
                <button class="btn btn-outline-dark" form="reservation" name="button"
                        value="ConfirmReservation"
                        type="button"
                        id="confirm-reservation-btn"
                        onclick="submitRequest('reservation','ConfirmReservation','reservation_table','reservation_checkbox')">
                    Confirm Reservation
                </button>
                <button class="btn btn-outline-dark" form="reservation" name="button"
                        value="DeleteReservation"
                        type="button"
                        id="delete-reservation-btn"
                        onclick="submitRequest('reservation','DeleteReservation','reservation_table','reservation_checkbox')">
                    Delete Reservation
                </button>
                <button class="btn btn-outline-dark" form="loan" name="button"
                        value="EndLoan"
                        type="button"
                        id="end-loan-btn"
                        onclick="submitRequest('loan','EndLoan','loan_table','loan_checkbox')">
                    End Loan
                </button>
                <button class="btn btn-outline-dark" id="add-book-copy">Modify book copy</button>
                <button class="btn btn-outline-dark" id="add-new-loan">New Loan</button>
            </c:if>
        </div>
    </div>
</nav>
<c:choose>
    <c:when test="${table.equals(\"active\")}">
        <div class="tab-content" id="nav-tabContent">
            <div class="tab-pane fade show active" id="nav-reservation" role="tabpanel" tabindex="0">
                <form action="<%= request.getContextPath()%>/admin" method="POST" id="reservation">
                        <table class="table table-hover table-striped table-borderless align-middle"
                               id="reservation_table">
                            <thead>
                            <tr>
                                <th scope="col">Selection</th>
                                <th scope="col">ISBN</th>
                                <th scope="col">User ID</th>
                                <th scope="col">Start Time</th>
                                <th scope="col">Book ID</th>
                            </tr>
                            </thead>
                            <tbody>
                                <c:forEach items="${reservationList}" var="reservationDTO">
                                    <tr>
                                        <td>
                                            <input type="checkbox" name="reservation" class="reservation_checkbox"
                                                   value="${reservationDTO.getUser()};${reservationDTO.getIsbn()}"/>
                                        </td>
                                        <td>${reservationDTO.getIsbn()}</td>
                                        <td>${reservationDTO.getUser()}</td>
                                        <td>${df.format(reservationDTO.getStartDate())}</td>
                                        <td>
                                            <select
                                                    data-isbn="${reservationDTO.getIsbn()}"
                                                    onfocus="menuSelectListId(
                                                            'reservation${reservationDTO.getUser()}${reservationDTO.getIsbn()}',
                                                            '${reservationDTO.getIsbn()}')"
                                                    name="IDList"
                                                    id="reservation${reservationDTO.getUser()}${reservationDTO.getIsbn()}"
                                                    class="book-id-selection">
                                                <!--  FILLED BY AJAX --->
                                            </select>
                                        </td>
                                    </tr>
                                </c:forEach>
                            </tbody>
                        </table>
                </form>
            </div>
            <div class="tab-pane fade" id="nav-loan" role="tabpanel" tabindex="0">
                <form action="<%= request.getContextPath()%>/admin" method="POST" id="loan">
                    <div class="table-responsive">
                        <table id="loan_table" class="table table-hover table-striped table-borderless align-middle">
                            <thead>
                            <tr>
                                <th scope="col">Selection</th>
                                <th scope="col">Book ID</th>
                                <th scope="col">ISBN</th>
                                <th scope="col">User ID</th>
                                <th scope="col">Start Time</th>
                            </tr>
                            </thead>
                            <tbody>
                                <c:forEach items="${loanList}" var="loanDTO">
                                    <tr>
                                        <td>
                                            <input type="checkbox" name="loan"  class="loan_checkbox"
                                                   value="${loanDTO.getIsbn()};${loanDTO.getCopyId()};${loanDTO.getUser()}" />
                                        </td>
                                        <td>${loanDTO.getCopyId()}</td>
                                        <td>${loanDTO.getIsbn()}</td>
                                        <td>${loanDTO.getUser()}</td>
                                        <td>${df.format(loanDTO.getStartDate())}</td>
                                    </tr>
                                </c:forEach>
                            </tbody>
                        </table>
                    </div>
                </form>
            </div>
        </div>
    </c:when>
    <c:when test="${table.equals(\"history\")}">
        <div class="tab-content" id="nav-tabContent">
            <div class="tab-pane fade show active" id="nav-history-reservation" role="tabpanel" tabindex="0">
                <div class="table-responsive">
                    <table class="table table-hover table-striped table-borderless align-middle"
                           id="history-reservation-table">
                        <thead>
                        <tr>
                            <th scope="col">ISBN</th>
                            <th scope="col">User ID</th>
                            <th scope="col">Start Date</th>
                            <th scope="col">End Date</th>
                            <th scope="col">Deleted</th>
                        </tr>
                        </thead>
                        <c:forEach items="${reservationList}" var="historyReservationDTO">
                            <tr>
                                <td>${historyReservationDTO.getIsbn()}</td>
                                <td>${historyReservationDTO.getUser()}</td>
                                <td>${df.format(historyReservationDTO.getStartDate())}</td>
                                <td>${df.format(historyReservationDTO.getEndDate())}</td>
                                <td>${historyReservationDTO.isDeleted()}</td>
                            </tr>
                        </c:forEach>
                    </table>
                </div>
            </div>
            <div class="tab-pane fade" id="nav-history-loan" role="tabpanel" tabindex="0">
                <div class="table-responsive">
                    <table class="table table-hover table-striped table-borderless align-middle" id="history-loan-table">
                        <thead>
                        <tr>
                            <th scope="col">ISBN</th>
                            <th scope="col">Book ID</th>
                            <th scope="col">User Email</th>
                            <th scope="col">Start Date</th>
                            <th scope="col">End Date</th>
                        </tr>
                        </thead>
                        <c:forEach items="${loanList}" var="historyLoanDTO">
                            <tr>
                                <td>${historyLoanDTO.getIsbn()}</td>
                                <td>${historyLoanDTO.getCopyId()}</td>
                                <td>${historyLoanDTO.getUser()}</td>
                                <td>${df.format(historyLoanDTO.getStartDate())}</td>
                                <td>${df.format(historyLoanDTO.getEndDate())}</td>
                            </tr>
                        </c:forEach>
                    </table>
                </div>
            </div>
        </div>
    </c:when>
    <c:otherwise>
        <h1>Error !</h1>
    </c:otherwise>
</c:choose>