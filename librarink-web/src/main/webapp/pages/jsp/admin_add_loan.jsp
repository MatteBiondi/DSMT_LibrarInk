<%@ page contentType="text/html;charset=UTF-8" %>

<form action="${pageContext.request.contextPath}/adminAddLoan" id="loan_registration">
    <div class="mb-3">
        <label for="add-loan-user">User email:</label>
        <input type="text" id="add-loan-user" name="User" class="form-control basicAutoComplete"
               data-url="${pageContext.request.contextPath}/request/async?filter=user"
               autocomplete="off">
    </div>
    <div class="mb-3">
        <label for="add-loan-isbn">ISBN:</label>
        <input type="text" id="add-loan-isbn" name="ISBN" class="form-control basicAutoComplete"
               data-url="${pageContext.request.contextPath}/request/async?filter=isbn"
               autocomplete="off">
    </div>
    <div class="mb-3">
        <label for="add-loan-id">Book ID:</label>
        <select class="form-select" name="IDList" id="add-loan-id">
            <!-- FILLED BY AJAX REQUEST -->
        </select>
    </div>
</form>
<button class="btn btn-primary" type="button"
        onclick="submitAddLoanPage('add-loan-isbn','add-loan-user','add-loan-id','loan_registration')"
        form="loan_registration"
        name="buttonConfirm"
        value="AddLoan">
    Add Loan
</button>



