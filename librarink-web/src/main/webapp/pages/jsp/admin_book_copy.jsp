<%@ page contentType="text/html;charset=UTF-8"%>

<form action="${pageContext.request.contextPath}/adminAddLoan" id="book-copy">
    <div class="mb-3">
        <label for="add-book-isbn">ISBN:</label><br>
        <input type="text" id="add-book-isbn" name="ISBN" class="form-control basicAutoComplete"
               data-url="${pageContext.request.contextPath}/request/async?filter=isbn"
               autocomplete="off">
    </div>
    <div class="mb-3">
        <label for="radio-add">add copy</label><br>
        <input class="add-book-opt" type="radio" checked id="radio-add" name="option_submit" value="add"
               onclick="$('#book-copy-operation').text('Add')"><br>
        <label for="radio-remove">remove copy</label><br>
        <input class="add-book-opt" type="radio" id="radio-remove" name="option_submit" value="remove"
               onclick="$('#book-copy-operation').text('Remove')">
    </div>
    <div class="mb-3">
        <label for="add-book-id">Book ID:</label>
        <select class="form-select" name="IDList" id="add-book-id" disabled>
            <!-- FILLED BY AJAX REQUEST -->
        </select>
    </div>
</form>
<button class="btn btn-primary"
        type="button"
        onclick="submitAddDeleteCopyBookPage('add-book-isbn','add-book-id','option_submit')"
        form="book-copy"
        name="buttonConfirm">
    <span id="book-copy-operation">Add</span> book copy
</button>