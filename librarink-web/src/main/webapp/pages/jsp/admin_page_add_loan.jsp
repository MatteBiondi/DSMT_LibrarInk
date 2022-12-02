<%--
  Created by IntelliJ IDEA.
  User: tummi
  Date: 05/11/2022
  Time: 16:15
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>admin_page_add_loan</title>
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
<body>
<%--@declare id="loan_registration"--%><form ACTION="admin",id="loan_registration">
  <label for="User">User email:</label><br>
  <input type="text" id="User" name="User"><br>
  <label for="ISBN">ISBN:</label><br>
  <input type="text" id="ISBN" name="ISBN"><br>
  <label for="IDBook">IDBook:</label><br>
  <input type="text" id="IDBook" name="IDBook" onclick="menuListId('reservationInputField','IdList',<script>getValue(inputIdField);</script>)"
         list="IdList" id=reservationInputField" name="bookID" />
  <datalist id="IdList"></datalist>


</form>
<button form="loan_registration" name="button" value="AddLoan">Confirm Reservation</button>
</body>
<script src="${pageContext.request.contextPath}/scripts/admin_page.js"></script>
</html>
