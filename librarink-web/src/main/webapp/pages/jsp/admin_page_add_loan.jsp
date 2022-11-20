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
</head>
<body>
<%--@declare id="loan_registration"--%><form ACTION="src/main/java/it/unipi/dsmt/servlet/AdminPageServlet.java",id="loan_registration">
  <label for="User">User email:</label><br>
  <input type="text" id="User" name="User"><br>
  <label for="ISBN">ISBN:</label><br>
  <input type="text" id="ISBN" name="ISBN">
    <label for="IDBook">ISBN:</label><br>
    <input type="text" id="IDBook" name="IDBook">
</form>
<button type="submit" form="loan_registration" name="button" value="AddLoan">Confirm Reservation</button>
</body>

</html>
