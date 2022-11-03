<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Librarink - Sign Up form</title>
</head>
<body>
<div style="text-align: center;"> <h1> Signup </h1> </div>
<form  action="<%= request.getContextPath()%>/signup" method="post" autocomplete="on">
    <div class="container">
        <label for="EnterEmail">Email : </label>
        <input type="text" id="EnterEmail" name="email" required>
        <label for="EnterName">Name : </label>
        <input type="text" id="EnterName" name="name" required>
        <label for="EnterSurname">Surname : </label>
        <input type="text" id="EnterSurname" name="surname" required>
        <label for="EnterAddress">Address : </label>
        <input type="text" id="EnterAddress" name="address" required>
        <label for="birthday">Birthday:</label>
        <input type="date" id="birthday" name="birthday">
        <label>Password : </label>
        <input type="password" placeholder="Enter Password" name="password" required>

        <button type="submit">SignUp</button>
        <button type="button" class="cancelbtn"> Cancel</button>
        <%
            String message = (String) request.getAttribute("message");
            if(message != null) {
        %>
                <h2 style="color:red;"><%=message%></h2>
        <%
            }
        %>
        Already a member? <a href="<%= request.getContextPath()%>/login"> click here to login </a>
    </div>
</form>
</body>
</html>
