<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Librarink - Login page</title>
</head>
<body>
<div style="text-align: center;"> <h1> Student Login Form </h1> </div>
<form  action="<%= request.getContextPath()%>/login" method="post" autocomplete="on">
    <div class="container">
        <label for="EnterEmail">Email : </label>
        <input type="text" id="EnterEmail" name="email" required>
        <label for="EnterPassword">Password : </label>
        <input type="password" id="EnterPassword" name="password" required>
        <button type="submit">Login</button>
        <input type="checkbox" checked="checked"> Remember me
        <button type="button" class="cancelbtn"> Cancel</button>
        <%
        if(request.getParameterMap().containsKey("message")) {
        %>
            <h2 style="color:red;"><%=request.getAttribute("message")%></h2>
        <%
        }
        %>
        New member? <a href="<%= request.getContextPath()%>/signup"> Click here to signup </a>
    </div>
</form>
</body>
</html>
