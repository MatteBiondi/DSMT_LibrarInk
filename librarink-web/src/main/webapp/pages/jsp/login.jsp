<%--
  Created by IntelliJ IDEA.
  User: tummi
  Date: 12/09/2022
  Time: 22:33
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Title</title>
</head>
<body>
<div style="text-align: center;"> <h1> Student Login Form </h1> </div>
<form  action="<%= request.getContextPath()%>/loginServlet" method="GET" >
    <div class="container">
        <label for="EnterEmail">Email : </label>
        <input type="text" id="EnterEmail" name="email" required>
        <label for="EnterPassword">Password : </label>
        <input type="password" id="EnterPassword" name="password" required>
        <button type="submit">Login</button>
        <input type="checkbox" checked="checked"> Remember me
        <button type="button" class="cancelbtn"> Cancel</button>
        <%
        if(request.getParameterMap().containsKey("message"))
        {
        %>
        <h2 style="color:red;"><%=request.getAttribute("message")%></h2>
        <%
            }
        %>
        New member? <a href="<%= request.getContextPath()%>/SignUpServlet"> click here to signUp </a>
    </div>
</form>
</body>
</html>
