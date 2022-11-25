<%@ page contentType="text/html;charset=UTF-8" %>

<!-- Page containing user login form -->
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Librarink - Login page</title>

    <link rel="icon" type="image/x-icon" href="${pageContext.request.contextPath}/images/favicon.ico">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/css/login.css" type="text/css" media="screen">
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.2.0/dist/css/bootstrap.min.css" rel="stylesheet"
          integrity="sha384-gH2yIJqKdNHPEq0n4Mqa/HGKIhSkIHeL5AyhkYV8i59U5AR6csBvApHHNl/vI1Bx" crossorigin="anonymous">
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.9.1/font/bootstrap-icons.css"
          integrity="sha384-xeJqLiuOvjUBq3iGOjvSQSIlwrpqjSHXpduPd6rQpuiM3f5/ijby8pCsnbu5S81n"
          crossorigin="anonymous">
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.2.1/dist/js/bootstrap.bundle.min.js"></script>
</head>
<body class="text-center">
    <main class="w-50 form-signin m-auto">
        <img class="mb-4" src="<%=request.getContextPath()%>/images/logo.svg" alt="Librarink Logo" width="72" height="57">
        <h1 class="mb-3 fw-normal">Librarink</h1>

        <!-- Login form sends data to loginServlet by post method -->
        <form  action="<%= request.getContextPath()%>/login" method="post" autocomplete="on">
            <div class="p-3 shadow-sm border rounded-3">
                <h2 class="text-center mb-4 text-primary">Please sign in</h2>

                <!-- Email field -->
                <div class="form-floating">
                    <input type="email" class="form-control" id="EnterEmail" name="email" autocomplete="on" required>
                    <label for="EnterEmail" class="text-left">Email : </label>
                </div>
                <!-- Password field -->
                <div class="form-floating">
                    <input type="password" class="form-control" id="EnterPassword" name="password" autocomplete="on" required>
                    <label for="EnterPassword" class="text-left">Password : </label>
                </div>
                <!-- Checkbox -->
                <div class="form-check">
                    <input class="form-check-input" type="checkbox" value="adminLogin" id="adminCheck" name="adminCheck"  />
                    <label class="form-check-label admin-check-label" for="adminCheck"> Admin user </label>
                </div>
                <button type="submit" class="btn btn-lg btn-primary">Login</button>
                <button type="reset" class="btn btn-lg btn-primary"> Cancel</button>

                <!-- If present, get message coming as request or session attribute and show it -->
                <%
                    String message =(String) request.getSession().getAttribute("message");
                    String messageType =(String) request.getSession().getAttribute("messageType");;
                    if(message != null) {
                        request.getSession().removeAttribute("message");
                        request.getSession().removeAttribute("message");
                    }
                    else {
                        message = (String) request.getAttribute("message");
                        messageType = (String) request.getAttribute("messageType");
                    }
                    if(message != null) {
                %>
                <div class="form-text <%=messageType%> "><%=message%></div>
                <%
                    }
                %>

                <!-- Section with link to signup page -->
                <div class="form-text">
                    New member?
                    <a href="<%= request.getContextPath()%>/signup"> Click here to signup </a>
                </div>
            </div>
        </form>
    </main>
</body>
</html>
