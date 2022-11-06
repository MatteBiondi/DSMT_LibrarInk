<%@ page contentType="text/html;charset=UTF-8" %>
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
        <form  action="<%= request.getContextPath()%>/login" method="post" autocomplete="on">
            <img class="mb-4" src="<%=request.getContextPath()%>/images/logo.svg" alt="Librarink Logo" width="72" height="57">
            <h1 class="mb-3 fw-normal">Librarink</h1>
            <div class="p-3 shadow-sm border rounded-3">
                <h2 class="text-center mb-4 text-primary">Please sign in</h2>
                <div class="form-floating">
                    <input type="email" class="form-control" id="EnterEmail" name="email" required>
                    <label for="EnterEmail" class="text-left">Email : </label>
                </div>

                <div class="form-floating">
                    <input type="password" class="form-control" id="EnterPassword" name="password" required>
                    <label for="EnterPassword" class="text-left">Password : </label>
                </div>

                <button type="submit" class="btn btn-lg btn-primary">Login</button>
                <button type="reset" class="btn btn-lg btn-primary"> Cancel</button>

                <%
                    String message =(String) request.getSession().getAttribute("message");
                    String messageType;
                    if(message != null) {
                        request.getSession().removeAttribute("message");
                        messageType = "success-message";
                    }
                    else {
                        message = (String) request.getAttribute("message");
                        messageType = "error-message";
                    }
                    if(message != null) {
                %>
                <div class="form-text <%=messageType%> "><%=message%></div>
                <%
                    }
                %>
                <div class="form-text">
                    New member?
                    <a href="<%= request.getContextPath()%>/signup"> Click here to signup </a>
                </div>
            </div>
        </form>
    </main>
</body>
</html>
