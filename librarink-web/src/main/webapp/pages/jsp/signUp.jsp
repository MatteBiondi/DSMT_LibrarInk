<%@ page contentType="text/html;charset=UTF-8" %>

<!-- Page containing user signup form -->
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <title>Librarink - Sign Up form</title>

        <link rel="icon" type="image/x-icon" href="${pageContext.request.contextPath}/images/favicon.ico">
        <link rel="stylesheet" href="${pageContext.request.contextPath}/css/signup.css" type="text/css" media="screen">
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.2.1/dist/css/bootstrap.min.css"
              rel="stylesheet" integrity="sha384-iYQeCzEYFbKjA/T2uDLTpkwGzCiq6soy8tYaI1GyVh/UjpbCx/TYkiZhlZB6+fzT"
              crossorigin="anonymous">
        <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.9.1/font/bootstrap-icons.css"
              integrity="sha384-xeJqLiuOvjUBq3iGOjvSQSIlwrpqjSHXpduPd6rQpuiM3f5/ijby8pCsnbu5S81n"
              crossorigin="anonymous">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.2.1/dist/js/bootstrap.bundle.min.js"></script>
    </head>
    <body>
        <main class="w-50 m-auto">
            <img class="mb-4" src="<%=request.getContextPath()%>/images/logo.svg" alt="Librarink Logo" width="72" height="57">
            <h1 class="mb-3 fw-normal">Librarink</h1>

            <!-- Signup form sends data to loginServlet by post method -->
            <form  action="<%= request.getContextPath()%>/signup" method="post" autocomplete="on">
                <h3 class="mb-4 fw-normal text-primary"> Signup Form</h3>
                <div class="container">

                    <!-- Email field -->
                    <div class="mb-3">
                        <label for="EnterEmail" class="form-label">Email : </label>
                        <input type="email" class="form-control" id="EnterEmail" name="email" required>
                    </div>

                    <!-- Name field -->
                    <div class="mb-3">
                        <label for="EnterName" class="form-label">Name : </label>
                        <input type="text" class="form-control" id="EnterName" name="name" required>
                    </div>

                    <!-- Surname field -->
                    <div class="mb-3">
                        <label for="EnterSurname" class="form-label">Surname : </label>
                        <input type="text" class="form-control" id="EnterSurname" name="surname" required>
                    </div>

                    <!-- Address field -->
                    <div class="mb-3">
                        <label for="EnterAddress" class="form-label">Address : </label>
                        <input type="text" class="form-control" id="EnterAddress" name="address" required>
                    </div>

                    <!-- Birthday field -->
                    <div class="mb-3">
                        <label for="birthday" class="form-label">Birthday:</label>
                        <input type="date" class="form-control" id="birthday" name="birthday">
                    </div>

                    <!-- Password field -->
                    <div class="mb-3">
                        <label for="password" class="form-label">Password : </label>
                        <input type="password" class="form-control" id="password" placeholder="Enter Password" name="password" required>
                    </div>

                    <button type="submit" class="btn btn-primary">SignUp</button>
                    <button type="reset" class="btn btn-primary">Cancel</button>

                    <div class="mb-3">
                        <!-- If present, get message coming as request or session attribute and show it -->
                        <%
                            String message = (String) request.getAttribute("message");
                            if(message != null) {
                        %>
                        <div class="form-text error-message"><%=message%></div>
                        <%
                            }
                        %>

                        <!-- Section with link to login page -->
                        <div class="form-text">Already a member?
                            <a href="<%= request.getContextPath()%>/login"> click here to login </a>
                        </div>
                    </div>
                </div>
            </form>
        </main>
    </body>
</html>
