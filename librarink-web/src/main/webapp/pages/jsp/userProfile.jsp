<%@ page contentType="text/html;charset=UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<!-- Page containing user details, including wishlist and pending loans/reservations-->
<html>
<head>
    <title>Librarink - User profile</title>
    <meta charset="UTF-8">
    <title>Librarink - Login page</title>

    <link rel="icon" type="image/x-icon" href="${pageContext.request.contextPath}/images/favicon.ico">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/css/userProfile.css" type="text/css" media="screen">
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

    <script src="${pageContext.request.contextPath}/scripts/userProfile.js"></script>
    <script src="${pageContext.request.contextPath}/scripts/util.js"></script>
    <script src="${pageContext.request.contextPath}/scripts/book_details.js"></script>
    <script src="${pageContext.request.contextPath}/scripts/websocket.js"></script>

</head>
<body>
    <!-- Navbar section -->
    <jsp:include page="../common/navbar_top.jsp">
        <jsp:param name="search_bar" value="false" />
    </jsp:include>

    <div class="wrapper">
        <section>
            <!-- Popup use to show book details in case of click on book thumbnail -->
            <div class="modal modal-xl fade" id="book-detail" tabindex="-1">
                <div class="modal-dialog">
                    <div class="modal-content">
                        <div class="modal-header">
                            <h1 class="modal-title fs-5" id="book-detail-title">Book detail</h1>
                            <div id="modal-close">
                                <button type="button" class="btn-close" data-bs-dismiss="modal"
                                        aria-label="Close"></button>
                            </div>
                        </div>
                        <div id="book-detail-body" class="modal-body">
                            <!-- FILL BY AJAX REQUEST -->
                        </div>
                    </div>
                </div>
            </div>

            <!-- User information content -->
            <div class="container py-5">

                <!-- Personal information -->
                <div class="row">
                    <div class="col-lg-4">
                        <div class="card mb-4">
                            <div class="card-body text-center">
                                <img src="${image}" alt="avatar" class="rounded-circle img-fluid" style="width: 150px;">
                                <h5 class="my-3">${name} ${surname}</h5>
                                <p class="text-muted mb-4">${address}</p>
                            </div>
                        </div>
                    </div>
                    <div class="col-lg-8">
                        <div class="card mb-4">
                            <div class="card-body">
                                <div class="row">
                                    <div class="col-sm-3">
                                        <p class="mb-0">Full Name</p>
                                    </div>
                                    <div class="col-sm-9">
                                        <p class="text-muted mb-0">${name} ${surname}</p>
                                    </div>
                                </div>
                                <hr>
                                <div class="row">
                                    <div class="col-sm-3">
                                        <p class="mb-0">Birthday</p>
                                    </div>
                                    <div class="col-sm-9">
                                        <p class="text-muted mb-0">${birthday}</p>
                                    </div>
                                </div>
                                <hr>
                                <div class="row">
                                    <div class="col-sm-3">
                                        <p class="mb-0">Email</p>
                                    </div>
                                    <div class="col-sm-9">
                                        <p class="text-muted mb-0">${email}</p>
                                    </div>
                                </div>
                                <hr>
                                <div class="row">
                                    <div class="col-sm-3">
                                        <p class="mb-0">Address</p>
                                    </div>
                                    <div class="col-sm-9">
                                        <p class="text-muted mb-0">${address}</p>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- Information about user wishlist, pendant loans and pendant reservations -->
                <div class="row" id="wishlistRow">
                    <h1 class="mb-3 fw-normal">My wishlist</h1>
                </div>

                <div class="row" id="reservationsRow">
                    <h1 class="mb-3 fw-normal">My reservations</h1>
                </div>

                <div class="row" id="loansRow">
                    <h1 class="mb-3 fw-normal">My loans</h1>
                </div>
            </div>
        </section>
    </div>
</body>
</html>
