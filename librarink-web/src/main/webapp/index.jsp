<%@ page contentType="text/html;charset=UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<html>
    <head>
        <title>Librarink</title>
        <link rel="icon" type="image/x-icon" href="${pageContext.request.contextPath}/images/favicon.ico">
        <link rel="stylesheet" href="${pageContext.request.contextPath}/css/index.css" type="text/css" media="screen">
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
        <script src="${pageContext.request.contextPath}/scripts/util.js"></script>
        <script src="${pageContext.request.contextPath}/scripts/book_search.js"></script>
        <script src="${pageContext.request.contextPath}/scripts/websocket.js"></script>
        <script src="${pageContext.request.contextPath}/scripts/book_details.js"></script>
    </head>

    <body>
        <div class="wrapper">
            <jsp:include page="pages/common/navbar_top.jsp">
                <jsp:param name="search_bar" value="true" />
            </jsp:include>
            <section id="content">
                <div class="modal modal-xl fade" id="book-detail" tabindex="-1">
                    <div class="modal-dialog">
                        <div class="modal-content">
                            <div class="modal-header">
                                <h1 class="modal-title fs-5" id="book-detail-title">Book detail</h1>
                                <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
                            </div>
                            <div id="book-detail-body" class="modal-body">
                                <!-- FILL BY AJAX REQUEST -->
                            </div>
                        </div>
                    </div>
                </div>
                <div id="book-list"><!-- FILL BY AJAX REQUEST --></div>
            </section>
        </div>
    </body>
</html>
