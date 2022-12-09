<%@ page contentType="text/html;charset=UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<jsp:useBean id="reservationList" scope="request" type="java.util.List"/>
<jsp:useBean id="loanList" scope="request" type="java.util.List"/>
<jsp:useBean id="table" scope="request" type="java.lang.String"/>

<html>
    <head>
        <title>Admin</title>
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
        <script src="https://cdn.jsdelivr.net/gh/xcash/bootstrap-autocomplete@master/dist/latest/bootstrap-autocomplete.min.js"></script>
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.2.1/dist/js/bootstrap.bundle.min.js"></script>
        <script src="//cdn.datatables.net/1.13.1/js/jquery.dataTables.min.js"></script>
        <link rel="stylesheet" href="//cdn.datatables.net/1.13.1/css/jquery.dataTables.min.css">
        <link rel="icon" type="image/x-icon" href="${pageContext.request.contextPath}/images/favicon.ico">
        <link rel="stylesheet" href="${pageContext.request.contextPath}/css/admin.css" type="text/css" media="screen">
        <link rel="stylesheet" href="${pageContext.request.contextPath}/css/message.css" type="text/css" media="screen">
        <script src="${pageContext.request.contextPath}/scripts/admin.js"></script>
        <c:choose>
            <c:when test="${table.equals(\"active\")}">
                <script src="${pageContext.request.contextPath}/scripts/admin_page.js"></script>
            </c:when>
            <c:when test="${table.equals(\"history\")}">
                <script src="${pageContext.request.contextPath}/scripts/admin_history_page.js"></script>
            </c:when>
        </c:choose>
    </head>
    <body>
        <div class="modal modal-xl fade" id="popup" tabindex="-1">
            <div class="modal-dialog">
                <div class="modal-content">
                    <div class="modal-header">
                        <h1 class="modal-title fs-5" id="popup-title"></h1>
                        <div id="modal-close">
                            <button type="button" class="btn-close"
                                    data-bs-dismiss="modal"
                                    aria-label="Close"></button>
                        </div>
                    </div>
                    <div id="popup-body" class="modal-body">
                        <!-- FILL BY AJAX REQUEST -->
                    </div>
                </div>
            </div>
        </div>
        <div class="wrapper">
            <jsp:include page="../common/admin_sidebar.jsp">
                <jsp:param name="table" value="${table}"/>
            </jsp:include>
            <section id="content">
                <jsp:include page="../common/admin_tables.jsp">
                    <jsp:param name="table" value="${table}"/>
                    <jsp:param name="reservationList" value="${reservationList}"/>
                    <jsp:param name="loanList" value="${loanList}"/>
                </jsp:include>
            </section>
        </div>
    </body>
</html>