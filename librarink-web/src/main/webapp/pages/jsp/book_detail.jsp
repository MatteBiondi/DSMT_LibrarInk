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
    <script src="${pageContext.request.contextPath}/scripts/websocket.js"></script>
    <script src="${pageContext.request.contextPath}/scripts/book_details.js"></script>
</head>

<body>
<div class="wrapper">
    <jsp:include page="../common/navbar_top.jsp">
        <jsp:param name="search_bar" value="false" />
    </jsp:include>
    <section id="content">

        <c:if test="${book == null}">
            <h1>Details no available !</h1>
        </c:if>
        <c:if test="${book != null}">
            <jsp:include page="../common/book.jsp">
                <jsp:param name="type" value="detail" />
                <jsp:param name="image" value="${book.getImage_url_m()}" />
                <jsp:param name="title" value="${book.getBook_title()}" />
                <jsp:param name="author" value="${book.getBook_author()}" />
                <jsp:param name="isbn" value="${book.getIsbn()}" />
                <jsp:param name="rate" value="${book.getSum_of_stars()}" />
                <jsp:param name="category" value="${book.getGenre()}" />
                <jsp:param name="publisher" value="${book.getPublisher()}" />
                <jsp:param name="published_date" value="${book.getYear_of_publication()}" />
                <jsp:param name="language" value="English" />
                <jsp:param name="description" value="${book.getDescription()}" />
                <jsp:param name="available_copies" value="${available_copies}" />
            </jsp:include>
        </c:if>
    </section><!-- TODO language-->
</div>

</body>
</html>
