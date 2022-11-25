<%@ page contentType="text/html;charset=UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<jsp:useBean id="books" scope="request" type="java.util.List"/>

<link rel="stylesheet" href="${pageContext.request.contextPath}/css/book.css" type="text/css" media="screen">
<link rel="stylesheet" href="${pageContext.request.contextPath}/css/book_list.css" type="text/css" media="screen">

<div id="book-items">
    <div class="row row-cols-2 row-cols-md-5 g-4">
        <c:forEach items="${books}" var="book">
            <jsp:include page="book.jsp">
                <jsp:param name="type" value="thumbnail" />
                <jsp:param name="image" value="${book.getImageUrlM()}" />
                <jsp:param name="title" value="${book.getTitle()}" />
                <jsp:param name="author" value="${book.getAuthor()}" />
                <jsp:param name="isbn" value="${book.getIsbn()}" />
            </jsp:include>
        </c:forEach>
    </div>
</div>
<nav id="pagination-menu">
    <jsp:include page="pagination.jsp"/>
</nav>
