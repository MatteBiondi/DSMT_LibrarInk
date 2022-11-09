<%@ page contentType="text/html;charset=UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<jsp:useBean id="books" scope="request" type="java.util.List"/>

<link rel="stylesheet" href="${pageContext.request.contextPath}/css/book.css" type="text/css" media="screen">
<link rel="stylesheet" href="${pageContext.request.contextPath}/css/book_list.css" type="text/css" media="screen">

<div id="book-items">

    <c:forEach items="${books}" var="book">
        <jsp:include page="book.jsp">
            <jsp:param name="type" value="thumbnail" />
            <jsp:param name="image" value="${book.getImage_url_m()}" />
            <jsp:param name="title" value="${book.getBook_title()}" />
            <jsp:param name="author" value="${book.getBook_author()}" />
            <jsp:param name="isbn" value="${book.getIsbn()}" />
            <jsp:param name="rate" value="${book.getSum_of_stars()}" />
        </jsp:include>
    </c:forEach>
</div>
<nav id="pagination-menu">
    <jsp:include page="pagination.jsp"/>
</nav>
