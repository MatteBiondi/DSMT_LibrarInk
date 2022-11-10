<%@ page contentType="text/html;charset=UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<div class="wrapper">
    <section id="content">
        <c:choose>
            <c:when test="${book == null}">
                <h1>Details no available !</h1>
            </c:when>
            <c:otherwise>
                <jsp:include page="../common/book.jsp">
                    <jsp:param name="type" value="detail" />
                    <jsp:param name="image" value="${book.getImage_url_m()}" />
                    <jsp:param name="title" value="${book.getBook_title()}" />
                    <jsp:param name="author" value="${book.getBook_author()}" />
                    <jsp:param name="isbn" value="${book.getIsbn()}" />
                    <jsp:param name="rating" value="${rating}" />
                    <jsp:param name="category" value="${book.getGenre()}" />
                    <jsp:param name="publisher" value="${book.getPublisher()}" />
                    <jsp:param name="published_date" value="${book.getYear_of_publication()}" />
                    <jsp:param name="language" value="English" />
                    <jsp:param name="description" value="${book.getDescription()}" />
                    <jsp:param name="available_copies" value="${available_copies}" />
                </jsp:include>
            </c:otherwise>
        </c:choose>
    </section>
</div>
<!-- TODO language-->