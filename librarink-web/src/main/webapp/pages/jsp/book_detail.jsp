<%@ page contentType="text/html;charset=UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<!-- Section in charge of containing all the book detailed information. See "book.jsp" -->
<div class="wrapper">
    <section id="content">
        <c:choose>
            <c:when test="${book == null}">
                <h1>Details no available !</h1>
            </c:when>
            <c:otherwise>
                <!-- Pass parameter to book.jsp that is in charge of book details visualization -->
                <jsp:include page="../common/book.jsp">
                    <jsp:param name="type" value="detail" />
                    <jsp:param name="image" value="${book.getImageUrlM()}" />
                    <jsp:param name="title" value="${book.getTitle()}" />
                    <jsp:param name="author" value="${book.getAuthor()}" />
                    <jsp:param name="isbn" value="${book.getIsbn()}" />
                    <jsp:param name="rating" value="${rating}" />
                    <jsp:param name="category" value="${book.getGenre()}" />
                    <jsp:param name="publisher" value="${book.getPublisher()}" />
                    <jsp:param name="published_date" value="${book.getYearOfPublication()}" />
                    <jsp:param name="language" value="${book.getLanguage()}" />
                    <jsp:param name="description" value="${book.getDescription()}" />
                    <jsp:param name="available_copies" value="${available_copies}" />
                </jsp:include>
            </c:otherwise>
        </c:choose>
    </section>
</div>