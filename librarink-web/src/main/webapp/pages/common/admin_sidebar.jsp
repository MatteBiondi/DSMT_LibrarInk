<%@ page contentType="text/html;charset=UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<jsp:useBean id="table" scope="request" type="java.lang.String"/>

<%--@declare id="reservation"--%>
<%--@declare id="loan"--%>
<div id="menu" class="d-flex flex-column flex-shrink-0 p-3 text-bg-dark">
    <a href="${pageContext.request.contextPath}/admin"
       class="d-flex align-items-center mb-3 mb-md-0 me-md-auto text-white text-decoration-none">
        <span class="fs-4">Librarink</span>
    </a>
    <hr>
    <ul id="commands" class="nav nav-pills flex-column mb-auto">
        <c:choose>
            <c:when test="${table.equals(\"active\")}">
                <li>
                    <button class="btn btn-outline-light"
                            onclick="window.location.assign('<%= request.getContextPath()%>/adminHistory')">
                        History table
                    </button>
                </li>
            </c:when>
            <c:when test="${table.equals(\"history\")}">
                <li>
                    <button class="btn btn-outline-light"
                            onclick="window.location.assign('<%= request.getContextPath()%>/admin')">
                        Active table
                    </button>
                </li>
            </c:when>
        </c:choose>
        <li>
            <button class="btn btn-outline-light"
                    onclick="window.location.assign('<%= request.getContextPath()%>/logout')">Logout</button>
        </li>
        <li>
            <button class="btn btn-outline-light" onclick="window.location.reload()">Refresh</button>
        </li>
    </ul>
</div>
