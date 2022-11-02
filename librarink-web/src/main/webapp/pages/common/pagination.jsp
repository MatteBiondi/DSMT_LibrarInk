<%@ page contentType="text/html;charset=UTF-8" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<ul class="pagination">
    <li class="page-item"><span class="page-link" data-offset="${page_offset-1}">
        Previous</span></li>
    <c:forEach items="${offsets}" var="i">
        <li class="page-item offset" >
            <span class="page-link"
               <c:if test="${i == page_offset}">style="background-color: lightsteelblue"</c:if>
               data-offset="${i}">
                    ${i}
            </span>
        </li>
    </c:forEach>
    <li class="page-item"><span class="page-link" data-offset="${page_offset+1}">Next</span></li>
</ul>