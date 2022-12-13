<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<%@ page contentType="text/html;charset=UTF-8" %>

<!-- Code in charge of visualizing book details in thumbnail or detailed way -->

<%
    String type = request.getParameter("type");
    final int MAX_RATE = 5;
%>

<% if (type != null && type.equals("thumbnail")) { %>
    <!--In case of list of books: not detailed representation -->
    <!-- Image + Rate + Title + Author -->

    <!-- Book card -->
    <div>
        <div id="<%= request.getParameter("isbn") %>" class="thumbnail card h-100" data-isbn="<%= request.getParameter("isbn") %>">
            <img src="<%= request.getParameter("image")%>" alt="Book cover image" class="card-img-top h-50">
            <div class="info_container card-body h-50">
                <h3>Title: </h3><p><%= request.getParameter("title")%></p>
                <h3>Author: </h3><p><%= request.getParameter("author")%></p>
            </div>
        </div>
    </div>
<% } else { %>
    <!--In case of a certain book: detailed representation -->
    <!--
    Image + Vote your self
    Title + Author + Category + Publisher + Published Date + Language + Description + Rate +
    Reserve button + Wishlist button
    -->

    <html>
    <head>
        <link rel="stylesheet" href="./css/book.css" type="text/css" media="screen">
        <title><%= request.getParameter("title")%></title>
    </head>
    <body>
        <h1 class="page_title"><%= request.getParameter("title")%></h1>
        <div class="detailed" id="<%= request.getParameter("isbn")%>">
            <div class="right_column">

                <!-- Section containing all the book information -->
                <div class="book_info">
                    <h3>Title:</h3>
                    <p id="title"><%= request.getParameter("title")%> </p>
                    <br>
                    <h3>Author:</h3>
                    <p id="author"><%= request.getParameter("author")%></p>
                    <br>
                    <h3>ISBN:</h3>
                    <p id="isbn"><%= request.getParameter("isbn")%></p>
                    <br>
                    <h3>Category:</h3>
                    <p id="category"><%= request.getParameter("category")%></p>
                    <br>
                    <h3>Publisher:</h3>
                    <p id="publisher"><%= request.getParameter("publisher")%></p>
                    <br>
                    <h3>Published date:</h3>
                    <p id="published_date"><%= request.getParameter("published_date")%></p>
                    <br>
                    <h3>Language:</h3>
                    <p id="language"><%= request.getParameter("language")%></p>
                </div>

                <!-- Short book description-->
                <h3>Brief Description: </h3>
                <p><%= request.getParameter("description")%></p>

                <!-- Rating section -->
                <h3>Users rate the book:
                    <span id="avg_rate">
                        <jsp:useBean id="df" scope="request" type="java.text.DecimalFormat"/>
                        <jsp:useBean id="rating" scope="request" type="java.lang.Double"/>
                        <c:choose>
                            <c:when test="${rating != null}">
                                <span id="user-rating">${df.format(rating)}</span>/<%= MAX_RATE %>
                            </c:when>
                            <c:otherwise><span id="user-rating">-</span>/<%= MAX_RATE %></c:otherwise>
                        </c:choose>
                    </span>
                </h3>
                <h3>Number of available copies:
                    <span id="copies_counter"><%= request.getParameter("available_copies")%></span>
                </h3>
                <div class="stars_div">
                    <span data-grade="5" class="star s5">&#x2605;</span>
                    <span data-grade="4" class="star s4">&#x2605;</span>
                    <span data-grade="3" class="star s3">&#x2605;</span>
                    <span data-grade="2" class="star s2">&#x2605;</span>
                    <span data-grade="1" class="star s1">&#x2605;</span>
                </div>
            </div>

            <div class="left_column">
                <!-- Image and buttons section -->
                <img src="<%= request.getParameter("image")%>" alt="Book cover image">
                <button class="btn btn-primary" id="reserve-btn">Reserve</button>
                <button class="btn btn-primary" id="wishlist-btn">Add to wishlist</button>
            </div>
        </div>
    </body>
    </html>
<%}%>
