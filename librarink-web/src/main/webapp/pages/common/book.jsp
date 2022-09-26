<%--
  Created by IntelliJ IDEA.
  User: Matteo
  Date: 13/09/2022
  Time: 18:07
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" %>
<%
    String type = request.getParameter("type");
    final int MAX_RATE = 5;
%>
<!--In case of list of books: not detailed representation -->
<!-- Image + Rate + Title + Author -->
<% if (type != null && type.equals("thumbnail")) { %>
    <a href = "<%= request.getParameter("link")%>" id = "<%= request.getParameter("isbn")%>"> <!--todo add anchor
    link-->
        <div class="thumbnail">
            <img src="<%= request.getParameter("image")%>" alt="Book cover image">
            <div class="info_container">
                <h3>Title: </h3><p><%= request.getParameter("title")%></p>
                <h3>Author: </h3><p><%= request.getParameter("author")%></p>
                <h3>Rate: </h3><p><%= request.getParameter("rate")%>/<%= MAX_RATE %></p>
            </div>
        </div>
    </a>
<% } else { %>
    <html>
    <head>
        <link rel="stylesheet" href="./css/book.css" type="text/css" media="screen">
        <title><%= request.getParameter("title")%></title>
    </head>
    <body>
    <!--In case of a certain book: detailed representation -->
    <!--
    Image + Vote your self
    Title + Author + Category + Publisher + Published Date + Language + Description + Rate +
    Reserve button + Whishlist button
    -->
        <h1 class="page_title"><%= request.getParameter("title")%></h1>
        <div class="detailed" id="<%= request.getParameter("isbn")%>">
            <div class="right_column">
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
                <h3>Brief Description: </h3>
                <p><%= request.getParameter("description")%></p>
                <h3>Users rate the book:
                    <span id="avg_rate"><%= request.getParameter("rate")%></span>/<%= MAX_RATE %>
                </h3>
                <h3>Number of available copies:
                    <span id="copies_counter"><%= request.getParameter("available_copies")%></span>
                </h3>
                <div class="stars_div">
                    <!--todo: vote if it is the first time or otherwise change the vote-->
                    <!--todo: Add js code-->
                    <a href="#" class="star s1">&#x2605;</a>
                    <a href="#" class="star s2">&#x2605;</a>
                    <a href="#" class="star s3">&#x2605;</a>
                    <a href="#" class="star s4">&#x2605;</a>
                    <a href="#" class="star s5">&#x2605;</a>
                </div>
            </div>
            <div class="left_column">
                <img src="<%= request.getParameter("image")%>" alt="Book cover image">
                <button>Reserve</button>
                <button>Add to wishlist</button>
            </div>
        </div>
    </body>
    </html>
<%}%>
