<html>
<head>
    <title>Librarink</title>
    <link rel="stylesheet" href="./css/book.css" type="text/css" media="screen">
</head>
<body>
<h2>Catalog:</h2>
<!--Todo: fields for the search, the book filter and the 'for' loop with include (see below)-->
<!--Todo: if the results are> x then use paging-->
<jsp:include page="pages/common/book.jsp" >
    <jsp:param name="type" value="thumbnail" />
    <jsp:param name="image" value="https://images-na.ssl-images-amazon.com/images/I/71gjt76M3xL.jpg" />
    <jsp:param name="title" value="Il Signore degli anelli. Trilogia" />
    <jsp:param name="author" value="John R. R. Tolkien" />
    <jsp:param name="isbn" value="978-8845210273" />
    <jsp:param name="rate" value="5" />
</jsp:include>
</body>
</html>
