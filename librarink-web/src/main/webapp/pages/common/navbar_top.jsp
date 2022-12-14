<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<!-- Code in charge of show the navbar section including homepage link, searchbar and
user utilities (notification, user page, logout) -->

<script src="${pageContext.request.contextPath}/scripts/navbar.js"></script>
<link rel="stylesheet" href="${pageContext.request.contextPath}/css/navbar_top.css" type="text/css" media="screen">
<link rel="stylesheet" href="${pageContext.request.contextPath}/css/message.css" type="text/css" media="screen">

<nav class="navbar-top navbar navbar-light bg-light">
    <!-- "Librarink" link to homepage -->
    <a class="navbar-brand" href="${pageContext.request.contextPath}/homepage"><header id="header">Librarink</header></a>

    <!-- Search bar -->
    <c:if test="${ param.search_bar == \"true\" }">
        <div class="d-flex align-items-center" id="search">
            <div class="input-group">
                <button type="button" class="btn btn-primary">Search by</button>
                <input id="search-text" type="text" class="form-control">
                <div class="btn-group">
                    <button id="search-chooser" type="button" data-bs-toggle="dropdown"
                            class="btn btn-primary dropdown-toggle dropdown-toggle-split"></button>
                    <i id="search-clear" class="bi bi-x-circle"></i>
                    <div class="dropdown-menu">
                        <span class="dropdown-item search-item">Title</span>
                        <span class="dropdown-item search-item">Author</span>
                        <span class="dropdown-item search-item">Isbn</span>
                    </div>
                </div>
            </div>
        </div>
    </c:if>

    <!-- Notification icon and user icon for logout and profile page -->
    <div class="d-flex align-items-center" id="icons">
        <div id="notification" class="dropdown">
            <a class="d-flex align-items-center text-reset hidden-arrow" href="#" data-bs-toggle="dropdown">
                <span id="notification-counter" data-counter="0" class="badge rounded-pill badge-notification
                      bg-danger"><!-- Filled by websocket --></span>
                <i class="bi bi-bell-fill"></i>
            </a>
            <ul id="notification-items" class="dropdown-menu dropdown-menu-end">
                <!-- Filled by websocket -->
            </ul>
        </div>
        <div id="user" class="dropdown">
            <a class="text-reset hidden-arrow dropdown-toggle" href="#" data-bs-toggle="dropdown">
                <i class="bi bi-person-circle"></i>
            </a>
            <ul class="dropdown-menu dropdown-menu-end">
                <li>
                    <a class="dropdown-item" href="${pageContext.request.contextPath}/user">My profile</a>
                </li>
                <li>
                    <a id="logout" class="dropdown-item" href="${pageContext.request.contextPath}/logout">Logout</a>
                </li>
            </ul>
        </div>
    </div>
</nav>