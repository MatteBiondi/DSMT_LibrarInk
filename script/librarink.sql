CREATE DATABASE  IF NOT EXISTS `librarink`
/*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci */
/*!80016 DEFAULT ENCRYPTION='N' */;
USE `librarink`;
-- MySQL dump 10.13  Distrib 8.0.30, for Win64 (x86_64)
--
-- Host: localhost    Database: librarink
-- ------------------------------------------------------
-- Server version	8.0.30

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `books`
--

DROP TABLE IF EXISTS `books`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `books` (
  `isbn` varchar(45) NOT NULL,
  `book_title` varchar(45) NOT NULL,
  `book_author` varchar(45) NOT NULL,
  `genre` varchar(45) NOT NULL,
  `year_of_publication` int NOT NULL,
  `publisher` varchar(45) NOT NULL,
  `description` varchar(700) NOT NULL DEFAULT 'Description is missing',
  `sum_of_stars` float(15,2) NOT NULL DEFAULT '0.00',
  `number_of_review` int NOT NULL DEFAULT '0',
  `image_url_s` varchar(100) NOT NULL,
  `image_url_m` varchar(100) NOT NULL,
  `image_url_l` varchar(100) NOT NULL,
  PRIMARY KEY (`isbn`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `books`
--

LOCK TABLES `books` WRITE;
/*!40000 ALTER TABLE `books` DISABLE KEYS */;
/*!40000 ALTER TABLE `books` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `grade`
--

DROP TABLE IF EXISTS `grade`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `grade` (
  `user_email` varchar(45) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `isbn` varchar(45) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `stars` float NOT NULL,
  PRIMARY KEY (`user_email`,`isbn`),
  KEY `grade_isbn_idx` (`isbn`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `grade`
--

LOCK TABLES `grade` WRITE;
/*!40000 ALTER TABLE `grade` DISABLE KEYS */;
/*!40000 ALTER TABLE `grade` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `history_reservation`
--

DROP TABLE IF EXISTS `history_reservation`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `history_reservation` (
  `user_email` varchar(45) NOT NULL,
  `isbn` varchar(45) NOT NULL,
  `start_date` date NOT NULL,
  `end_date` date DEFAULT NULL,
  `deleted` tinyint NOT NULL DEFAULT '0',
  PRIMARY KEY (`user_email`,`isbn`,`start_date`),
  KEY `isbn_res_idx` (`isbn`),
  CONSTRAINT `email_user_res` FOREIGN KEY (`user_email`) REFERENCES `users` (`email`) ON DELETE CASCADE,
  CONSTRAINT `isbn_res` FOREIGN KEY (`isbn`) REFERENCES `books` (`isbn`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `history_reservation`
--

LOCK TABLES `history_reservation` WRITE;
/*!40000 ALTER TABLE `history_reservation` DISABLE KEYS */;
/*!40000 ALTER TABLE `history_reservation` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `histrory_loan`
--

DROP TABLE IF EXISTS `histrory_loan`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `histrory_loan` (
  `user_email` varchar(45) NOT NULL,
  `isbn` varchar(45) NOT NULL,
  `id_copy` varchar(45) NOT NULL,
  `start_date` date NOT NULL,
  `end_date` date DEFAULT NULL,
  PRIMARY KEY (`isbn`,`id_copy`,`start_date`,`user_email`),
  KEY `email_loan__idx` (`user_email`),
  CONSTRAINT `email_loan` FOREIGN KEY (`user_email`) REFERENCES `users` (`email`) ON DELETE CASCADE,
  CONSTRAINT `isbn_loan` FOREIGN KEY (`isbn`) REFERENCES `books` (`isbn`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `histrory_loan`
--

LOCK TABLES `histrory_loan` WRITE;
/*!40000 ALTER TABLE `histrory_loan` DISABLE KEYS */;
/*!40000 ALTER TABLE `histrory_loan` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `users`
--

DROP TABLE IF EXISTS `users`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `users` (
  `email` varchar(45) NOT NULL,
  `password` varchar(64) NOT NULL,
  `name` varchar(45) NOT NULL,
  `surname` varchar(45) NOT NULL,
  `address` varchar(45) NOT NULL,
  `birthday` date NOT NULL,
  `image` varchar(100) DEFAULT 'https://cdn.onlinewebfonts.com/svg/img_335286.png',
  PRIMARY KEY (`email`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users`
--

LOCK TABLES `users` WRITE;
/*!40000 ALTER TABLE `users` DISABLE KEYS */;
/*!40000 ALTER TABLE `users` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `wishlist`
--

DROP TABLE IF EXISTS `wishlist`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `wishlist` (
  `email_user` varchar(45) NOT NULL,
  `isbn` varchar(45) NOT NULL,
  PRIMARY KEY (`isbn`,`email_user`),
  KEY `isbm_idx` (`isbn`),
  KEY `user_email_idx` (`email_user`),
  CONSTRAINT `email` FOREIGN KEY (`email_user`) REFERENCES `users` (`email`) ON DELETE CASCADE,
  CONSTRAINT `isbn` FOREIGN KEY (`isbn`) REFERENCES `books` (`isbn`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `wishlist`
--

LOCK TABLES `wishlist` WRITE;
/*!40000 ALTER TABLE `wishlist` DISABLE KEYS */;
/*!40000 ALTER TABLE `wishlist` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Dumping events for database 'librarink'
--

--
-- Dumping routines for database 'librarink'
--
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2022-10-31 15:22:00
