package it.unipi.dsmt.librarink;

import java.io.Serializable;

public class BookDTO implements Serializable {
    String isbn;
    String title;
    String author;
    String genre;
    String yearOfPublication;
    String publisher;
    String language;
    String imageUrlS;
    String imageUrlM;
    String imageUrlL;
    String description;

    public String getIsbn() {
        return isbn;
    }

    public void setIsbn(String isbn) {
        this.isbn = isbn;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getAuthor() {
        return author;
    }

    public void setAuthor(String author) {
        this.author = author;
    }

    public String getGenre() {
        return genre;
    }

    public void setGenre(String genre) {
        this.genre = genre;
    }

    public String getYearOfPublication() {
        return yearOfPublication;
    }

    public void setYearOfPublication(String yearOfPublication) {
        this.yearOfPublication = yearOfPublication;
    }

    public String getPublisher() {
        return publisher;
    }

    public void setPublisher(String publisher) {
        this.publisher = publisher;
    }

    public String getLanguage() {
        return language;
    }

    public void setLanguage(String language) {
        this.language = language;
    }

    public String getImageUrlS() {
        return imageUrlS;
    }

    public void setImageUrlS(String imageUrlS) {
        this.imageUrlS = imageUrlS;
    }

    public String getImageUrlM() {
        return imageUrlM;
    }

    public void setImageUrlM(String imageUrlM) {
        this.imageUrlM = imageUrlM;
    }

    public String getImageUrlL() {
        return imageUrlL;
    }

    public void setImageUrlL(String imageUrlL) {
        this.imageUrlL = imageUrlL;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    @Override
    public String toString() {
        return "libraink_booksDTO{" +
                "isbn='" + isbn + '\'' +
                ", book_title='" + title + '\'' +
                ", genre='" + genre + '\'' +
                ", year_of_publication='" + yearOfPublication + '\'' +
                ", publisher='" + publisher + '\'' +
                ", image_url_s='" + imageUrlS + '\'' +
                ", image_url_m='" + imageUrlM + '\'' +
                ", image_url_l='" + imageUrlL + '\'' +
                '}';
    }
}
