package it.unipi.dsmt.librarink.entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name="books")
public class Books {
    @Id
    @Column(name = "isbn")
    String isbn;
    @Column(name = "book_title")
    String book_title;
    @Column(name = "book_author")
    String book_author;
    @Column(name = "genre")
    String genre;
    @Column(name = "year_of_publication")
    String year_of_publication;
    @Column(name = "publisher")
    String publisher;
    @Column(name = "description")
    String description;
    @Column(name = "sum_of_stars")
    Float sum_of_stars;
    @Column(name = "number_of_review")
    Integer number_of_review;
    @Column(name = "image_url_s")
    String image_url_s;
    @Column(name = "image_url_m")
    String image_url_m;
    @Column(name = "image_url_l")
    String image_url_l;

    public String getIsbn() {
        return isbn;
    }

    public void setIsbn(String isbn) {
        this.isbn = isbn;
    }

    public String getBook_title() {
        return book_title;
    }

    public void setBook_title(String book_title) {
        this.book_title = book_title;
    }

    public String getBook_author() {
        return book_author;
    }

    public void setBook_author(String book_author) {
        this.book_author = book_author;
    }

    public String getGenre() {
        return genre;
    }

    public void setGenre(String genre) {
        this.genre = genre;
    }

    public String getYear_of_publication() {
        return year_of_publication;
    }

    public void setYear_of_publication(String year_of_publication) {
        this.year_of_publication = year_of_publication;
    }

    public String getPublisher() {
        return publisher;
    }

    public void setPublisher(String publisher) {
        this.publisher = publisher;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Float getSum_of_stars() {
        return sum_of_stars;
    }

    public void setSum_of_stars(Float sum_of_stars) {
        this.sum_of_stars = sum_of_stars;
    }

    public Integer getNumber_of_review() {
        return number_of_review;
    }

    public void setNumber_of_review(Integer number_of_review) {
        this.number_of_review = number_of_review;
    }

    public String getImage_url_s() {
        return image_url_s;
    }

    public void setImage_url_s(String image_url_s) {
        this.image_url_s = image_url_s;
    }

    public String getImage_url_m() {
        return image_url_m;
    }

    public void setImage_url_m(String image_url_m) {
        this.image_url_m = image_url_m;
    }

    public String getImage_url_l() {
        return image_url_l;
    }

    public void setImage_url_l(String image_url_l) {
        this.image_url_l = image_url_l;
    }
}
