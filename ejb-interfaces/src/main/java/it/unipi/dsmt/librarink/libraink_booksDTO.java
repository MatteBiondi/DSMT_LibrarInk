package it.unipi.dsmt.librarink;

public class libraink_booksDTO {
    String isbn;
    String book_title;
    String genre;
    int year_of_publication;
    String publisher;
    Float sum_of_stars;
    int number_of_review;
    String image_url_s;
    String image_url_m;
    String image_url_l;

    public String getIsbn() {
        return isbn;
    }

    public String getBook_title() {
        return book_title;
    }

    public String getGenre() {
        return genre;
    }

    public int getYear_of_publication() {
        return year_of_publication;
    }

    public String getPublisher() {
        return publisher;
    }

    public Float getSum_of_stars() {
        return sum_of_stars;
    }

    public int getNumber_of_review() {
        return number_of_review;
    }

    public String getImage_url_s() {
        return image_url_s;
    }

    public String getImage_url_m() {
        return image_url_m;
    }

    public String getImage_url_l() {
        return image_url_l;
    }

    public void setIsbn(String isbn) {
        this.isbn = isbn;
    }

    public void setBook_title(String book_title) {
        this.book_title = book_title;
    }

    public void setGenre(String genre) {
        this.genre = genre;
    }

    public void setYear_of_publication(int year_of_publication) {
        this.year_of_publication = year_of_publication;
    }

    public void setPublisher(String publisher) {
        this.publisher = publisher;
    }

    public void setSum_of_stars(Float sum_of_stars) {
        this.sum_of_stars = sum_of_stars;
    }

    public void setNumber_of_review(int number_of_review) {
        this.number_of_review = number_of_review;
    }

    public void setImage_url_s(String image_url_s) {
        this.image_url_s = image_url_s;
    }

    public void setImage_url_m(String image_url_m) {
        this.image_url_m = image_url_m;
    }

    public void setImage_url_l(String image_url_l) {
        this.image_url_l = image_url_l;
    }

    @Override
    public String toString() {
        return "libraink_booksDTO{" +
                "isbn='" + isbn + '\'' +
                ", book_title='" + book_title + '\'' +
                ", genre='" + genre + '\'' +
                ", year_of_publication=" + year_of_publication +
                ", publisher='" + publisher + '\'' +
                ", sum_of_stars=" + sum_of_stars +
                ", number_of_review=" + number_of_review +
                ", image_url_s='" + image_url_s + '\'' +
                ", image_url_m='" + image_url_m + '\'' +
                ", image_url_l='" + image_url_l + '\'' +
                '}';
    }
}
