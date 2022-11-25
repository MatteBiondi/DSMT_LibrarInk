package it.unipi.dsmt.librarink;

import java.io.Serializable;

public class GradeDTO implements Serializable {
    private String user;
    private String isbn;
    private float stars;

    public String getUser() {
        return user;
    }

    public void setUser(String user_email) {
        this.user = user_email;
    }

    public String getIsbn() {
        return isbn;
    }

    public void setIsbn(String isbn) {
        this.isbn = isbn;
    }

    public float getStars() {
        return stars;
    }

    public void setStars(float stars) {
        this.stars = stars;
    }

    @Override
    public String toString() {
        return "Librarink_gradesDTO{" +
                "user_email='" + user + '\'' +
                ", isbn='" + isbn + '\'' +
                ", stars=" + stars +
                '}';
    }
}
