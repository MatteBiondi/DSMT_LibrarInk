package it.unipi.dsmt.librarink;

import java.io.Serializable;

public class Librarink_gradesDTO implements Serializable {
    private String user_email;
    private String isbn;
    private float stars;

    public String getUser_email() {
        return user_email;
    }

    public void setUser_email(String user_email) {
        this.user_email = user_email;
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
                "user_email='" + user_email + '\'' +
                ", isbn='" + isbn + '\'' +
                ", stars=" + stars +
                '}';
    }
}
