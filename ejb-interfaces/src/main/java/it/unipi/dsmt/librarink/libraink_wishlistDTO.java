package it.unipi.dsmt.librarink;

import java.io.Serializable;

public class libraink_wishlistDTO implements Serializable {
    String email_user;
    String isbn;

    public String getEmail_user() {
        return email_user;
    }

    public void setEmail_user(String email_user) {
        this.email_user = email_user;
    }

    public String getIsbn() {
        return isbn;
    }

    public void setIsbn(String isbn) {
        this.isbn = isbn;
    }

    @Override
    public String toString() {
        return "libraink_wishlistDTO{" +
                "email_user='" + email_user + '\'' +
                ", isbn='" + isbn + '\'' +
                '}';
    }
}
