package it.unipi.dsmt.librarink.entities;

import javax.persistence.Column;
import javax.persistence.Id;
import java.io.Serializable;

public class WishListKey implements Serializable {
    private String email_user;
    private String isbn;
    public WishListKey() {

    }
    public WishListKey(String email_user, String isbn) {
        this.email_user = email_user;
        this.isbn = isbn;
    }

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
}
