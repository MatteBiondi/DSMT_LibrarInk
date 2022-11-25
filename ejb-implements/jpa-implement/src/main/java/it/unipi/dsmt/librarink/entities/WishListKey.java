package it.unipi.dsmt.librarink.entities;

import java.io.Serializable;

public class WishListKey implements Serializable {
    private String user;
    private String isbn;
    public WishListKey() {

    }
    public WishListKey(String user, String isbn) {
        this.user = user;
        this.isbn = isbn;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public String getIsbn() {
        return isbn;
    }

    public void setIsbn(String isbn) {
        this.isbn = isbn;
    }
}
