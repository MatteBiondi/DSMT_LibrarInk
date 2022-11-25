package it.unipi.dsmt.librarink;

import java.io.Serializable;

public class WishlistDTO implements Serializable {
    String user;
    String isbn;

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

    @Override
    public String toString() {
        return "libraink_wishlistDTO{" +
                "user='" + user + '\'' +
                ", isbn='" + isbn + '\'' +
                '}';
    }
}
