package it.unipi.dsmt.librarink.entities;

import javax.persistence.*;

@Entity
@Table(name="wishlist")
@IdClass(WishListKey.class)
public class Wishlist {
    @Id
    @Column(name="user")
    String user;
    @Id
    @Column(name="isbn")
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
    public WishListKey getKey()
    {
        return new WishListKey(user,isbn);
    }
}
