package it.unipi.dsmt.librarink.entities;

import javax.persistence.*;

@Entity
@Table(name="wishlist")
public class Wishlist {
    @Id
    @Column(name="email_user")
    String email_user;
    @Id
    @Column(name="isbn")
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
    public WishListKey getKey()
    {
        return new WishListKey(email_user,isbn);
    }
}
