package it.unipi.dsmt.librarink.entities;

import javax.persistence.*;

@Entity
@Table(name="grade")
@IdClass(GradeKey.class)
public class Grade {
    @Id
    @Column(name="user")
    String user;
    @Id
    @Column(name="isbn")
    String isbn;
    @Column(name = "stars")
    Float stars;

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

    public Float getStars() {
        return stars;
    }

    public void setStars(Float stars) {
        this.stars = stars;
    }

    public GradeKey getKey()
    {
        return new GradeKey(this.user, this.isbn);
    }
}
