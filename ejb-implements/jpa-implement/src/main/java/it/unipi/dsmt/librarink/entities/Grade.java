package it.unipi.dsmt.librarink.entities;

import javax.persistence.*;

@Entity
@Table(name="grade")
@IdClass(GradeKey.class)
public class Grade {
    @Id
    @Column(name="user_email")
    String user_email;
    @Id
    @Column(name="isbn")
    String isbn;
    @Column(name = "stars")
    Float stars;

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

    public Float getStars() {
        return stars;
    }

    public void setStars(Float stars) {
        this.stars = stars;
    }

    public GradeKey getKey()
    {
        return new GradeKey(this.user_email, this.isbn);
    }
}
