package it.unipi.dsmt.librarink.entities;

public class GradeKey {
    private String user_email;
    private String isbn;

    public GradeKey(String user_email, String isbn) {
        this.user_email = user_email;
        this.isbn = isbn;
    }

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
}
