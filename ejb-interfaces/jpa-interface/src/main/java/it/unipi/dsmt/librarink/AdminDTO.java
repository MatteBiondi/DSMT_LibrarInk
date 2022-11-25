package it.unipi.dsmt.librarink;

import java.io.Serializable;
import java.util.Objects;

public class AdminDTO implements Serializable {
    private String email;
    private String password;

    public void setEmail(String email){this.email = email;}

    public void setPassword(String password){this.password = password;}

    public String getEmail() {
        return email;
    }

    public String getPassword() {
        return password;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AdminDTO entity = (AdminDTO) o;
        return Objects.equals(this.email, entity.email) &&
                Objects.equals(this.password, entity.password);
    }

    @Override
    public int hashCode() {
        return Objects.hash(email, password);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" +
                "email = " + email + ", " +
                "password = " + password + ")";
    }
}
