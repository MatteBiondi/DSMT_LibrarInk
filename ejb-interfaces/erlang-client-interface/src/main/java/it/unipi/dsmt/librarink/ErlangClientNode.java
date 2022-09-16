package it.unipi.dsmt.librarink;

import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpMbox;

import javax.ejb.Local;

@Local
public interface ErlangClientNode {
    OtpMbox getMbox() throws NullPointerException;
    OtpErlangRef makeRef() throws NullPointerException;
}
