Librarink
=====

An OTP application handling concurrent operations of a library website

Build
-----
    $ rebar3 compile

Build docs
-----
    $ rebar3 edoc

Manual test
-----
    $ docker run -d --name rabbitmq --hostname rabbitmq -p 5672:5672 -p5671:5671 -p 15672:15672 rabbitmq:3-management
    $ rebar3 shell
