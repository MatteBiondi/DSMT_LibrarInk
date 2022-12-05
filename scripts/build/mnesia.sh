cd ~/Librarink/
rebar3 as mnesia_$1_$2_rel release -n librarink_mnesia_$1_$2 -o  mnesia_$1_$2
grep -q "export PATH=\$PATH:~/Librarink/mnesia_$1_$2/librarink_mnesia_$1_$2/bin" ~/.bashrc \
    || echo "export PATH=\$PATH:~/Librarink/mnesia_$1_$2/librarink_mnesia_$1_$2/bin" >> ~/.bashrc

 