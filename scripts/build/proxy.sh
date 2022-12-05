cd ~/Librarink/
rebar3 as proxy_rel release -n librarink_proxy -o proxy
grep -q "export PATH=\$PATH:~/Librarink/proxy/librarink_proxy/bin" ~/.bashrc \
    || echo "export PATH=\$PATH:~/Librarink/proxy/librarink_proxy/bin" >> ~/.bashrc
