cd ~/Librarink/
rebar3 as websocket_rel release -n librarink_websocket -o websocket
grep -q "export PATH=\$PATH:~/Librarink/websocket/librarink_websocket/bin" ~/.bashrc \
    || echo "export PATH=\$PATH:~/Librarink/websocket/librarink_websocket/bin" >> ~/.bashrc
