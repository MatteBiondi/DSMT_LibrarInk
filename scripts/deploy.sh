IP=('172.18.0.28' '172.18.0.29' '172.18.0.30' '172.18.0.31' '172.18.0.32' '172.18.0.33')

for ip in "${IP[@]}"
do
    cd ../../

    # Create working directory
    ssh -f -n -i DSMT_Librarink/scripts/keys/dsmt.pem root@"$ip" "mkdir -p Librarink"

    # Copy erlang files
    scp -i DSMT_Librarink/scripts/keys/dsmt.pem -C -r \
    DSMT_Librarink/scripts/build \
    DSMT_Librarink/librarink-erlang/apps \
    DSMT_Librarink/librarink-erlang/config \
    DSMT_Librarink/librarink-erlang/rebar.config \
    root@"$ip":/root/Librarink

    cd DSMT_Librarink/scripts

    ## build and build
    
    # Proxy
    ssh -i keys/dsmt.pem root@"$ip" "bash Librarink/build/proxy.sh"

    # Websocket
    ssh -i keys/dsmt.pem root@"$ip" "bash Librarink/build/websocket.sh"

    # Mnesia active/backup 1
    ssh -i keys/dsmt.pem root@"$ip" "bash Librarink/build/mnesia.sh active 1"
    ssh -i keys/dsmt.pem root@"$ip" "bash Librarink/build/mnesia.sh backup 1"

    # Mnesia active/backup 2
    ssh -i keys/dsmt.pem root@"$ip" "bash Librarink/build/mnesia.sh active 2"
    ssh -i keys/dsmt.pem root@"$ip" "bash Librarink/build/mnesia.sh backup 2"

    # Mnesia active/backup 3
    ssh -i keys/dsmt.pem root@"$ip" "bash Librarink/build/mnesia.sh active 3"
    ssh -i keys/dsmt.pem root@"$ip" "bash Librarink/build/mnesia.sh backup 3"
done

# Glassfish
mvn -f  ../pom.xml clean deploy
