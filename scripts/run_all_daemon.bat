ssh root@172.18.0.30 -i keys\dsmt.pem librarink_websocket daemon
ssh root@172.18.0.31 -i keys\dsmt.pem librarink_mnesia_active_1 daemon
ssh root@172.18.0.32 -i keys\dsmt.pem librarink_mnesia_active_2 daemon
ssh root@172.18.0.33 -i keys\dsmt.pem librarink_mnesia_active_3 daemon
ssh root@172.18.0.32 -i keys\dsmt.pem librarink_mnesia_backup_1 daemon
ssh root@172.18.0.33 -i keys\dsmt.pem librarink_mnesia_backup_2 daemon
ssh root@172.18.0.31 -i keys\dsmt.pem librarink_mnesia_backup_3 daemon
ssh root@172.18.0.29 -i keys\dsmt.pem librarink_proxy daemon
