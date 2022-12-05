# Software installation and configuration

Librarink relies on some external components in order to build and run the application, following steps are
necessary to prepare the environment.

*Note that some tool can be installed and configured only on one physical machine*

## MySQL

```bash
apt update && apt install mysql-server
sed -i "/^bind-address/c\bind-address\t\t= 0.0.0.0" /etc/mysql/mysql.conf.d/mysqld.cnf
service mysql start
mysql -uroot -proot
CREATE USER 'librarink'@'%' IDENTIFIED WITH mysql_native_password  BY 'root';
CREATE DATABASE librarink;
GRANT ALL PRIVILEGES ON librarink.* TO 'librarink'@'%' WITH GRANT OPTION;
FLUSH PRIVILEGES;
 ```

### MySQL credentials

* root\root (admin)
* librarink\root (librarink)

---

## RabbitMQ

```bash
curl -s https://packagecloud.io/install/repositories/rabbitmq/rabbitmq-server/script.deb.sh | bash
apt-get install rabbitmq-server=3.10.7-1
service rabbitmq-server start
rabbitmq-plugins enable rabbitmq_management
rabbitmqctl add_user librarink root
rabbitmqctl set_user_tags librarink administrator
rabbitmqctl set_permissions -p / librarink ".*" ".*" ".*"
```

### RabbitMQ credentials

* guest\guest         (local only)
* librarink\root      (local and remote)

---

## Rebar3

```bash
git clone https://github.com/erlang/rebar3.git
cd rebar3
./bootstrap
./rebar3 local install
echo 'export PATH=$PATH:~/.cache/rebar3/bin' >> ~/.bashrc
```

---

## Maven

```bash
apt update && apt install maven
update-alternatives --config java # Select Java 1.8 
```

---

## Glassfish

```bash
echo 'export PATH=$PATH:~/servers/glassfish5/glassfish/bin/' >> ~/.bashrc
```

---
