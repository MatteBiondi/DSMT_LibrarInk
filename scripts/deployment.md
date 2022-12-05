# Librarink deploy

The Librarink application is composed by several components:

* Glassfish application server
* Erlang server
  * Proxy server
  * Mnesia database
  * Web(socket) server
* MySQL DB server
* RabbitMQ server

The components and subcomponents are distributed and replicated among the available machines.

The current configuration uses 6 Ubuntu machines<sup>1</sup>, available at the following addresses:

* 172.18.0.28
* 172.18.0.29
* 172.18.0.30
* 172.18.0.31
* 172.18.0.32
* 172.18.0.33

 <sup>1</sup> The machines are actually provided through virtualization

---
## Distribution

The components are distributed in order to improve performance, splitting the load on
multiple machines.
There are more components than available machines, therefore some component are place on same machine.
However, the choices are not random, but take into account the needs of communication and the load that a
component is supposed to handle.  

* Websocket server and RabbitMQ are on the same physical machine, but different Erlang node, reducing the latency
  of communication
* Glassfish AS and MySQl DB are place together in order to minimize client response time
  
---

## Replication

The only components that are replicated for data availability and fault tolerance are the Mnesia DB
replicas.
The distribution is made in such a way that replica and relative backup are on different
physical machine, so in case of hardware failure of a physical node data will be still available.

---

## Deployment configuration

| Component               | IP address  | Erlang Node     |
|-------------------------|-------------|-----------------|
| Glassfish AS            | 172.18.0.28 | -               |
| MySQL DB server         | 172.18.0.29 | -               |
| Proxy server            | 172.18.0.29 | Proxy           |
| Web server              | 172.18.0.30 | Cowboy          |
| RabbitMQ server         | 172.18.0.30 | RabbitMQ        |
| Mnesia replica 1        | 172.18.0.31 | Mnesia active 1 |
| Mnesia replica 2        | 172.18.0.32 | Mnesia active 2 |
| Mnesia replica 3        | 172.18.0.33 | Mnesia active 3 |
| Mnesia replica 1 backup | 172.18.0.32 | Mnesia backup 1 |
| Mnesia replica 2 backup | 172.18.0.33 | Mnesia backup 2 |
| Mnesia replica 3 backup | 172.18.0.31 | Mnesia backup 3 |

## Scripts

The deployment phase is automated via the shell batch\bash script *deploy*.

If some problems arises check if the line *"[ -z "$PS1" ] && return"* is commented in the file *~/.bashrc*
