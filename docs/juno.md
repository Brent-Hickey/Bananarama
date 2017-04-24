# Welcome to Leapsight JUNO

{{TOC}}

JUNO is an open-source high-performance and scalable all-in-one API Management, Gateway and Services Networking Platform that implements the open Web Application Messaging Protocol (WAMP). It is specially designed for the requirements of distributed architectures based on micro-services (µServices) and the dynamics of Cloud and Internet Of Things (IoT) applications.

As opposed to other API management solutions, JUNO unifies Remote Procedure Call (RPC) and Publish-Subscribe messaging patterns over HTTP, WebSockets and TCP/IP under a single layer.
    
In addition, JUNO provides out-of-the-box clustering capabilities using an eventually consistent model that ensures high availability.

JUNO is implemented in Erlang which provides the underlying operating system to handle concurrency and scalability requirements.

## Architecture Key Drivers

### Why WAMP?
WAMP is an open standard WebSocket subprotocol that provides two application messaging patterns in one unified protocol: Remote Procedure Calls + Publish & Subscribe. Using WAMP you can build distributed systems out of application components which are loosely coupled and communicate in (soft) real-time.

### Why Erlang?
Erlang is a wonderful programming language and platform for concurrent and (soft) real-time applications. 

### Why an Eventually Consistent Model?
Because we want JUNO to be scaleable and always-on. 

It is really stupid to design super scaleable and fault-tolerant backend platforms using NoSQL databases, eventually consistency and more sophisticated techniques like CRDTs only to then define an API layer on top that relies on a strong consistency model. This usually happens when you use an API Gateway that relies on a RDBMS for data replication. All the hard work you’ve done in the backend to provide an always-on system is then hampered by an entry point which is not!

### Why 
## Limits
All limits are imposed per Realm

* Rate Limiting - DoS Hardening
    * Per IP
    * Per Peer ({IP, Port})
* Quotas
    * Per Realm
    * Per Peer ({IP, Port})