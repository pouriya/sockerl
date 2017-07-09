![sockerl travis test status](https://travis-ci.org/Pouriya-Jahanbakhsh/sockerl.png?branch=master)

# SockErl
### Overview  
Sockerl is an advanced Erlang/Elixir socket library for TCP protocols and provides fast, useful and easy-to-use API for implementing server, clients and connection pools.  




### Features  
* In server, client and client pool implementation every connection has its own process and Sockerl provides useful and simple API for accessing them and sending data through them or sending Erlang message to them, closing them etc.  

* Every connection process can handle generic calls (requests that have made using `gen_server:call/2-3` or `gen:call/3-4`), generic casts (requests that have made using `gen_server:cast/2`), generic events (messages in form of `{'$gen_event', Event}`), disconnecting (when other side closes connection), [Erlang system messages](http://erlang.org/doc/design_principles/spec_proc.html#msg), etc.

* Acceptors and Connectors have been written as `Special Erlang process` and they are faster than `gen_server`, `gen_fsm`, etc.  

* Blocking and non-blocking sockets are supported with doing a few changes in your code.  

* SSL is supported too (you can define your own `sockerl_transporter` behavior for any protocol like SSL).  

* Connects to multiple addresses and has multiple connections to every address in client pools.  

* Add new Client(s) to existing pool.  

* Clean and human-understandable error outputs.  

* Accepts standard OTP [sys debug options](https://github.com/erlang/otp/blob/OTP-20.0/lib/stdlib/src/sys.erl#L46) for Acceptors and Connections for generating clean debug output.  

* Simple API for controlling crash of connection processes.  

All features not listed here.  



***
# Download source

##### Git
```sh
~ $ git clone --branch 17.7.10 https://github.com/Pouriya-Jahanbakhsh/sockerl.git
```

# Download compiled
For compiled source and its dependencies download **lib.tar.gz** from [**Releases**](https://github.com/Pouriya-Jahanbakhsh/sockerl/releases)

# Use as dependency

##### Rebar3
Put this in `deps` in `rebar.config`:
```erlang
{sockerl, "17.7.10"}
```


##### Rebar
Put this in `deps` in `rebar.config`:  
```erlang
{sockerl, ".*", {git, "https://github.com/Pouriya-Jahanbakhsh/sockerl.git", {tag, "17.7.10"}}}
```

##### Mix
Put this in deps in `mix.exs`:  
```erlang
{:sockerl, "~> 17.7.10"}
```


#### erlang.mk
```sh
dep_sockerl = hex 17.7.10
```




### Quick start

### Simple Non-blocking server 
`echo_server.erl`:
```erlang
-module(echo_server).
-behaviour(sockerl).

%% API:
-export([start/1
        ,packet_count/1]).


%% sockerl callbacks:
-export([listen_init/2
        ,connector_init/2
        ,handle_packet/3
        ,handle_disconnect/2
        ,handle_call/4
        ,terminate/3
        ,code_change/3]).





start(Port) ->
    RegisterName = {local, ?MODULE}, %% Optional
    CallbackMod = ?MODULE,
    InitArg = null,
    StartOpts = [],
    sockerl:start_link_server(RegisterName
                             ,CallbackMod
                             ,InitArg
                             ,Port
                             ,StartOpts).





packet_count(Con) ->
    gen_server:call(Con, how_much).





listen_init(null, _ListenSock) ->
    {ok, null2}.





%% SMD: #sockerl_metadata{}, Erlang record containing useful things.
connector_init(null2, _SMD) ->
    {ok, [{state, 0}]}.





handle_packet(Pkt, Count = _MyState, _SMD) ->
    {ok, [{state, Count + 1}, {packet, Pkt}]}.





handle_call(how_much, From, Count, _SMD) ->
    {ok, [{reply, From, Count}]}.





handle_disconnect(_Count, _SMD) ->
    %% If client closes connection, 
    %% Server connection process will crash with reason 'normal'
    {stop, normal}.





terminate(_Reason, _State, _SMD) ->
    ok.





code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```
Compile and go to the Erlang shell (You should add path for `echo_server.beam` and `sockerl` and its dependencies):
```erlang
Erlang/OTP 19 [erts-8.2.2] [source-1ca84a4] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V8.2.2  (abort with ^G)
1> echo_server:start(8080).
{ok,<0.104.0>}

%% Getting all available server connections:
2> sockerl:get_server_connections(echo_server).
[]
```
Make a `telnet` connection to server:
```sh
$ telnet 127.0.0.1 8080

Trying 127.0.0.1...
Connected to 127.0.0.1.
Escape character is '^]'.
```
Get server connections again:
```erlang
3> sockerl:get_server_connections(echo_server).
[{#Port<0.7532>,<0.116.0>}]
```
Send some texts in `telnet` connection:
```sh
foo
foo
2
2
3
3
4
4
5
5
```
Server replies after sending every text.  
Check packet count for connection:
```erlang
%% Getting pid of our connection process
4> [{_, Con}] = sockerl:get_server_connections(echo_server).
[{#Port<0.7532>,<0.116.0>}]

5> echo_server:packet_count(Con).
5
```

For more info see [**Sockerl wiki**](https://github.com/Pouriya-Jahanbakhsh/sockerl/wiki)

### License
`BSD 3-Clause`

### Links
[**GitHub**](https://github.com/Pouriya-Jahanbakhsh/sockerl)  
This documentation is available in [**http://docs.codefather.org/sockerl**](http://docs.codefather.org/sockerl)
