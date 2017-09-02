![sockerl travis test status](https://travis-ci.org/Pouriya-Jahanbakhsh/sockerl.png?branch=master) [![Hex version](https://img.shields.io/hexpm/v/sockerl.svg "Hex version")](https://hex.pm/packages/sockerl)

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
~ $ git clone --branch 17.9.2 https://github.com/Pouriya-Jahanbakhsh/sockerl.git
```

# Download compiled
For compiled source and its dependencies download **lib.tar.gz** from [**Releases**](https://github.com/Pouriya-Jahanbakhsh/sockerl/releases)

# Use as dependency

##### Rebar3
Put this in `deps` in `rebar.config`:
```erlang
{sockerl, "17.9.2"}
```


##### Rebar
Put this in `deps` in `rebar.config`:  
```erlang
{sockerl, ".*", {git, "https://github.com/Pouriya-Jahanbakhsh/sockerl.git", {tag, "17.9.2"}}}
```

##### Mix
Put this in deps in `mix.exs`:  
```erlang
{:sockerl, "~> 17.9.2"}
```


#### erlang.mk
```sh
dep_sockerl = hex 17.9.2
```

# Documentation
See [**Wiki**](https://github.com/Pouriya-Jahanbakhsh/sockerl/wiki)

### License
`BSD 3-Clause`

### Links
[**GitHub**](https://github.com/Pouriya-Jahanbakhsh/sockerl)  
This documentation is available in [**http://docs.codefather.org/sockerl**](http://docs.codefather.org/sockerl)
