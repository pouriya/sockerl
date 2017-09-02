%%% ------------------------------------------------------------------------------------------------
%%% Sockerl is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2017-2018, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author  Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version 17.9.2
%% -------------------------------------------------------------------------------------------------

-module(sockerl).
-author("pouriya.jahanbakhsh@gmail.com").


%% -------------------------------------------------------------------------------------------------
%% Exports:





%% API:
-export([start_link_server/3
        ,start_link_server/4
        ,start_link_server/5
        ,stop_server/1
        ,stop_server/2
        ,get_acceptors/1
        ,sleep_acceptors/1
        ,wakeup_acceptors/1
        ,get_acceptor_modes/1
        ,change_acceptor_modes/1
        ,get_server_connections/1

        ,start_link_connector_pool/3
        ,start_link_connector_pool/4
        ,start_link_connector_pool/5
        ,stop_pool/1
        ,stop_pool/2
        ,get_pool_connections/1
        ,add_connector/3

        ,start_link_connector/4
        ,start_link_connector/5
        ,start_link_connector/6
        ,send_sync/2
        ,send_sync/3
        ,send_async/2
        ,stop_connector/1
        ,stop_connector/2]).





%% -------------------------------------------------------------------------------------------------
%% Behaviour info:





%% Mandatory for server implementation.
%%-callback
%%listen_init(InitArg, ListenSock) ->
%%    'ok'             |
%%    {'ok', InitArg2} |
%%    {'stop', Reason} |
%%    'ignore'
%%when
%%    InitArg :: any(),
%%    ListenSock ::  sockerl_types:socket(),
%%    InitArg2 :: any(),
%%    Reason :: any().







-callback
connector_init(InitArg, ConnectionSock) ->
    'ok'                    | %% your State will be atom 'undefined'
    {'ok', Opts}            |
    'close'                 |
    {'close', Opts}         |
    {'stop', Reason}        |
    {'stop', Reason, Opts}  |
    'ignore'
when
    InitArg :: any(),
    ConnectionSock ::  sockerl_types:socket(),
    Opts :: [] | [Opt],
    Opt :: {'state', any()}
         | {'timeout', timeout()}
         | {'srtimeout', timeout()}
         | {'length', sockerl_types:length()}
         | {'packet', sockerl_types:packet()}
         | {'transporter', module()}
         | {'socket', sockerl_types:socket()}
         | {'setopts', list()},
    Reason :: any().







-callback
handle_packet(Packet, State, Metadata) ->
    'ok'                    |
    {'ok', Opts}            |
    'close'                 |
    {'close', Opts}         |
    {'stop', Reason}        |
    {'stop', Reason, Opts} 
when     
    Packet :: sockerl_types:packet(),
    State :: any(),
    Metadata ::  sockerl_types:metadata(),
    Opts :: [] | [Opt],
    Opt :: {'state', any()}
         | {'timeout', timeout()}
         | {'srtimeout', timeout()}
         | {'length', sockerl_types:length()}
         | {'packet', sockerl_types:packet()}
         | {'transporter', module()}
         | {'socket', sockerl_types:socket()}
         | {'setopts', list()},
    Reason :: any().







%% Mandatory when you want to send call to connections ('$gen_call')
%%-callback
%%handle_call(Request, From, State, Metadata) ->
%%    'ok'                    |
%%    {'ok', Opts}            |
%%    'close'                 |
%%    {'close', Opts}         |
%%    {'stop', Reason}        |
%%    {'stop', Reason, Opts}
%%when
%%    Request :: any(),
%%    From :: {pid(), ref()},
%%    State :: any(),
%%    Metadata ::  sockerl_types:metadata(),
%%    Opts :: [] | [Opt],
%%    Opt :: {'state', any()}
%%         | {'reply', From, Response} % Just use in handle_call/4
%%         | {'timeout', timeout()}
%%         | {'srtimeout', timeout()}
%%         | {'length', sockerl_types:length()}
%%         | {'packet', sockerl_types:packet()}
%%         | {'transporter', module()}
%%         | {'socket', sockerl_types:socket()}
%%         | {'setopts', list()},
%%    Response :: any(),
%%    Reason :: any().







%% Mandatory when you want to send cast to connections ('$gen_cast')
%%-callback
%%handle_cast(Cast, State, Metadata) ->
%%    'ok'                    |
%%    {'ok', Opts}            |
%%    'close'                 |
%%    {'close', Opts}         |
%%    {'stop', Reason}        |
%%    {'stop', Reason, Opts}
%%when
%%    State :: any(),
%%    Metadata ::  sockerl_types:metadata(),
%%    Opts :: [] | [Opt],
%%    Opt :: {'state', any()}
%%         | {'timeout', timeout()}
%%         | {'srtimeout', timeout()}
%%         | {'length', sockerl_types:length()}
%%         | {'packet', sockerl_types:packet()}
%%         | {'transporter', module()}
%%         | {'socket', sockerl_types:socket()}
%%         | {'setopts', list()},
%%    Reason :: any().







%% Mandatory when you want to send event to connections ('$gen_event')
%%-callback
%%handle_event(Event, State, Metadata) ->
%%    'ok'                    |
%%    {'ok', Opts}            |
%%    'close'                 |
%%    {'close', Opts}         |
%%    {'stop', Reason}        |
%%    {'stop', Reason, Opts}
%%when
%%    Packet :: binary() | string(),
%%    State :: any(),
%%    Metadata ::  sockerl_types:metadata(),
%%    Opts :: [] | [Opt],
%%    Opt :: {'state', any()}
%%         | {'timeout', timeout()}
%%         | {'srtimeout', timeout()}
%%         | {'length', sockerl_types:length()}
%%         | {'packet', sockerl_types:packet()}
%%         | {'transporter', module()}
%%         | {'socket', sockerl_types:socket()}
%%         | {'setopts', list()},
%%    Reason :: any().







%% Useful when you want to use passive sockets (see README file) or
%% you want to use 'timeout' option.
%%-callback
%%timeout(State, Metadata) ->
%%    'ok'                    |
%%    {'ok', Opts}            |
%%    'close'                 |
%%    {'close', Opts}         |
%%    {'stop', Reason}        |
%%    {'stop', Reason, Opts}
%%when
%%    State :: any(),
%%    Metadata ::  sockerl_types:metadata(),
%%    Opts :: [] | [Opt],
%%    Opt :: {'state', any()}
%%         | {'timeout', timeout()}
%%         | {'srtimeout', timeout()}
%%         | {'length', sockerl_types:length()}
%%         | {'packet', sockerl_types:packet()}
%%         | {'transporter', module()}
%%         | {'socket', sockerl_types:socket()}
%%         | {'setopts', list()},
%%    Reason :: any().







%% Useful when you want to use passive sockets (see README file).
%%-callback
%%srtimeout(State, Metadata) ->
%%    'ok'                    |
%%    {'ok', Opts}            |
%%    'close'                 |
%%    {'close', Opts}         |
%%    {'stop', Reason}        |
%%    {'stop', Reason, Opts}
%%when
%%    State :: any(),
%%    Metadata ::  sockerl_types:metadata(),
%%    Opts :: [] | [Opt],
%%    Opt :: {'state', any()}
%%         | {'timeout', timeout()}
%%         | {'srtimeout', timeout()}
%%         | {'length', sockerl_types:length()}
%%         | {'packet', sockerl_types:packet()}
%%         | {'transporter', module()}
%%         | {'socket', sockerl_types:socket()}
%%         | {'setopts', list()},
%%    Reason :: any().







%% Useful when you want to use passive sockets (see README file).
-callback
handle_disconnect(State, Metadata) ->
    'ok'                    |
    {'ok', Opts}            |
    'close'                 |
    {'close', Opts}         |
    {'stop', Reason}        |
    {'stop', Reason, Opts}
when
    State :: any(),
    Metadata ::  sockerl_types:metadata(),
    Opts :: [] | [Opt],
    % Don't use {'packet', Pkt}, {'setopts, Opts} ... because socket has
    % been closed and after this terminate/3 will be called.
    Opt :: {'state', any()},
    Reason :: any().







-callback
terminate(Reason, State, Metadata) ->
    any()
when
    Reason :: any(),
    State :: any(),
    Metadata ::  sockerl_types:metadata().







-callback
code_change(OldVsn, State, Extra) ->
    {'ok', NewState}
    when
        OldVsn :: any(),
        State :: any(),
        Extra :: any(),
        NewState :: any().





%% -------------------------------------------------------------------------------------------------
%% API functions:





-spec
start_link_server(module(), term(), sockerl_types:port_number()) ->
    sockerl_types:start_return().
%% @doc
%%      Starts and links a socket server.
%% @end
start_link_server(Mod, InitArg, Port) ->
    sockerl_server_sup:start_link(Mod, InitArg, Port).







-spec
start_link_server(sockerl_types:register_name() | module()
                 ,module() | term()
                 ,term() | sockerl_types:port_number()
                 ,sockerl_types:port_number() |
                  sockerl_types:start_options()) ->
    sockerl_types:start_return().
%% @doc
%%      Starts and links a socket server.
%% @end
start_link_server(Name_or_Mod, Mod_or_InitArg, InitArg_or_Port, Port_or_Opts) ->
    sockerl_server_sup:start_link(Name_or_Mod, Mod_or_InitArg, InitArg_or_Port, Port_or_Opts).







-spec
start_link_server(sockerl_types:register_name()
                 ,module()
                 ,term()
                 ,sockerl_types:port_number()
                 ,sockerl_types:start_options()) ->
    sockerl_types:start_return().
%% @doc
%%      Starts and links a socket server.
%% @end
start_link_server(Name, Mod, InitArg, Port, Opts) ->
    sockerl_server_sup:start_link(Name, Mod, InitArg, Port, Opts).







-spec
get_server_connections(sockerl_types:name()) ->
    [] | [{sockerl_types:socket(), pid()}].
%% @doc
%%      Returns all available server connections.
%% @end
get_server_connections(Server) ->
    sockerl_server_sup:fetch_connections(Server).







-spec
get_acceptors(sockerl_types:name()) ->
    [] | [{pos_integer(), pid()}].
%% @doc
%%      Returns all server acceptors.
%% @end
get_acceptors(Server) ->
    sockerl_server_sup:fetch_acceptors(Server).







-spec
sleep_acceptors(sockerl_types:name()) ->
    'ok'.
%% @doc
%%      Turns all server acceptors to sleep mode.
%% @end
sleep_acceptors(Server) ->
    sockerl_server_sup:sleep_acceptors(Server).







-spec
wakeup_acceptors(sockerl_types:name()) ->
    'ok'.
%% @doc
%%      Turns all server acceptors to accept mode.
%% @end
wakeup_acceptors(Server) ->
    sockerl_server_sup:wakeup_acceptors(Server).







-spec
get_acceptor_modes(sockerl_types:name()) ->
    sockerl_types:acceptor_mode()                   |
    [{pos_integer(), sockerl_types:acceptor_mode()}].
%% @doc
%%      Returns mode(s) of server acceptors.
%% @end
get_acceptor_modes(Server) ->
    sockerl_server_sup:get_acceptor_modes(Server).







-spec
change_acceptor_modes(sockerl_types:name()) ->
    sockerl_types:acceptor_mode() | 'not_allowed'.
change_acceptor_modes(Server) ->
    sockerl_server_sup:change_acceptor_modes(Server).







-spec
stop_server(sockerl_types:name()) ->
    'ok'.
%% @doc
%%      stops server and all connections it has.
%% @end
stop_server(Server) ->
    sockerl_server_sup:stop(Server).







-spec
stop_server(sockerl_types:name(), Reason::any()) ->
    'ok'.
%% @doc
%%      stops server and all connections it has.
%% @end
stop_server(Server, Reason) ->
    sockerl_server_sup:stop(Server, Reason).







-spec
start_link_connector_pool(module(), term(), sockerl_types:addresses()) ->
    sockerl_types:start_return().
%% @doc
%%      Starts and links a socket connection pool.
%% @end
start_link_connector_pool(Mod, InitArg, Addrs) ->
    sockerl_connector_sup:start_link(Mod, InitArg, Addrs).







-spec
start_link_connector_pool(sockerl_types:register_name()|
                          module()
                         ,module() | term()
                         ,term() | sockerl_types:addresses()
                         ,sockerl_types:addresses() |
                          sockerl_types:start_options()) ->
    sockerl_types:start_return().
%% @doc
%%      Starts and links a socket connection pool.
%% @end
start_link_connector_pool(Name_or_Mod, Mod_or_InitArg, InitArg_or_Addrs, Addrs_or_Opts) ->
    sockerl_connector_sup:start_link(Name_or_Mod, Mod_or_InitArg, InitArg_or_Addrs, Addrs_or_Opts).







-spec
start_link_connector_pool(sockerl_types:register_name()
                         ,module()
                         ,term()
                         ,sockerl_types:addresses()
                         ,sockerl_types:start_options()) ->
    sockerl_types:start_return().
%% @doc
%%      Starts and links a socket connection pool.
%% @end
start_link_connector_pool(Name, Mod, InitArg, Addrs, Opts) ->
    sockerl_connector_sup:start_link(Name, Mod, InitArg, Addrs, Opts).







-spec
get_pool_connections(sockerl_types:name()) ->
    [] | [{sockerl_types:socket(), pid()}].
%% @doc
%%      Returns all available pool connections.
%% @end
get_pool_connections(Pool) ->
    sockerl_connector_sup:fetch(Pool).







-spec
add_connector(sockerl_types:name(), sockerl_types:host(), sockerl_types:port_number()) ->
    sockerl_types:start_return().
%% @doc
%%      Adds new connector for Host:Port in pool.
%% @end
add_connector(Pool, Host, Port) ->
    sockerl_connector_sup:add(Pool, Host, Port).







-spec
stop_pool(sockerl_types:name()) ->
    'ok'.
%% @doc
%%      stops pool and all connections it has.
%% @end
stop_pool(Pool) ->
    sockerl_connector_sup:stop(Pool).







-spec
stop_pool(sockerl_types:name(), any()) ->
    'ok'.
%% @doc
%%      stops pool and all connections it has with specific reason.
%% @end
stop_pool(Pool, Reason) ->
    sockerl_connector_sup:stop(Pool, Reason).







-spec
start_link_connector(module(), term(), sockerl_types:hostname(), sockerl_types:port_number()) ->
    sockerl_types:start_return().
%% @doc
%%      Starts and links a socket connection process.
%% @end
start_link_connector(Mod, InitArg, Host, Port) ->
    sockerl_connector:start_link(Mod, InitArg, Host, Port).







-spec
start_link_connector(sockerl_types:register_name() | module()
                     ,module() | term()
                     ,term() | sockerl_types:hostname()
                     ,sockerl_types:hostname() |
                      sockerl_types:port_number()
                     ,sockerl_types:port_number() |
                      sockerl_types:start_options()) ->
    sockerl_types:start_return().
%% @doc
%%      Starts and links a socket connection process.
%% @end
start_link_connector(Name_or_Mod, Mod_or_InitArg, InitArg_or_Host, Host_or_Port, Port_or_Opts) ->
    sockerl_connector:start_link(Name_or_Mod
                                ,Mod_or_InitArg
                                ,InitArg_or_Host
                                ,Host_or_Port
                                ,Port_or_Opts).







-spec
start_link_connector(sockerl_types:register_name()
                    ,module()
                    ,term()
                    ,sockerl_types:hostname()
                    ,sockerl_types:port_number()
                    ,sockerl_types:start_options()) ->
    sockerl_types:start_return().
%% @doc
%%      Starts and links a socket connection process.
%% @end
start_link_connector(Name, Mod, InitArg, Host, Port, Opts) ->
    sockerl_connector:start_link(Name, Mod, InitArg, Host, Port, Opts).







-spec
send_sync(sockerl_types:name(), sockerl_types:packet()) ->
    'ok' | sockerl_types:error().
%% @doc
%%      Sends packet synchronously through connector.
%% @end
send_sync(Con, Packet) ->
    sockerl_connector:send_sync(Con, Packet).







-spec
send_sync(sockerl_types:name(), sockerl_types:packet(), timeout()) ->
    'ok' | sockerl_types:error().
%% @doc
%%      Sends packet synchronously through connector with timeout.
%% @end
send_sync(Con, Packet, Timeout) ->
    sockerl_connector:send_sync(Con, Packet, Timeout).







-spec
send_async(sockerl_types:name(), sockerl_types:packet()) ->
    'ok'.
%% @doc
%%      Sends packet asynchronously through connector.
%% @end
send_async(Con, Packet) ->
    sockerl_connector:send_async(Con, Packet).







-spec
stop_connector(sockerl_types:name()) ->
    'ok'.
%% @doc
%%      Stops connector.
%% @end
stop_connector(Con) ->
    sockerl_connector:stop(Con).







-spec
stop_connector(sockerl_types:name(), any()) ->
    'ok'.
%% @doc
%%      Stops connector with specific reason.
%% @end
stop_connector(Con, Reason) ->
    sockerl_connector:stop(Con, Reason).