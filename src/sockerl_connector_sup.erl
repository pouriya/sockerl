%%% --------------------------------------------------------------------
%%% BSD 3-Clause License
%%%
%%% Copyright (c) 2016-2017, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%% FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% --------------------------------------------------------------------
%% @author  Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version
%% @hidden
%% ---------------------------------------------------------------------


-module(sockerl_connector_sup).
-author("pouriya.jahanbakhsh@gmail.com").


%% ---------------------------------------------------------------------
%% Exports:





%% API:
-export([start_link/3
        ,start_link/4
        ,start_link/5
        ,start_link/6
        ,add/2
        ,fetch/1
        ,fetch_random/1
        ,stop/1
        ,stop/2]).





%% 'director' callback:
-export([init/1]).





%% ---------------------------------------------------------------------
%% Records & Macros & Includes:





-define(DEFAULT_START_OPTIONS, []).
-define(DEFAULT_CHILDSPEC_PLAN
       ,[fun sockerl_utils:default_connector_plan_fun/2]).
-define(DEFAULT_CHILDSPEC_COUNT, 1).
-define(DEFAULT_TERMINATE_TIMEOUT, 10*1000).
-define(DEFAULT_CONNECTOR_COUNT, 1).





%% ---------------------------------------------------------------------
%% API:





-spec
start_link(module(), term(), sockerl_types:start_options()) ->
    sockerl_types:start_return().
%% @doc
%%      starts and links a supervisor for supervising incoming
%%      connections of server.
%% @end
start_link(Mod, InitArg, Opts) when erlang:is_atom(Mod) andalso
                                    erlang:is_list(Opts) ->
    director:start_link(?MODULE, {Mod, InitArg, Opts}).







-spec
start_link(module()
          ,term()
          ,sockerl_types:host()
          ,sockerl_types:port_number()) ->
    sockerl_types:start_return().
%% @doc
%%      starts and links a connection pool supervisor.
%% @end
start_link(Mod, InitArg, Host, Port) when erlang:is_atom(Mod),
                                          erlang:is_integer(Port) ->
    director:start_link(?MODULE
                       ,{Mod
                        ,InitArg
                        ,Host
                        ,Port
                        ,?DEFAULT_START_OPTIONS}).







-spec
start_link(sockerl_types:register_name() | module()
          ,module() | term()
          ,term() | sockerl_types:host()
          ,sockerl_types:host() | sockerl_types:port_number()
          ,sockerl_types:port_number() |
           sockerl_types:start_options()) ->
    sockerl_types:start_return().
%% @doc
%%      starts and links a connection pool supervisor.
%% @end
start_link(Name
          ,Mod
          ,InitArg
          ,Host
          ,Port) when erlang:is_tuple(Name),
                      erlang:is_atom(Mod),
                      erlang:is_integer(Port) ->
    director:start_link(Name
                       ,?MODULE
                       ,{Mod
                        ,InitArg
                        ,Host
                        ,Port
                        ,?DEFAULT_START_OPTIONS});
start_link(Mod
          ,InitArg
          ,Host
          ,Port
          ,Opts) when erlang:is_atom(Mod),
                      erlang:is_integer(Port),
                      erlang:is_list(Opts) ->
    director:start_link(?MODULE, {Mod, InitArg, Host, Port, Opts}).







-spec
start_link(sockerl_types:register_name()
          ,module()
          ,term()
          ,sockerl_types:host()
          ,sockerl_types:port_number()
          ,sockerl_types:start_options()) ->
    sockerl_types:start_return().
%% @doc
%%      starts and links a connection pool supervisor.
%% @end
start_link(Name
          ,Mod
          ,InitArg
          ,Host
          ,Port
          ,Opts) when erlang:is_tuple(Name),
                      erlang:is_atom(Mod),
                      erlang:is_integer(Port),
                      erlang:is_list(Opts) ->
    director:start_link(Name
                       ,?MODULE
                       ,{Mod, InitArg, Host, Port, Opts}).







-spec
fetch(sockerl_types:name()) ->
    [] | [{sockerl_types:socket(), pid()}].
%% @doc
%%      fetch all available connections with their pids.
%% @end
fetch(ConSup) ->
    director:get_pids(ConSup).







-spec
fetch_random(sockerl_types:name()) ->
    {sockerl_types:socket(), pid()} | 'empty'.
fetch_random(ConSup) ->
    case director:get_pids(ConSup) of
        [] ->
            empty;
        [Con] ->
            Con;
        Cons ->
            choose_random(Cons)
    end.







-spec
add(sockerl_types:name(), sockerl_types:socket()) ->
    sockerl_types:start_return().
%% @doc
%%      adds new connection handler process to pool (for server).
%% @end
add(ConSup, Sock) ->
    director:start_child(ConSup
                        ,#{id => Sock
                          ,start => {sockerl_connector
                                    ,start_link
                                    ,[Sock]}
                          ,append => true
                          ,count => 0
                          ,plan => []}).







-spec
stop(sockerl_types:name()) ->
    'ok'.
%% @doc
%%      stops pool and all of its connections.
%% @end
stop(Server) ->
    director:stop(Server, normal, ?DEFAULT_TERMINATE_TIMEOUT).







-spec
stop(sockerl_types:name(), Reason::any()) ->
    ok.
%% @doc
%%      stops pool and all of its connections.
%% @end
stop(Server, Reason) ->
    director:stop(Server, Reason, ?DEFAULT_TERMINATE_TIMEOUT).





%% ---------------------------------------------------------------------
%% 'director' callback:





%% @hidden
init({Mod, InitArg, Opts}) ->
    ConPlan = sockerl_utils:get_value(connection_childspec_plan
                                     ,Opts
                                     ,?DEFAULT_CHILDSPEC_PLAN
                                     ,fun director_check:filter_plan/1),
    ConRunPlanCount =
        sockerl_utils:get_value(connector_childspec_count
                               ,Opts
                               ,?DEFAULT_CHILDSPEC_COUNT
                               ,fun sockerl_utils:is_timeout/1),
    {ok
    ,[]
    ,#{start => {sockerl_connector, start_link, [Mod, InitArg, Opts]}
      ,count => ConRunPlanCount
      ,plan => ConPlan
      ,type => worker}};

init({Mod, InitArg, Host, Port, Opts}) when erlang:is_list(Opts) ->
    ConCount =
        sockerl_utils:get_value(connector_count
                               ,Opts
                               ,?DEFAULT_CONNECTOR_COUNT
                               ,fun sockerl_utils:is_whole_integer/1),
    ConPlan = sockerl_utils:get_value(connector_childspec_plan
                                     ,Opts
                                     ,?DEFAULT_CHILDSPEC_PLAN
                                     ,fun director_check:filter_plan/1),
    ConRunPlanCount =
        sockerl_utils:get_value(connector_childspec_count
                               ,Opts
                               ,?DEFAULT_CHILDSPEC_COUNT
                               ,fun sockerl_utils:is_timeout/1),
    ChildSpecs = [#{id => Count
                  ,start => {sockerl_connector
                            ,start_link
                            ,[Mod, InitArg, Host, Port, Opts]}
                  ,count => ConRunPlanCount
                  ,plan => ConPlan
                  ,type => worker} || Count <- lists:seq(1, ConCount)],
    {ok, ChildSpecs}.





%% ---------------------------------------------------------------------
%% Internal functions:





choose_random(List) ->
    lists:nth(crypto:rand_uniform(1, erlang:length(List)+1), List).