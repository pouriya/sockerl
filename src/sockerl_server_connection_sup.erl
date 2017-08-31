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
%% @version 17.7.10
%% @hidden
%% ---------------------------------------------------------------------


-module(sockerl_server_connection_sup).
-author("pouriya.jahanbakhsh@gmail.com").


%% ---------------------------------------------------------------------
%% Exports:





%% API:
-export([start_link/3
        ,add/2
        ,fetch/1]).





%% 'director' export:
-export([init/1]).






-define(DEF_START_OPTS, [{log_validate_fun, fun log_validate/2}]).





%% ---------------------------------------------------------------------
%% Internal functions:





-spec
start_link(module(), term(), sockerl_types:start_options()) ->
    sockerl_types:start_return().
%% @doc
%%      starts and links connection supervisor for keeping other
%%      connection supervisor(s).
%% @end
start_link(Mod, InitArg ,Opts) ->
    director:start_link(?MODULE, {Mod, InitArg, Opts}, ?DEF_START_OPTS).







-spec
add(sockerl_types:name(), term()) ->
    sockerl_types:start_return().
%% @doc
%%      adds new supervisor to connection supervisor.
%% @end
add(RootSup, Id) ->
    director:start_child(RootSup
                        ,#{plan => []
                         ,count => 0
                         ,id => Id
                         ,append => true}).







-spec
fetch(sockerl_types:name()) ->
    [] | [{sockerl_types:socket(), pid()}].
%% @doc
%%      fetch all available connections from supervisors.
%% @end
fetch(RootSup) ->
    concat([sockerl_connector_sup:fetch(Pid)
           || {_Id, Pid} <- director:get_pids(RootSup)]).





%% ---------------------------------------------------------------------
%% 'director' callback:





%% @hidden
init({Mod, InitArg, Opts}) ->
    {ok
    ,[]
    ,#{start => {sockerl_connector_sup
                ,start_link_
                ,[Mod, InitArg, Opts]}
      ,plan => [stop]
      ,type => supervisor
      ,count => 1}}.





%% ---------------------------------------------------------------------
%% Internal functions:





concat(List) ->
    concat(List, []).







concat([[Elem|List3]|List], List2) ->
    concat([List3|List], [Elem|List2]);
concat([[]|List], List2) ->
    concat(List, List2);
concat([], List2) ->
    List2.





log_validate('$director', {warning, _}) ->
    long;
log_validate(_, {info, start}) ->
    none;
log_validate(_, {error, normal}) ->
    none;
log_validate(_, _) ->
    long.