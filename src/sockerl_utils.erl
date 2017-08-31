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
%% @version 17.9
%% @hidden
%% -------------------------------------------------------------------------------------------------


-module(sockerl_utils).
-author("pouriya.jahanbakhsh@gmail.com").


%% -------------------------------------------------------------------------------------------------
%% Exports:





%% API:
-export([get_value/2
        ,get_value/3
        ,get_value/4
        ,is_timeout/1
        ,is_whole_integer/1
        ,debug_options/3
        ,default_connector_plan_fun/3
        ,filter_socket_options/1
        ,stacktrace/1]).





%% -------------------------------------------------------------------------------------------------
%% API:





-spec
get_value(any(), list()) ->
    any().
get_value(Key, Opts) ->
    do_get_value(Key, Opts, undefined, undefined).







-spec
get_value(any(), list(), any()) ->
    any().
get_value(Key, Opts, Def) ->
    do_get_value(Key, Opts, {default, Def}, undefined).







-spec
get_value(any(), list(), any(), fun((any()) -> ok | {ok, term()} | {error, term()} | boolean())) ->
    any().
%% @doc
%%      gets value of Key from Opts. if not found, default value Def
%%      will returned. founded value will pass to filter fun Filter/1.
%% @end
get_value(Key, Opts, Def, Filter) ->
    do_get_value(Key, Opts, {default, Def}, Filter).







-spec
stacktrace(pos_integer()) ->
    list().
stacktrace(Count) ->
    case erlang:process_info(erlang:self(), [current_stacktrace]) of
        [{current_stacktrace, StackTrace}] ->
            if
                erlang:length(StackTrace) > Count ->
                    lists:nthtail(Count, StackTrace);
                true ->
                    StackTrace
            end;
        undefined ->
            []
    end.







-spec
is_timeout(term()) ->
    boolean().
is_timeout(infinity) ->
    true;
is_timeout(Timeout) when erlang:is_integer(Timeout) ->
    if
        Timeout >= 0 ->
            true;
        true ->
            false
    end.







-spec
is_whole_integer(term()) ->
    boolean().
is_whole_integer(Int) when erlang:is_integer(Int) ->
    if
        Int >= 0 ->
            true;
        true ->
            false
    end.







-spec
debug_options(term(), pid(), [] | [sys:dbg_opt()]) ->
    list().
debug_options(Name, Pid, DbgOpts) ->
    try
        sys:debug_options(DbgOpts)
    catch
        _ErrorType:_Reason ->
            error_logger:warning_report("~p [~p]: bad debug options: ~p", [Pid, Name, DbgOpts]),
            []
    end.







-spec
default_connector_plan_fun(term(), term(), pos_integer()) ->
    'delete' | 'stop'.
%% @doc
%%      It can be used as plan element in director plan.
%%       if process crashed with reason 'normal', supervisor will remove
%%       it from its children, if crashed with other reasons, supervisor
%%       swill crash with that reasons.
default_connector_plan_fun(_, normal, _RestartCount) ->
    delete;
default_connector_plan_fun(_, _Reason, _RestartCount) ->
    stop.







-spec
filter_socket_options(list()) ->
    'ok' | sockerl_types:error().
filter_socket_options(SockOpts) when erlang:is_list(SockOpts) ->
    case lists:keytake(active, 1, SockOpts) of
        {value, {_, Val}, SockOpts2} ->
            if
                erlang:is_boolean(Val) ->
                    filter_socket_options(SockOpts2);
                true ->
                    {error, {socket_options, [{active, Val}, {socket_options, SockOpts}]}}
            end;
        false ->
            ok
    end;
filter_socket_options(Other) ->
    {error, {socket_options_type, [{socket_options, Other}]}}.





%% -------------------------------------------------------------------------------------------------
%% Internal functions:





do_get_value(Key, Opts, Def, Filter) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Value} ->
            case Filter of
                undefined ->
                    Value;
                Filter ->
                    case Filter(Value) of
                        ok ->
                            Value;
                        {ok, Value2} ->
                            Value2;
                        true ->
                            Value;
                        false ->
                            erlang:error({option_value_type, [{key, Key}
                                                             ,{value, Value}
                                                             ,{stacktrace, stacktrace(2)}]});
                        {error, Reason} ->
                            erlang:error({option_value, [{reason, Reason}
                                                        ,{key, Key}
                                                        ,{value, Value}
                                                        ,{stacktrace, stacktrace(2)}]})
                    end
            end;
        false ->
            case Def of
                {default, Def2} ->
                    Def2;
                undefined ->
                    erlang:error({value_not_found, [{key, Key}, {stacktrace, stacktrace(2)}]})
            end
    end.