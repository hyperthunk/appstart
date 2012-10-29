%% -----------------------------------------------------------------------------
%%
%% appstart: OTP Application Callback Module
%%
%% Copyright (c) 2010 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%%
%% @doc Pre-fabricated Application Callback Module. Only use for simple cases
%% (e.g. start/2 and stop/1). Set your start and/or stop MF(A) list in the 
%% appstart config section of your application's `env' key.
%%
%% -----------------------------------------------------------------------------
-module(appstarter).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(application).

-export([start/2, stop/1, load/1]).
-export([fail/2]).

load(App) ->
    application:load(App).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(StartType, StartArgs) ->
    {ok, App} = application:get_application(),
    Env = application:get_all_env(App),
    AppStart = proplists:get_value(appstart, Env),
    appstart:start_deps(App, AppStart),
    Options = case lists:keytake(fastlog, 1, StartArgs) of
        {value, {fastlog, configure}, Opts} ->
            fastlog:configure(App),
            Opts;
        _ -> StartArgs
    end,
    Conf = proplists:get_value(startup, AppStart, 
                               [?MODULE, fail, [Env]]),
    call_handler(StartType, Options, Conf, Env, AppStart).

stop(State) ->
    Env = application:get_all_env(),
    AppStart = proplists:get_value(appstart, Env),
    case proplists:get_value(stop, AppStart) of
        undefined ->
            ok;
        Conf ->
            call_handler(ignored, [State], Conf, Env, AppStart)
    end.

%%
%% @hidden
%%
fail(Env, _) ->
    {error, {badconfig, Env}}.

call_handler(StartType, StartArgs, [M], Env, Options) ->
    call_handler(StartType, StartArgs, [M, start_link, Env], Env, Options);
call_handler(StartType, StartArgs, [M,F|[]], Env, Options) ->
    call_handler(StartType, StartArgs, [M,F|Env], ignored, Options);
call_handler(StartType, StartArgs, [M,F|Rest]=Conf, Env, Options) ->
    %% choosing the max arity version of F guarantees we don't *loose*
    %% any information that the startup handling module might want...
    appstart:notify("call_handler ~p, ~p, ~p, ~p, ~p~n", 
                    [StartType, StartArgs, Conf, Env, Options], Options),
    Arity = case proplists:get_all_values(F, M:module_info(exports)) of
        [N] -> N;
        [_|_]=NS -> lists:max(NS)
    end,
    case Arity of
        0 ->
            Arg0 = [],
            notify_handler_executing(M, F, Arg0, Options),
            apply(M, F, Arg0);
        1 ->
            Arg1 = [StartArgs ++ Rest],
            notify_handler_executing(M, F, Arg1, Options),
            apply(M, F, Arg1);
        2 ->
            Arg2 = [StartType, StartArgs ++ Rest],
            notify_handler_executing(M, F, Arg2, Options),
            apply(M, F, Arg2);
        _ ->            
            fail(Conf, ignored)
    end.

notify_handler_executing(M, F, A, Options) ->
    appstart:notify("Calling handler ~p:~p with args ~p~n", [M, F, A], Options).
