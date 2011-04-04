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

-module(appstart_loader).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(application).

-export([start/2, stop/1]).
-export([fail/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(StartType, StartArgs) ->
    {ok, App} = application:get_application(),
    appstart:start_deps(App),
    Env = application:get_all_env(),
    AppStart = proplists:get_value(appstart, Env),
    Conf = proplists:get_value(startup, AppStart, 
                               [?MODULE, fail, [Env]]),
    call_handler(StartType, StartArgs, Conf).

stop(State) ->
    Env = application:get_all_env(),
    AppStart = proplists:get_value(appstart, Env),
    case proplists:get_value(stop, AppStart) of
        undefined ->
            ok;
        Conf ->
            call_handler(ignored, [State], Conf)
    end.

%%
%% @hidden
%%
fail(Env, _) ->
    {error, {noconfig, Env}}.

call_handler(StartType, StartArgs, [M,F|Rest]=Conf) ->
    case lists:keyfind(F, 1, M:module_info(exports)) of
        {F, 0} ->
            apply(M, F, []);
        {F, 1} ->
            apply(M, F, [StartArgs ++ Rest]);
        {F, 2} ->
            apply(M, F, [StartType, StartArgs ++ Rest]);
        false ->            
            fail(Conf, ignored)
    end;
call_handler(StartType, StartArgs, [M]) ->
    call_handler(StartType, StartArgs, [M, start_link]).
