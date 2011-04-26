%% -----------------------------------------------------------------------------
%%
%% appstart: OTP Application Startup Helper
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
%% @doc Helper module for working with OTP application startup.
%%
%% -----------------------------------------------------------------------------

-module(appstart).
-export([start/1, 
         start/2, 
         start_deps/1,
         load/1]).

-define(FIND_ERROR_PREDICATE, fun({error, _}) -> true; (_) -> false end).

load(App) ->
    appstarter:load(App).

%% @doc Starts the OTP application `AppName', forcing all dependent applications
%% to start first. For example, if you application specification contains a
%% {applications,[kernel,stdlib,sasl,riak_err]} tuple, the `sasl' and `riak_err'
%% applications will be started (if they're not already running).
start(App) ->
    start(App, temporary).

%% @doc As start/1, but accepts a Type specification which is
%% passed directly on to application:start/2).
start(App, _) when App =:= kernel, App =:= stdlib ->
    ok;
start(App, Type) when is_atom(App) ->
    start_it(App, Type, fun start_app/3).

start_deps(App) ->
    start_it(App, ignored, fun start_deps/3).

start_deps(_, Config, _) ->
    start_app_deps(Config).

start_app_deps(Config) ->
    Apps = [start(A) || A <- proplists:get_value(applications, Config, [])],
    case lists:any(?FIND_ERROR_PREDICATE, Apps) of
        true ->
            {error, {dependents, lists:filter(?FIND_ERROR_PREDICATE, Apps)}};
        false ->
            Apps
    end.

start_app(App, Config, Type) ->
    case start_app_deps(Config) of
        {error, _}=Error ->
            Error;
        _ -> 
            application:start(App, Type)
    end.

start_it(App, Type, Callback) ->
    case lookup_app(App) of
        already_loaded ->
            already_loaded;
        {error,_}=Err ->
            Err;
        Path ->
            {ok, AppData} = file:consult(Path),
            [{application, App, Config}] = AppData,
            %% TODO: check for configuration overrides here....
            Callback(App, Config, Type)
    end.

lookup_app(App) ->
    Loaded = [AppName ||
                {AppName, _, _} <- application:which_applications(),
                AppName =:= App],
    case length(Loaded) > 0 of
        false ->
            lookup_appfile(App);
        true ->
            already_loaded
    end.

lookup_appfile(App) ->
    AppFile = atom_to_list(App) ++ ".app",
    case code:where_is_file(AppFile) of
        non_existing ->
            find_irregular_appfile(App, AppFile);
        Path ->
            Path
    end.

%% because not everyone follows OTP principles...
find_irregular_appfile(App, AppFile) ->
    case code:lib_dir(App, src) of
        {error, bad_name} ->
            {error,{no_lib_dir, App}};
        LibDir ->
            case filelib:wildcard(filename:join(LibDir, AppFile) ++ "*") of
                [F] -> F;
                _ -> {error, {no_app_file, App}}
            end
    end.
