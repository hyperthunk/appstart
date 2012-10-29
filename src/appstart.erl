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
         start/3,
         start_deps/2,
         load/1,
         notify/3]).

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
start(App, Options) when is_atom(App) andalso is_list(Options) ->
    start(App, temporary, Options);
start(App, Type) when is_atom(App) andalso is_atom(Type) ->
    start_it(App, Type, [], fun start_app/4).

%% @doc As start/1, but accepts a Type specification which is
%% passed directly on to application:start/2), and a proplist
%% containing settings used internally by appstart.
start(App, Type, Options) when is_atom(App) andalso
                              is_atom(Type) andalso
                              is_list(Options) ->
    start_it(App, Type, Options, fun start_app/4).

start_deps(App, Options) ->
    start_it(App, temporary, Options, fun start_deps/4).

start_deps(_, Config, Type, Options) ->
    start_app_deps(Config, Type, Options).

start_app_deps(Config, Type, Options) ->
    Apps = [start(A, Type, Options) || A <- proplists:get_value(applications, Config, [])],
    case lists:any(?FIND_ERROR_PREDICATE, Apps) of
        true ->
            {error, {dependants, lists:filter(?FIND_ERROR_PREDICATE, Apps)}};
        false ->
            Apps
    end.

start_app(App, Config, Type, Options) ->
    case start_app_deps(Config, Type, Options) of
        {error, _}=Error ->
            notify("Unable to start dependant applications: ~p~n", [Error], Options),
            Error;
        _ ->
            notify("Starting application ~p (~p)~n", [App, Type], Options),
            Res = application:start(App, Type),
            notify("application:start/2 = ~p~n", [Res], Options),
            Res
    end.

start_it(App, Type, Opt, Callback) ->
    Env = application:get_all_env(appstart),
    Options = lists:concat(Opt, Env),
    case lookup_app(App, Options) of
        already_loaded ->
            already_loaded;
        {error,_}=Err ->
            Err;
        {load_from, _Path} ->
            %% we're looking at one of two possible situations:
            %% (a) the lib_dir for App is in an .ez archive
            %% (b) we're running on beam loaded into an escript (archive)
            notify("Explicit application:load/1 required for ~p~n", [App], Options),
            case application:load(App) of
                {error, {already_loaded, App}} ->
                    start_it2(App, Type, Options, Callback);
                {error, _}=Failed ->
                    notify("Failed to load ~p: ~p~n", [App, Failed], Options),
                    Failed;
                ok ->
                    start_it2(App, Type, Options, Callback)
            end;
        {application, App, Config} ->
            %% TODO: check for configuration overrides here....
            notify("Callback: ~s~n", [callback_info(Callback)], Options),
            Callback(App, Config, Type, Options)
    end.

start_it2(App, Type, Options, Callback) ->
    {ok, KeySet} = application:get_all_key(App),
    notify("Found keyset for app ~p~n", [App], Options),
    notify("Callback: ~s~n", [callback_info(Callback)], Options),
    Callback(App, KeySet, Type, Options).

lookup_app(App, Options) ->
    Loaded = [AppName ||
                {AppName, _, _} <- application:which_applications(),
                AppName =:= App],
    case length(Loaded) > 0 of
        false ->
            lookup_appfile(App, Options);
        true ->
            notify("App ~p is already loaded~n", [App], Options),
            already_loaded
    end.

lookup_appfile(App, Options) ->
    AppFile = atom_to_list(App) ++ ".app",
    notify("Searching for application config ~s~n", [App], Options),
    case code:where_is_file(AppFile) of
        non_existing ->
            find_irregular_appfile(App, AppFile, Options);
        Path ->
            case filelib:is_regular(Path) of
                true ->
                    sanitize_path(Path, Options);
                false ->
                    find_irregular_appfile(App, AppFile, Options)
            end
    end.

sanitize_path(Path, Options) ->
    notify("Adjusting path ~s to current environment~n", [Path], Options),
    case lists:prefix(code:lib_dir(), Path) of
        true ->
            {load_from, Path};
        false ->
            case file:consult(Path) of
                {ok, [{application, _App, _Config}=AppConf]} ->
                    AppConf;
                {ok, {application, _App, _Config}=AppData} ->
                   AppData;
                {error, _} ->
                    {load_from, Path}
            end
    end.

%% because not everyone follows OTP principles...
find_irregular_appfile(App, AppFile, Options) ->
    case code:lib_dir(App, src) of
        {error, bad_name} ->
            {error, {no_lib_dir, App}};
        LibDir ->
            case filelib:is_dir(LibDir) of
                true ->
                    find_in_libdir(App, AppFile, LibDir, Options);
                false ->
                    {load_from, LibDir}
            end
    end.

find_in_libdir(App, AppFile, LibDir, Options) ->
    case filelib:wildcard(filename:join(LibDir, AppFile) ++ "*") of
        [F] ->
            sanitize_path(F, Options);
        _ ->
            {error, {no_app_file, App}}
    end.

notify(Format, Args, Options) ->
    Notice = "[APPSTART]  " ++ Format,
    case proplists:get_value(logging, Options) of
        console ->
            io:format(Notice, Args);
        {M, F} ->
            apply(M, F, [Notice, Args]);
        {file, Path} ->
            file:write(Path, io_lib:format(Notice, Args), [append]);
        _ ->
            ignored
    end.

callback_info(Callback) ->
    PList = erlang:fun_info(Callback),
    Mod = proplists:get_value(module, PList),
    Func = proplists:get_value(name, PList),
    atom_to_list(Mod) ++ ":" ++ atom_to_list(Func).
