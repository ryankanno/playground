-module(crawler).
-behaviour(gen_server).

-export([start/0, start_link/0, stop/0, init/1, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% public api
-export([crawl/1]).

%% private
-export([spawn_crawl/1]).

%% public
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(Module) ->
    gen_server:call(Module, stop).

stop() ->
    stop(?MODULE).

crawl(Url, Time) ->
    gen_server:cast(?MODULE, {crawl, Url}). 

%% Will burst hundreds of crawlers if need be. should put on a queue to
%% limit activity
do_crawl(Url) -> 
    spawn(?MODULE, spawn_crawl, [Url]),
    ok.

spawn_crawl(Url) ->
    {ok, {_Status,_Headers, Body}} = http:request(Url),
    {ok, FileId} = file:open("hello.txt", [read, write]),
    io:fwrite(FileId, "~s", [Body]),
    file:close(FileId).

init([]) ->
    process_flag(trap_exit, true),
    inets:start(),

    case ets:info(?MODULE) of
        undefined -> ets:new(?MODULE, [public, named_table, ordered_set, {write_concurrency, true}]);
        _ -> true
    end,

    {ok, ?MODULE}.

handle_cast({crawl, Url}, State) ->
    do_crawl(Url),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
