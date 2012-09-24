-module(crawler_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(crawler_sup, []).

init(_Args) ->
    {ok, {
    {one_for_one, 1, 60},
    [{crawler, {crawler, start_link, []},
    permanent, 
    brutal_kill, worker, [crawler]}]}}.
