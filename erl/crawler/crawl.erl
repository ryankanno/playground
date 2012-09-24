-module(crawl).

%% public api
-export([crawl/2]).

crawl(Url, {M, F} = Callback) ->
    UserAgent = "Magical Agent",
    {ok, {{_HttpVer, _Code, _Msg}, _Headers, Body}} = httpc:request(Url),
    M:F().
