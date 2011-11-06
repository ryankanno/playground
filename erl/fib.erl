-module(fib).
-export([fib/1, fib2/1]). 

%% Normal recursion (EEPS!)
fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 0 ->
    fib(N-1) + fib(N-2).

%% Tail recursion
fib2(N) -> fib2(N, 0, 1).
fib2(0, Acc, _) -> Acc;
fib2(N, Acc, Next) when N > 0 ->
    fib2(N-1, Next, Acc+Next).
