-module(simple_server).

-compile([export_all, nowarn_export_all]).

start(State) ->
    Pid = spawn(?MODULE, loop, [State]),
    io:format("Starting ~p~n", [Pid]),
    Pid.



loop(State) ->
    receive
        update ->
            io:format("Updating code for ~p~n", [self()]),
            ?MODULE:loop(State);
        stop ->
            io:format("Stopped ~p~n", [self()]);
        {start, Pid} ->
            Pid ! {started},
            loop({started});
        {pause, Pid} ->
            Pid ! {paused},
            loop({paused});
        {state, Pid} ->
            Pid ! State,
            loop(State)
    end.

recv_msg() ->
    receive
        X ->
            io:format("state: ~p~n", [X]),
            X
    after 5000 ->
            io:format("Timeout, no answer")
    end.

test() ->
    Pid = ?MODULE:start({}),

    Pid ! {state, self()},
    {} = recv_msg(),

    Pid ! {pause, self()},
    {paused} = recv_msg(),

    Pid ! {state, self()},
    {paused} = recv_msg(),

    Pid ! {start, self()},
    {started} = recv_msg(),

    Pid ! {state, self()},
    {started} = recv_msg(),

    Pid ! stop.



% receive X -> io:format("state: ~p~n", [X]) after 5000 -> io:format("Timeout, no answer") end.

