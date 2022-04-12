-module(example_server).

-export([start/0, stop/1, loop/1]).
-export([february_days/2, leap_second/2]).
-export([get_state/1, add_state/3, remove_state/2, update_state/3, clear_state/1, get_stats/1]).

-define(TIMEOUT, 1000).

%% "Behaviour"/server functions
start() ->
    State = #{
        example_library_count => 0,
        example_library2_count => 0,
        saved_state => #{}
    },
    Pid = spawn(?MODULE, loop, [State]),
    io:format("Started example server with Pid = ~p~n", [Pid]),
    {ok, Pid}.

stop(Pid) ->
    Pid ! {stop, self()},
    io:format("Sending signal to server ~p to stop", [Pid]),
    receive
        {ok, stopped} ->
            io:format("Server ~p stopped", [Pid]),
            {ok, stopped}
    after ?TIMEOUT ->
        {error, could_not_stop}
    end.

%% API functions
february_days(Pid, Days) ->
    Pid ! {call, february_days, self(), Days},
    receive
        {ok, Reply} ->
            {ok, Reply}
    after ?TIMEOUT ->
        {error, timeout}
    end.

leap_second(Pid, Seconds) ->
    Pid ! {cast, leap_second, self(), Seconds},
    ok.

get_state(Pid) ->
    Pid ! {call, get_state, self()},
    receive
        {get_state, Reply} ->
            {ok, Reply}
    after ?TIMEOUT ->
        {error, timeout}
    end.

add_state(Pid, Key, Value) ->
    Pid ! {call, add_state, self(), {Key, Value}},
    receive
        {added_state, Key} ->
            ok
    after ?TIMEOUT ->
        {error, timeout}
    end.

update_state(Pid, Key, Value) ->
    Pid ! {call, update_state, self(), {Key, Value}},
    receive
        {updated_state, Key} ->
            ok
    after ?TIMEOUT ->
        {error, timeout}
    end.

remove_state(Pid, Key) ->
    Pid ! {call, remove_state, self(), Key},
    receive
        {removed_state, Key} ->
            ok
    after ?TIMEOUT ->
        {error, timeout}
    end.

clear_state(Pid) ->
    Pid ! {call, clear_state, self()},
    receive
        cleared_state ->
            ok
    after ?TIMEOUT ->
        {error, timeout}
    end.

get_stats(Pid) ->
    Pid ! {call, get_stats, self()},
    receive
        {get_stats, Reply} ->
            {ok, Reply}
    after ?TIMEOUT ->
        {error, timeout}
    end.

%% Internal functions
loop(State) ->
    receive
        {call, february_days, ReplyPid, Days} ->
            io:format("State = ~p~n", [State]),
            Result = example_library:can_month_be_february(Days),
            ReplyPid ! {ok, Result},
            #{example_library_count := CallCount} = State,
            loop(State#{example_library_count := CallCount + 1});
        {cast, leap_second, ReplyPid, Seconds} ->
            io:format("State = ~p~n", [State]),
            Result = example_library2:seconds_valid_in_time(Seconds),
            ReplyPid ! {ok, Result},
            #{example_library2_count := CallCount} = State,
            loop(State#{example_library2_count := CallCount + 1});
        {call, get_state, ReplyPid} ->
            io:format("State = ~p~n", [State]),
            #{saved_state := Saved} = State,
            ReplyPid ! {get_state, Saved},
            loop(State);
        {call, add_state, ReplyPid, {Key, Value}} ->
            io:format("State = ~p~n", [State]),
            #{saved_state := Saved} = State,
            NewSaved = Saved#{Key => Value},
            NewState = State#{saved_state := NewSaved},
            io:format("NewState = ~p~n", [NewState]),
            ReplyPid ! {added_state, Key},
            loop(NewState);
        {call, remove_state, ReplyPid, Key} ->
            io:format("State = ~p~n", [State]),
            #{saved_state := Saved} = State,
            NewSaved = maps:remove(Key, Saved),
            NewState = State#{saved_state := NewSaved},
            io:format("NewState = ~p~n", [NewState]),
            ReplyPid ! {removed_state, Key},
            loop(NewState);
        {call, update_state, ReplyPid, {Key, Value}} ->
            io:format("State = ~p~n", [State]),
            #{saved_state := Saved} = State,
            NewSaved = maps:update(Key, Value, Saved),
            NewState = State#{saved_state := NewSaved},
            io:format("NewState = ~p~n", [NewState]),
            ReplyPid ! {updated_state, Key},
            loop(NewState);
        {call, clear_state, ReplyPid} ->
            io:format("State = ~p~n", [State]),
            NewState = #{
                example_library_count => 0,
                example_library2_count => 0,
                saved_state => #{}
            },
            io:format("NewState = ~p~n", [NewState]),
            ReplyPid ! cleared_state,
            loop(NewState);
        {call, get_stats, ReplyPid} ->
            io:format("State = ~p~n", [State]),
            Stats = maps:without([saved_state], State),
            ReplyPid ! {get_stats, Stats},
            loop(State);
        {stop, ReplyPid} ->
            ReplyPid ! {ok, stopped},
            ok;
        Other ->
            io:format("Received unknown message, dropping ~p", [Other])
    end.
