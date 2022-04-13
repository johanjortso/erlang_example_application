-module(turnstile).

-author("Kasun").

-behaviour(gen_fsm). %% Old, use gen_statem instead

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1]).

-export([coin/0, locked/2, push/0, unlocked/2]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    State = locked,
    Data = [],
    {ok, State, Data}.

coin() -> gen_fsm:send_event(?MODULE, coin).

push() -> gen_fsm:send_event(?MODULE, push).

locked(Input, State) ->
    case Input of
        coin ->
            io:fwrite("Unlocked ~n"),
            {next_state, unlocked, State};
        push ->
            io:fwrite("Already locked ~n"),
            {next_state, locked, State}
    end.

unlocked(Input, State) ->
    case Input of
        push ->
            io:fwrite("Going through ~n"),
            io:fwrite("Locked ~n"),
            {next_state, locked, State};
        coin ->
            io:fwrite("Already unlocked ~n"),
            {next_state, unlocked, State}
    end.

% Eshell V5.9.3.1  (abort with ^G)
% 1> turnstile:start_link().
% {ok,<0.32.0>}
% 2> turnstile:coin().
% Unlocked
% ok
% 3> turnstile:push().
% Locked
% ok
% 4> turnstile:push().
% Already locked
% ok
% 5>

