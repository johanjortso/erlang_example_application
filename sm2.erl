-module(sm2).

-compile([export_all, nowarn_export_all]).

-behaviour(gen_statem).

name() ->
    pushbutton_statem. % The registered server name

%% API.  This example uses a registered name name()
%% and does not link to the caller.
start() ->
    gen_statem:start({local, name()}, ?MODULE, [], []).

push() -> gen_statem:call(name(), push).

get_count() -> gen_statem:call(name(), get_count).

stop() -> gen_statem:stop(name()).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) -> void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

init([]) ->
    %% Set the initial state + data.  Data is used only as a counter.
    State = off,
    Data = 0,
    {ok, State, Data}.

callback_mode() -> handle_event_function.

%%% state callback(s)

handle_event({call, From}, push, off, Data) ->
    %% Go to 'on', increment count and reply
    %% that the resulting status is 'on'
    {next_state, on, Data + 1, [{reply, From, on}]};
handle_event({call, From}, push, on, Data) ->
    %% Go to 'off' and reply that the resulting status is 'off'
    {next_state, off, Data + 1, [{reply, From, off}]};
%%
%% Event handling common to all states
handle_event({call, From}, get_count, State, Data) ->
    %% Reply with the current count
    {next_state, State, Data, [{reply, From, Data}]};
handle_event(_, _, State, Data) ->
    %% Ignore all other events
    {next_state, State, Data}.

% 1> pushbutton:start().
% {ok,<0.36.0>}
% 2> pushbutton:get_count().
% 0
% 3> pushbutton:push().
% on
% 4> pushbutton:get_count().
% 1
% 5> pushbutton:push().
% off
% 6> pushbutton:get_count().
% 1
% 7> pushbutton:stop().
% ok
% 8> pushbutton:push().
% ** exception exit: {noproc,{gen_statem,call,[pushbutton_statem,push,infinity]}}
%      in function  gen:do_for_proc/2 (gen.erl, line 261)
%      in call from gen_statem:call/3 (gen_statem.erl, line 386)

