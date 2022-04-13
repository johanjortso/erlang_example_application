-module(sm).

%-compile([export_all, nowarn_export_all]).

-export([get_count/0, push/0, res/0, start/0, stop/0]).

-export([callback_mode/0,
         code_change/4,
         init/1,
         terminate/3]).

-export([off/3, on/3]).

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

res() -> gen_statem:call(name(), res, 3000).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) -> void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

init([]) ->
    %% Set the initial state + data.  Data is used only as a counter.
    State = off,
    Data = {0, 0},
    {ok, State, Data}.

callback_mode() -> state_functions.

%%% state callback(s)

off({call, From}, res, _Data) ->
    {next_state, off, {0, 0}, [{reply, From, off}]};
off({call, From}, push, Data) ->
    %% Go to 'on', increment count and reply
    %% that the resulting status is 'on'
    {On, Off} = Data,
    {next_state, on, {On + 1, Off}, [{reply, From, on}]};
off(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

on({call, From}, res, _Data) ->
    {next_state, off, {0, 0}, [{reply, From, off}]};
on({call, From}, push, Data) ->
    %% Go to 'off' and reply that the resulting status is 'off'
    {On, Off} = Data,
    {next_state, off, {On, Off + 1}, [{reply, From, off}]};
on(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

%% Handle events common to all states
handle_event({call, From}, get_count, Data) ->
    %% Reply with the current count
    {keep_state, Data, [{reply, From, Data}]};
handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state, Data}.

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

