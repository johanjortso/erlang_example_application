%%%-------------------------------------------------------------------
%%% File    : example_SUITE.erl
%%% Author  :
%%% Description :
%%% https://www.erlang.org/doc/apps/common_test/example_chapter.html
%%% Created :
%%%-------------------------------------------------------------------
-module(example_SUITE).

%% Note: This directive should only be used in test suites.
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ct:pal("Starting example server"),
    {ok, Pid} = example_server:start(),
    ct:pal("Started example server with Pid = ~p~n", [Pid]),
    Config ++ [{server_pid, Pid}].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    Pid = proplists:get_value(server_pid, Config),
    {ok, stopped} = example_server:stop(Pid),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [
        {util_functions, [], [february_days_sync_call, leap_second_async_call]},
        {state_functions, [], [state_management, call_stats]}
    ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [{group, GroupName} || {GroupName, _Options, TestCases} <- groups(), length(TestCases) > 0].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
my_test_case() ->
    [].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
february_days_sync_call(Config) ->
    Pid = proplists:get_value(server_pid, Config),
    Days = 30,
    {ok, unlikely_but_happened_once} = example_server:february_days(Pid, Days),
    {comment, "example_library called successfully via server."}.

leap_second_async_call(Config) ->
    Pid = proplists:get_value(server_pid, Config),
    Seconds = 60,
    ok = example_server:leap_second(Pid, Seconds),
    {ok, valid_when_positive_leap_second} =
        receive
            {ok, Reply} ->
                {ok, Reply}
        after 5 * 1000 ->
            {error, timeout}
        end,
    {comment, "example_library2 called successfully via server."}.

state_management(Config) ->
    Pid = proplists:get_value(server_pid, Config),

    ExpectedInitalState = #{},
    {ok, InitialState} = example_server:get_state(Pid),
    ExpectedInitalState = InitialState,
    ct:pal("Getting state OK~n~p", [InitialState]),

    ExpectedAddState = #{a => 1, b => 2},
    ok = example_server:add_state(Pid, a, 1),
    ok = example_server:add_state(Pid, b, 2),
    {ok, AddState} = example_server:get_state(Pid),
    ExpectedAddState = AddState,
    ct:pal("Adding state OK~n~p", [AddState]),

    ExpectedUpdateState = #{a => 1, b => 42},
    ok = example_server:update_state(Pid, b, 42),
    {ok, UpdateState} = example_server:get_state(Pid),
    ExpectedUpdateState = UpdateState,
    ct:pal("Updating state OK~n~p", [UpdateState]),

    ExpectedRemoveState = #{a => 1},
    ok = example_server:remove_state(Pid, b),
    {ok, RemoveState} = example_server:get_state(Pid),
    ExpectedRemoveState = RemoveState,
    ct:pal("Removing state OK~n~p", [RemoveState]),

    ok = example_server:clear_state(Pid),
    {ok, ClearState} = example_server:get_state(Pid),
    ExpectedInitalState = ClearState,
    ct:pal("Clearing state OK~n~p", [ClearState]),

    {comment, "State management CRUD ok"}.

call_stats(Config) ->
    Pid = proplists:get_value(server_pid, Config),
    EmptyStats = #{
        example_library_count => 0,
        example_library2_count => 0
    },
    {ok, EmptyStats} = example_server:get_stats(Pid),

    [example_server:february_days(Pid, Days) || Days <- lists:seq(-1, 31)],
    [example_server:leap_second(Pid, Seconds) || Seconds <- lists:seq(-1, 61)],
    ExpectedStats = #{example_library2_count => 63,example_library_count => 33},
    {ok, Stats} = example_server:get_stats(Pid),
    ct:pal("Stats = ~p", [Stats]),
    ExpectedStats = Stats,

    ok = example_server:clear_state(Pid),
    {ok, EmptyStats} = example_server:get_stats(Pid),
    {comment, "Call stats OK"}.

%% From terminal:
%% ct_run -dir .
%% or:
%%  $ ct_run -suite check_log_SUITE

%% To use the Erlang shell to run our test, you can evaluate the following call:

%% ct:run_test([{dir, "."}]).
%% ct:run_test([{suite, "example_SUITE"}]).
%% ct:run_test([{suite, "example_SUITE"}, {group, grp}]).
%% ct:run_test([{suite, "example_SUITE"}, {group, [grp1, grp2]}]).

%  ct:run_test([{suite, "example_SUITE"}, {logdir, "ct"}, {group, groupA}, {testcase, tc_01}]).

%  ct:run_test([{logdir, "ct"}, {suite, "example_SUITE"}, {testcase, my_test_case}]).

%% ct:run_test([{suite, "example_SUITE"}, {logdir, "ct"}, {group, [util_functions, state_functions]}]).
