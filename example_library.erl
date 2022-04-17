-module(example_library).

-export([can_month_be_february/1]).

-include_lib("eunit/include/eunit.hrl").

%% Public API
can_month_be_february(NumberOfDaysInMonth) when
    NumberOfDaysInMonth >= 1,
    NumberOfDaysInMonth =< 28
->
    true_every_year;
can_month_be_february(NumberOfDaysInMonth) when NumberOfDaysInMonth =:= 29 ->
    true_on_leap_years;
can_month_be_february(_NumberOfDaysInMonth = 30) ->
    %% Usually doesn't happen, but bappened once in the Swedish calendar in 1712,
    %% due to shenanigans when switching calendars.
    %% https://en.wikipedia.org/wiki/List_of_non-standard_dates#February_30
    unlikely_but_happened_once;
can_month_be_february(_NumberOfDaysInMonth) ->
    false.

%% Eunit tests
can_month_be_february_impossible_edge_cases_test_() ->
    [
        ?_assertEqual(false, can_month_be_february(-1)),
        ?_assertEqual(false, can_month_be_february(0)),
        ?_assertEqual(false, can_month_be_february(31))
    ].

can_month_be_february_leap_year_test() ->
        ?assertEqual(true_on_leap_years, can_month_be_february(29)).

can_month_be_february_very_special_case_test() ->
        ?assertEqual(unlikely_but_happened_once, can_month_be_february(30)).

can_month_be_february_normal_days_test_() ->
    [
        ?_assertEqual(true_every_year, can_month_be_february(NumberOfDaysInMonth))
     || NumberOfDaysInMonth <- lists:seq(1, 28)
    ].
