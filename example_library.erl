-module(example_library).

-export([can_month_be_february/1]).

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
