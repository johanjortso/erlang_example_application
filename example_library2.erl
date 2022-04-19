-module(example_library2).

-export([seconds_valid_in_time/1]).

%% https://en.wikipedia.org/wiki/Leap_second
seconds_valid_in_time(Seconds) when
    Seconds >= 0,
    Seconds =< 58
->
    always_valid;
seconds_valid_in_time(_Seconds = 59) ->
    usually_valid_except_when_negative_leap_second;
seconds_valid_in_time(_Seconds = 60) ->
    valid_when_positive_leap_second;
seconds_valid_in_time(_Seconds) ->
    false.
