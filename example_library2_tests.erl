-module(example_library2_tests).

-include_lib("eunit/include/eunit.hrl").

seconds_valid_in_time_edge_cases_test_() ->
    [
        ?_assertEqual(false, example_library2:seconds_valid_in_time(-1)),
        ?_assertEqual(false, example_library2:seconds_valid_in_time(61))
    ].

seconds_valid_in_time_normal_test_() ->
    [
        ?_assertEqual(always_valid, example_library2:seconds_valid_in_time(Seconds))
     || Seconds <- lists:seq(0, 58)
    ].

seconds_valid_in_time_negative_leap_second_test() ->
    ?assertEqual(
        usually_valid_except_when_negative_leap_second, example_library2:seconds_valid_in_time(59)
    ).

seconds_valid_in_time_positive_leap_second_test() ->
    ?assertEqual(
        valid_when_positive_leap_second, example_library2:seconds_valid_in_time(60)
    ).
