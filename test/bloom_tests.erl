%% @hidden
-module(bloom_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all_my_test_() ->
    [  {"Should create a bloom filter",  fun should_create_a_bloom_filter/0}
     , {"Should be sized correctly", fun should_be_sized_correctly/0}
     , {"Should contain one element", fun should_contain_one_element/0}
    ].

should_create_a_bloom_filter() ->
    Bloom = bloom:new(5, 0.001),
    true = bloom:is_bloom(Bloom),
    false = bloom:is_bloom({foo}).

should_be_sized_correctly() ->
    Bloom1 = bloom:new(5, 0.1),
    32 = bloom:filter_size(Bloom1),
    Bloom2 = bloom:new(5, 0.01),
    48 = bloom:filter_size(Bloom2),
    Bloom3 = bloom:new(5), % default is 0.001
    72 = bloom:filter_size(Bloom3),
    Bloom4 = bloom:new(5, 0.0001),
    96 = bloom:filter_size(Bloom4).

should_contain_one_element() ->
    Bloom = bloom:new(5, 0.01),
    0 = bloom:count(Bloom),
    Bloom1 = bloom:add_element(<<"abcdef">>, Bloom),
    Bloom1 = bloom:add_element(<<"abcdef">>, Bloom),
    1 = bloom:count(Bloom1),
    true = bloom:is_element(<<"abcdef">>, Bloom1),
    false = bloom:is_element(<<"zzzzzz">>, Bloom1),
    Bloom2 = bloom:clear(Bloom1),
    0 = bloom:count(Bloom2),
    false = bloom:is_element(<<"abcdef">>, Bloom2),
    false = bloom:is_element(<<"zzzzzz">>, Bloom2).


%% Helper functions

%% fold_lines(FileName, Fun) ->
%%     {ok, Device} = file:open(FileName, [read]),
%%     fold_over_lines(Device, Fun).

%% fold_over_lines(Device, Fun) ->
%%     case io:get_line(Device, "") of
%%         eof  ->
%%             file:close(Device);
%%         Line ->
%%             Fun(Line), fold_over_lines(Device, Fun)
%%     end.
