-module(day1_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/day1_test.gleam").
-export([new_rotation_test/0, rotate_test/0, rotate_cant_go_negative_test/0, rotate_wraps_around_one_hundred_test/0, no_zeros_hit_test/0, one_zero_hit_test/0, multiple_zeros_hit_test/0, one_exact_hit_test/0, multiple_exact_hits_test/0, no_hits_left_test/0, one_hit_left_test/0, multiple_hits_left_test/0, one_exact_hit_left_test/0, multiple_exact_hits_left_test/0, massive_rotation_test/0, edge_case_test/0]).

-file("test/day1_test.gleam", 7).
-spec new_rotation_test() -> gleam@option:option(aoc25@day1:rotation()).
new_rotation_test() ->
    Line = <<"R32\n"/utf8>>,
    _assert_subject = aoc25@day1:new_rotation(Line),
    case _assert_subject of
        {some, {right, 32}} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"new_rotation_test"/utf8>>,
                        line => 9,
                        value => _assert_fail,
                        start => 204,
                        'end' => 261,
                        pattern_start => 215,
                        pattern_end => 235})
    end.

-file("test/day1_test.gleam", 12).
-spec rotate_test() -> aoc25@day1:rotation_result().
rotate_test() ->
    Dial = {dial, 0, 0},
    Rotation = {right, 12},
    _assert_subject = aoc25@day1:rotate(Dial, Rotation),
    case _assert_subject of
        {rotation_result, 12, 0} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"rotate_test"/utf8>>,
                        line => 16,
                        value => _assert_fail,
                        start => 363,
                        'end' => 446,
                        pattern_start => 374,
                        pattern_end => 412})
    end.

-file("test/day1_test.gleam", 20).
-spec rotate_cant_go_negative_test() -> aoc25@day1:rotation_result().
rotate_cant_go_negative_test() ->
    Dial = {dial, 0, 0},
    Rotation = {left, 1},
    _assert_subject = aoc25@day1:rotate(Dial, Rotation),
    case _assert_subject of
        {rotation_result, 99, 0} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"rotate_cant_go_negative_test"/utf8>>,
                        line => 24,
                        value => _assert_fail,
                        start => 563,
                        'end' => 646,
                        pattern_start => 574,
                        pattern_end => 612})
    end.

-file("test/day1_test.gleam", 28).
-spec rotate_wraps_around_one_hundred_test() -> aoc25@day1:rotation_result().
rotate_wraps_around_one_hundred_test() ->
    Dial = {dial, 99, 0},
    Rotation = {right, 10},
    _assert_subject = aoc25@day1:rotate(Dial, Rotation),
    case _assert_subject of
        {rotation_result, 9, 1} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"rotate_wraps_around_one_hundred_test"/utf8>>,
                        line => 32,
                        value => _assert_fail,
                        start => 774,
                        'end' => 852,
                        pattern_start => 785,
                        pattern_end => 822})
    end.

-file("test/day1_test.gleam", 35).
-spec no_zeros_hit_test() -> integer().
no_zeros_hit_test() ->
    Dial = {dial, 0, 0},
    Rotation = {right, 10},
    _assert_subject = aoc25@day1:zeros_hit(Dial, Rotation),
    case _assert_subject of
        0 -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"no_zeros_hit_test"/utf8>>,
                        line => 39,
                        value => _assert_fail,
                        start => 960,
                        'end' => 1005,
                        pattern_start => 971,
                        pattern_end => 972})
    end.

-file("test/day1_test.gleam", 42).
-spec one_zero_hit_test() -> integer().
one_zero_hit_test() ->
    Dial = {dial, 95, 0},
    Rotation = {right, 10},
    _assert_subject = aoc25@day1:zeros_hit(Dial, Rotation),
    case _assert_subject of
        1 -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"one_zero_hit_test"/utf8>>,
                        line => 46,
                        value => _assert_fail,
                        start => 1114,
                        'end' => 1159,
                        pattern_start => 1125,
                        pattern_end => 1126})
    end.

-file("test/day1_test.gleam", 49).
-spec multiple_zeros_hit_test() -> integer().
multiple_zeros_hit_test() ->
    Dial = {dial, 95, 0},
    Rotation = {right, 210},
    _assert_subject = aoc25@day1:zeros_hit(Dial, Rotation),
    case _assert_subject of
        3 -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"multiple_zeros_hit_test"/utf8>>,
                        line => 53,
                        value => _assert_fail,
                        start => 1275,
                        'end' => 1320,
                        pattern_start => 1286,
                        pattern_end => 1287})
    end.

-file("test/day1_test.gleam", 56).
-spec one_exact_hit_test() -> integer().
one_exact_hit_test() ->
    Dial = {dial, 99, 0},
    Rotation = {right, 1},
    _assert_subject = aoc25@day1:zeros_hit(Dial, Rotation),
    case _assert_subject of
        1 -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"one_exact_hit_test"/utf8>>,
                        line => 60,
                        value => _assert_fail,
                        start => 1429,
                        'end' => 1474,
                        pattern_start => 1440,
                        pattern_end => 1441})
    end.

-file("test/day1_test.gleam", 63).
-spec multiple_exact_hits_test() -> integer().
multiple_exact_hits_test() ->
    Dial = {dial, 0, 0},
    Rotation = {right, 200},
    _assert_subject = aoc25@day1:zeros_hit(Dial, Rotation),
    case _assert_subject of
        2 -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"multiple_exact_hits_test"/utf8>>,
                        line => 67,
                        value => _assert_fail,
                        start => 1585,
                        'end' => 1630,
                        pattern_start => 1596,
                        pattern_end => 1597})
    end.

-file("test/day1_test.gleam", 70).
-spec no_hits_left_test() -> integer().
no_hits_left_test() ->
    Dial = {dial, 10, 0},
    Rotation = {left, 5},
    _assert_subject = aoc25@day1:zeros_hit(Dial, Rotation),
    case _assert_subject of
        0 -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"no_hits_left_test"/utf8>>,
                        line => 74,
                        value => _assert_fail,
                        start => 1737,
                        'end' => 1782,
                        pattern_start => 1748,
                        pattern_end => 1749})
    end.

-file("test/day1_test.gleam", 77).
-spec one_hit_left_test() -> integer().
one_hit_left_test() ->
    Dial = {dial, 20, 0},
    Rotation = {left, 21},
    _assert_subject = aoc25@day1:zeros_hit(Dial, Rotation),
    case _assert_subject of
        1 -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"one_hit_left_test"/utf8>>,
                        line => 81,
                        value => _assert_fail,
                        start => 1890,
                        'end' => 1935,
                        pattern_start => 1901,
                        pattern_end => 1902})
    end.

-file("test/day1_test.gleam", 84).
-spec multiple_hits_left_test() -> integer().
multiple_hits_left_test() ->
    Dial = {dial, 50, 0},
    Rotation = {left, 251},
    _assert_subject = aoc25@day1:zeros_hit(Dial, Rotation),
    case _assert_subject of
        3 -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"multiple_hits_left_test"/utf8>>,
                        line => 88,
                        value => _assert_fail,
                        start => 2050,
                        'end' => 2095,
                        pattern_start => 2061,
                        pattern_end => 2062})
    end.

-file("test/day1_test.gleam", 91).
-spec one_exact_hit_left_test() -> integer().
one_exact_hit_left_test() ->
    Dial = {dial, 10, 0},
    Rotation = {left, 10},
    _assert_subject = aoc25@day1:zeros_hit(Dial, Rotation),
    case _assert_subject of
        1 -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"one_exact_hit_left_test"/utf8>>,
                        line => 95,
                        value => _assert_fail,
                        start => 2209,
                        'end' => 2254,
                        pattern_start => 2220,
                        pattern_end => 2221})
    end.

-file("test/day1_test.gleam", 98).
-spec multiple_exact_hits_left_test() -> integer().
multiple_exact_hits_left_test() ->
    Dial = {dial, 21, 0},
    Rotation = {left, 221},
    _assert_subject = aoc25@day1:zeros_hit(Dial, Rotation),
    case _assert_subject of
        3 -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"multiple_exact_hits_left_test"/utf8>>,
                        line => 102,
                        value => _assert_fail,
                        start => 2370,
                        'end' => 2415,
                        pattern_start => 2381,
                        pattern_end => 2382})
    end.

-file("test/day1_test.gleam", 105).
-spec massive_rotation_test() -> integer().
massive_rotation_test() ->
    Dial = {dial, 50, 0},
    Rotation = {right, 1000},
    _assert_subject = aoc25@day1:zeros_hit(Dial, Rotation),
    case _assert_subject of
        10 -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"massive_rotation_test"/utf8>>,
                        line => 109,
                        value => _assert_fail,
                        start => 2525,
                        'end' => 2571,
                        pattern_start => 2536,
                        pattern_end => 2538})
    end.

-file("test/day1_test.gleam", 112).
-spec edge_case_test() -> list(nil).
edge_case_test() ->
    Test_cases = [{50, [{left, 150}], 2},
        {50, [{right, 50}], 1},
        {50, [{left, 50}, {right, 50}], 1},
        {50, [{left, 50}, {left, 50}], 1},
        {50, [{left, 150}, {right, 50}], 2}],
    gleam@list:map(
        Test_cases,
        fun(X) ->
            {Current, Moves, Expected} = X,
            Dial = {dial, Current, 0},
            Result = gleam@list:fold(
                Moves,
                Dial,
                fun(Dial@1, Rotation) -> aoc25@day1:update(Dial@1, Rotation) end
            ),
            Msg = <<<<<<<<<<"current: "/utf8,
                                (erlang:integer_to_binary(Current))/binary>>/binary,
                            " moves: "/utf8>>/binary,
                        (gleam@string:join(
                            gleam@list:map(
                                Moves,
                                fun aoc25@day1:rotation_to_string/1
                            ),
                            <<", "/utf8>>
                        ))/binary>>/binary,
                    " expected: "/utf8>>/binary,
                (erlang:integer_to_binary(Expected))/binary>>,
            _assert_subject = erlang:element(3, Result),
            case _assert_subject =:= Expected of
                true -> nil;
                false -> erlang:error(#{gleam_error => assert,
                        message => Msg,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1_test"/utf8>>,
                        function => <<"edge_case_test"/utf8>>,
                        line => 137,
                        kind => binary_operator,
                        operator => '==',
                        left => #{kind => expression,
                            value => _assert_subject,
                            start => 3307,
                            'end' => 3319
                            },
                        right => #{kind => expression,
                            value => Expected,
                            start => 3323,
                            'end' => 3331
                            },
                        start => 3300,
                        'end' => 3331,
                        expression_start => 3307})
            end
        end
    ).
