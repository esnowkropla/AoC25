-module(day1).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/day1.gleam").
-export([main/0]).

-file("src/day1.gleam", 4).
-spec main() -> nil.
main() ->
    Contents@1 = case simplifile:read(<<"day1-input"/utf8>>) of
        {ok, Contents} -> Contents;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"day1"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 5,
                        value => _assert_fail,
                        start => 67,
                        'end' => 117,
                        pattern_start => 78,
                        pattern_end => 90})
    end,
    gleam_stdlib:println(Contents@1).
