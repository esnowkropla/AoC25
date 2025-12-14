-module(lenient_parse@internal@scale).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lenient_parse/internal/scale.gleam").
-export([deques/3]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lenient_parse/internal/scale.gleam", 6).
?DOC(false).
-spec deques(
    gleam@deque:deque(integer()),
    gleam@deque:deque(integer()),
    integer()
) -> {gleam@deque:deque(integer()), gleam@deque:deque(integer())}.
deques(Whole_digits, Fractional_digits, Scale_factor) ->
    case gleam@int:compare(Scale_factor, 0) of
        eq ->
            {Whole_digits, Fractional_digits};

        gt ->
            {Digit, Fractional_digits@1} = begin
                _pipe = gleam@deque:pop_front(Fractional_digits),
                gleam@result:unwrap(_pipe, {0, Fractional_digits})
            end,
            deques(
                gleam@deque:push_back(Whole_digits, Digit),
                Fractional_digits@1,
                Scale_factor - 1
            );

        lt ->
            {Digit@1, Whole_digits@1} = begin
                _pipe@1 = gleam@deque:pop_back(Whole_digits),
                gleam@result:unwrap(_pipe@1, {0, Whole_digits})
            end,
            deques(
                Whole_digits@1,
                gleam@deque:push_front(Fractional_digits, Digit@1),
                Scale_factor + 1
            )
    end.
