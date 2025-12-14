-module(lenient_parse@internal@convert).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lenient_parse/internal/convert.gleam").
-export([digits_to_int_from_list/2, digits_to_int_with_base/2, digits_to_int/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lenient_parse/internal/convert.gleam", 13).
?DOC(false).
-spec digits_to_int_from_list(list(integer()), integer()) -> integer().
digits_to_int_from_list(Digits, Base) ->
    _pipe = Digits,
    gleam@list:fold(_pipe, 0, fun(Acc, Digit) -> (Acc * Base) + Digit end).

-file("src/lenient_parse/internal/convert.gleam", 9).
?DOC(false).
-spec digits_to_int_with_base(gleam@deque:deque(integer()), integer()) -> integer().
digits_to_int_with_base(Digits, Base) ->
    _pipe = Digits,
    _pipe@1 = gleam@deque:to_list(_pipe),
    digits_to_int_from_list(_pipe@1, Base).

-file("src/lenient_parse/internal/convert.gleam", 5).
?DOC(false).
-spec digits_to_int(gleam@deque:deque(integer())) -> integer().
digits_to_int(Digits) ->
    digits_to_int_with_base(Digits, 10).
