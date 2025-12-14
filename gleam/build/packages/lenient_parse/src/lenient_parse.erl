-module(lenient_parse).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lenient_parse.gleam").
-export([to_float/1, to_int_with_base/2, to_int/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/lenient_parse.gleam", 8).
?DOC(" Converts a string to a float.\n").
-spec to_float(binary()) -> {ok, float()} |
    {error, lenient_parse@parse_error:parse_error()}.
to_float(Text) ->
    _pipe = Text,
    _pipe@1 = lenient_parse@internal@tokenizer:tokenize_float(_pipe),
    lenient_parse@internal@parser:parse_float_tokens(_pipe@1).

-file("src/lenient_parse.gleam", 22).
?DOC(
    " Converts a string to an integer with a specified base.\n"
    "\n"
    " The base must be between 2 and 36 (inclusive), or 0. When the base is set to\n"
    " 0, the base is inferred from the prefix specifier, if one is present. If no\n"
    " prefix specifier is present, base 10 is used.\n"
).
-spec to_int_with_base(binary(), integer()) -> {ok, integer()} |
    {error, lenient_parse@parse_error:parse_error()}.
to_int_with_base(Text, Base) ->
    Is_valid_base = (Base =:= 0) orelse ((Base >= 2) andalso (Base =< 36)),
    gleam@bool:guard(
        not Is_valid_base,
        {error, {invalid_base_value, Base}},
        fun() -> _pipe = Text,
            _pipe@1 = lenient_parse@internal@tokenizer:tokenize_int(_pipe),
            lenient_parse@internal@parser:parse_int_tokens(_pipe@1, Base) end
    ).

-file("src/lenient_parse.gleam", 13).
?DOC(" Converts a string to an integer using a default base of 10.\n").
-spec to_int(binary()) -> {ok, integer()} |
    {error, lenient_parse@parse_error:parse_error()}.
to_int(Text) ->
    _pipe = Text,
    to_int_with_base(_pipe, 10).
