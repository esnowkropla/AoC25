-module(lenient_parse@internal@token).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lenient_parse/internal/token.gleam").
-export([to_error/2]).
-export_type([token/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type token() :: {sign, integer(), binary(), boolean()} |
    {digit, integer(), binary(), integer()} |
    {underscore, integer()} |
    {decimal_point, integer()} |
    {exponent_symbol, integer(), binary()} |
    {whitespace, integer(), lenient_parse@internal@whitespace:whitespace_data()} |
    {unknown, integer(), binary()}.

-file("src/lenient_parse/internal/token.gleam", 18).
?DOC(false).
-spec to_error(token(), integer()) -> lenient_parse@parse_error:parse_error().
to_error(Token, Base) ->
    case Token of
        {sign, Index, Sign, _} ->
            {invalid_sign_position, Index, Sign};

        {digit, Index@1, Character, Value} when Value >= Base ->
            {out_of_base_range, Index@1, Character, Value, Base};

        {digit, Index@2, Character@1, _} ->
            {invalid_digit_position, Index@2, Character@1};

        {underscore, Index@3} ->
            {invalid_underscore_position, Index@3};

        {decimal_point, Index@4} ->
            {invalid_decimal_position, Index@4};

        {exponent_symbol, Index@5, Exponent_symbol} ->
            {invalid_exponent_symbol_position, Index@5, Exponent_symbol};

        {whitespace, Index@6, Data} ->
            {unknown_character, Index@6, erlang:element(3, Data)};

        {unknown, Index@7, Character@2} ->
            {unknown_character, Index@7, Character@2}
    end.
