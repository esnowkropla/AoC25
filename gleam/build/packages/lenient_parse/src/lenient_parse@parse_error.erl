-module(lenient_parse@parse_error).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lenient_parse/parse_error.gleam").
-export_type([parse_error/0]).

-type parse_error() :: empty_string |
    whitespace_only_string |
    {invalid_underscore_position, integer()} |
    {invalid_decimal_position, integer()} |
    {invalid_sign_position, integer(), binary()} |
    {invalid_digit_position, integer(), binary()} |
    {base_prefix_only, {integer(), integer()}, binary()} |
    {out_of_base_range, integer(), binary(), integer(), integer()} |
    {invalid_exponent_symbol_position, integer(), binary()} |
    {unknown_character, integer(), binary()} |
    {invalid_base_value, integer()} |
    {out_of_int_range, binary()} |
    {out_of_float_range, binary()}.


