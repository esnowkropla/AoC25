-module(lenient_parse@internal@build).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lenient_parse/internal/build.gleam").
-export([float_value/4, integer_value/3]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lenient_parse/internal/build.gleam", 101).
?DOC(false).
-spec float_string(list(integer()), list(integer()), boolean()) -> binary().
float_string(Whole_digits, Fractional_digits, Is_positive) ->
    Whole_string = case Whole_digits of
        [] ->
            <<"0"/utf8>>;

        _ ->
            _pipe = Whole_digits,
            _pipe@1 = gleam@list:map(_pipe, fun erlang:integer_to_binary/1),
            gleam@string:join(_pipe@1, <<""/utf8>>)
    end,
    Fractional_string = case Fractional_digits of
        [] ->
            <<"0"/utf8>>;

        _ ->
            _pipe@2 = Fractional_digits,
            _pipe@3 = gleam@list:map(_pipe@2, fun erlang:integer_to_binary/1),
            gleam@string:join(_pipe@3, <<""/utf8>>)
    end,
    case Is_positive of
        true ->
            <<<<Whole_string/binary, "."/utf8>>/binary,
                Fractional_string/binary>>;

        false ->
            <<<<<<"-"/utf8, Whole_string/binary>>/binary, "."/utf8>>/binary,
                Fractional_string/binary>>
    end.

-file("src/lenient_parse/internal/build.gleam", 13).
?DOC(false).
-spec float_value(
    boolean(),
    gleam@deque:deque(integer()),
    gleam@deque:deque(integer()),
    integer()
) -> {ok, float()} | {error, lenient_parse@parse_error:parse_error()}.
float_value(Is_positive, Whole_digits, Fractional_digits, Scale_factor) ->
    {Whole_digits@1, Fractional_digits@1} = lenient_parse@internal@scale:deques(
        Whole_digits,
        Fractional_digits,
        Scale_factor
    ),
    Exponent = begin
        _pipe = Fractional_digits@1,
        gleam@deque:length(_pipe)
    end,
    {Digits, _} = lenient_parse@internal@scale:deques(
        Whole_digits@1,
        Fractional_digits@1,
        Exponent
    ),
    Digits_list = begin
        _pipe@1 = Digits,
        gleam@deque:to_list(_pipe@1)
    end,
    case begin
        _pipe@2 = Digits_list,
        bigi:undigits(_pipe@2, 10)
    end of
        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"unreachable"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lenient_parse/internal/build"/utf8>>,
                    function => <<"float_value"/utf8>>,
                    line => 34});

        {ok, Coefficient} ->
            Sign = begin
                _pipe@3 = case Is_positive of
                    true ->
                        1;

                    false ->
                        -1
                end,
                bigi_ffi:from(_pipe@3)
            end,
            Decimal = lenient_parse@internal@pilkku@pilkku:new_bigint(
                Sign,
                Coefficient,
                bigi_ffi:from(- Exponent)
            ),
            case begin
                _pipe@4 = Decimal,
                lenient_parse@internal@pilkku@pilkku:to_float(_pipe@4)
            end of
                {ok, Float_value} when (Float_value =:= +0.0) andalso not Is_positive ->
                    {ok, -0.0};

                {ok, Float_value@1} ->
                    {ok, Float_value@1};

                {error, _} ->
                    Float_string = float_string(
                        begin
                            _pipe@5 = Whole_digits@1,
                            gleam@deque:to_list(_pipe@5)
                        end,
                        begin
                            _pipe@6 = Fractional_digits@1,
                            gleam@deque:to_list(_pipe@6)
                        end,
                        Is_positive
                    ),
                    {error, {out_of_float_range, Float_string}}
            end
    end.

-file("src/lenient_parse/internal/build.gleam", 122).
?DOC(false).
-spec integer_string(list(integer()), boolean()) -> binary().
integer_string(Digits_list, Is_positive) ->
    Integer_string = begin
        _pipe = Digits_list,
        _pipe@1 = gleam@list:map(_pipe, fun erlang:integer_to_binary/1),
        gleam@string:join(_pipe@1, <<""/utf8>>)
    end,
    case Is_positive of
        true ->
            Integer_string;

        false ->
            <<"-"/utf8, Integer_string/binary>>
    end.

-file("src/lenient_parse/internal/build.gleam", 68).
?DOC(false).
-spec integer_value(gleam@deque:deque(integer()), integer(), boolean()) -> {ok,
        integer()} |
    {error, lenient_parse@parse_error:parse_error()}.
integer_value(Digits, Base, Is_positive) ->
    Digits_list = begin
        _pipe = Digits,
        gleam@deque:to_list(_pipe)
    end,
    case begin
        _pipe@1 = Digits_list,
        bigi:undigits(_pipe@1, Base)
    end of
        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"unreachable"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"lenient_parse/internal/build"/utf8>>,
                    function => <<"integer_value"/utf8>>,
                    line => 83});

        {ok, Big_int} ->
            case begin
                _pipe@2 = Big_int,
                bigi_ffi:to(_pipe@2)
            end of
                {ok, Value} ->
                    Value@1 = case Is_positive of
                        true ->
                            Value;

                        false ->
                            - Value
                    end,
                    {ok, Value@1};

                {error, _} ->
                    Integer_string = begin
                        _pipe@3 = Digits_list,
                        integer_string(_pipe@3, Is_positive)
                    end,
                    {error, {out_of_int_range, Integer_string}}
            end
    end.
