-module(lenient_parse@internal@pilkku@pilkku).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lenient_parse/internal/pilkku/pilkku.gleam").
-export([new_bigint/3, to_float/1]).
-export_type([cast_error/0, decimal/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type cast_error() :: loss_of_precision | out_of_range.

-opaque decimal() :: {decimal,
        bigi:big_int(),
        bigi:big_int(),
        bigi:big_int(),
        gleam@set:set(lenient_parse@internal@pilkku@context:signal())}.

-file("src/lenient_parse/internal/pilkku/pilkku.gleam", 80).
?DOC(false).
-spec new_bigint(bigi:big_int(), bigi:big_int(), bigi:big_int()) -> decimal().
new_bigint(Sign, Coefficient, Exponent) ->
    Sign@1 = case bigi_ffi:compare(Sign, bigi_ffi:zero()) of
        lt ->
            bigi_ffi:from(-1);

        _ ->
            bigi_ffi:from(1)
    end,
    {decimal, Sign@1, Coefficient, Exponent, gleam@set:new()}.

-file("src/lenient_parse/internal/pilkku/pilkku.gleam", 121).
?DOC(false).
-spec scale_up(bigi:big_int(), bigi:big_int(), bigi:big_int(), bigi:big_int()) -> {bigi:big_int(),
    bigi:big_int()}.
scale_up(Num, Den, Exp, One) ->
    case bigi_ffi:compare(Num, Den) of
        gt ->
            {Num, Exp};

        eq ->
            {Num, Exp};

        lt ->
            scale_up(
                bigi_ffi:bitwise_shift_left(Num, 1),
                Den,
                bigi_ffi:subtract(Exp, One),
                One
            )
    end.

-file("src/lenient_parse/internal/pilkku/pilkku.gleam", 134).
?DOC(false).
-spec scale_down(bigi:big_int(), bigi:big_int(), bigi:big_int(), bigi:big_int()) -> {bigi:big_int(),
    bigi:big_int()}.
scale_down(Num, Den, Exp, One) ->
    New_den = bigi_ffi:bitwise_shift_left(Den, 1),
    case bigi_ffi:compare(Num, New_den) of
        lt ->
            {bigi_ffi:bitwise_shift_right(Den, 52), Exp};

        _ ->
            scale_down(Num, New_den, bigi_ffi:add(Exp, One), One)
    end.

-file("src/lenient_parse/internal/pilkku/pilkku.gleam", 210).
?DOC(false).
-spec pow10(bigi:big_int()) -> bigi:big_int().
pow10(Val) ->
    Val@2 = case bigi_ffi:power(bigi_ffi:from(10), Val) of
        {ok, Val@1} -> Val@1;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"lenient_parse/internal/pilkku/pilkku"/utf8>>,
                        function => <<"pow10"/utf8>>,
                        line => 211,
                        value => _assert_fail,
                        start => 6824,
                        'end' => 6879,
                        pattern_start => 6835,
                        pattern_end => 6842})
    end,
    Val@2.

-file("src/lenient_parse/internal/pilkku/pilkku.gleam", 203).
?DOC(false).
-spec ratio(bigi:big_int(), bigi:big_int()) -> {bigi:big_int(), bigi:big_int()}.
ratio(Coef, Exp) ->
    case bigi_ffi:compare(Exp, bigi_ffi:zero()) of
        gt ->
            {bigi_ffi:multiply(Coef, pow10(Exp)), bigi_ffi:from(1)};

        eq ->
            {bigi_ffi:multiply(Coef, pow10(Exp)), bigi_ffi:from(1)};

        lt ->
            {Coef, pow10(bigi_ffi:negate(Exp))}
    end.

-file("src/lenient_parse/internal/pilkku/pilkku.gleam", 142).
?DOC(false).
-spec decimal_to_float(
    bigi:big_int(),
    bigi:big_int(),
    bigi:big_int(),
    bigi:big_int()
) -> {ok, float()} | {error, cast_error()}.
decimal_to_float(Sign, Num, Den, Exp) ->
    Quo = bigi_ffi:divide(Num, Den),
    Rem = bigi_ffi:subtract(Num, bigi_ffi:multiply(Quo, Den)),
    One = bigi_ffi:from(1),
    Shifted_den = bigi_ffi:bitwise_shift_right(Den, 1),
    Tmp = case bigi_ffi:compare(Rem, Shifted_den) of
        gt ->
            bigi_ffi:add(Quo, bigi_ffi:from(1));

        lt ->
            Quo;

        eq ->
            case bigi_ffi:bitwise_and(Quo, One) =:= One of
                true ->
                    bigi_ffi:add(Quo, One);

                false ->
                    Quo
            end
    end,
    Sign@1 = case Sign =:= bigi_ffi:from(-1) of
        true ->
            One;

        false ->
            bigi_ffi:zero()
    end,
    P52 = bigi_ffi:from(4503599627370496),
    Tmp@1 = bigi_ffi:subtract(Tmp, P52),
    Exp@1 = case bigi_ffi:compare(Tmp@1, P52) of
        lt ->
            Exp;

        _ ->
            bigi_ffi:add(Exp, One)
    end,
    Sign@2 = bigi_ffi:bitwise_shift_left(Sign@1, 63),
    gleam@result:'try'(
        case {bigi_ffi:compare(Exp@1, bigi_ffi:from(-1022)),
            bigi_ffi:compare(Exp@1, bigi_ffi:from(1023))} of
            {lt, _} ->
                {error, out_of_range};

            {_, gt} ->
                {error, out_of_range};

            {_, _} ->
                {ok, Exp@1}
        end,
        fun(Exp@2) ->
            Exp@3 = begin
                _pipe = bigi_ffi:add(Exp@2, bigi_ffi:from(1023)),
                bigi_ffi:bitwise_shift_left(_pipe, 52)
            end,
            gleam@result:'try'(
                case {bigi_ffi:compare(Tmp@1, bigi_ffi:zero()),
                    bigi_ffi:compare(
                        Tmp@1,
                        bigi_ffi:subtract(bigi_ffi:from(4503599627370496), One)
                    )} of
                    {lt, _} ->
                        {error, out_of_range};

                    {_, gt} ->
                        {error, out_of_range};

                    {_, _} ->
                        {ok, Tmp@1}
                end,
                fun(Coef) ->
                    Combined = begin
                        _pipe@1 = Sign@2,
                        _pipe@2 = bigi_ffi:bitwise_or(_pipe@1, Exp@3),
                        bigi_ffi:bitwise_or(_pipe@2, Coef)
                    end,
                    Num@2 = case bigi_ffi:to_bytes(
                        Combined,
                        big_endian,
                        unsigned,
                        8
                    ) of
                        {ok, <<Num@1/float>>} -> Num@1;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"lenient_parse/internal/pilkku/pilkku"/utf8>>,
                                        function => <<"decimal_to_float"/utf8>>,
                                        line => 197,
                                        value => _assert_fail,
                                        start => 6477,
                                        'end' => 6569,
                                        pattern_start => 6488,
                                        pattern_end => 6505})
                    end,
                    {ok, Num@2}
                end
            )
        end
    ).

-file("src/lenient_parse/internal/pilkku/pilkku.gleam", 97).
?DOC(false).
-spec to_float(decimal()) -> {ok, float()} | {error, cast_error()}.
to_float(Decimal) ->
    F2 = bigi_ffi:from(52),
    {Num, Den} = ratio(erlang:element(3, Decimal), erlang:element(4, Decimal)),
    Boundary = bigi_ffi:bitwise_shift_left(Den, 52),
    case Num =:= bigi_ffi:zero() of
        true ->
            {ok, +0.0};

        false ->
            One = bigi_ffi:from(1),
            case bigi_ffi:compare(Num, Boundary) of
                gt ->
                    {Den@1, Exp} = scale_down(Num, Boundary, F2, One),
                    decimal_to_float(
                        erlang:element(2, Decimal),
                        Num,
                        Den@1,
                        Exp
                    );

                eq ->
                    {Den@1, Exp} = scale_down(Num, Boundary, F2, One),
                    decimal_to_float(
                        erlang:element(2, Decimal),
                        Num,
                        Den@1,
                        Exp
                    );

                _ ->
                    {Num@1, Exp@1} = scale_up(
                        Num,
                        Boundary,
                        F2,
                        bigi_ffi:from(1)
                    ),
                    decimal_to_float(
                        erlang:element(2, Decimal),
                        Num@1,
                        Den,
                        Exp@1
                    )
            end
    end.
