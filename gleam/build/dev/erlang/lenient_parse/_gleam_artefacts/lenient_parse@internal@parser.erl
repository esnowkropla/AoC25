-module(lenient_parse@internal@parser).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lenient_parse/internal/parser.gleam").
-export([parse_float_tokens/1, parse_int_tokens/2]).
-export_type([parse_data/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type parse_data(AMA) :: {parse_data,
        AMA,
        integer(),
        list(lenient_parse@internal@token:token())}.

-file("src/lenient_parse/internal/parser.gleam", 188).
?DOC(false).
-spec parse_whitespace_loop(
    list(lenient_parse@internal@token:token()),
    integer(),
    binary()
) -> {ok, parse_data(gleam@option:option(binary()))} |
    {error, lenient_parse@parse_error:parse_error()}.
parse_whitespace_loop(Tokens, Index, Acc) ->
    case Tokens of
        [{unknown, Index@1, Character} | _] ->
            {error, {unknown_character, Index@1, Character}};

        [{whitespace, Index@2, Data} | Rest] ->
            parse_whitespace_loop(
                Rest,
                Index@2 + 1,
                <<Acc/binary, (erlang:element(2, Data))/binary>>
            );

        _ ->
            Data@1 = case Acc of
                <<""/utf8>> ->
                    none;

                _ ->
                    {some, Acc}
            end,
            {ok, {parse_data, Data@1, Index, Tokens}}
    end.

-file("src/lenient_parse/internal/parser.gleam", 181).
?DOC(false).
-spec parse_whitespace(list(lenient_parse@internal@token:token()), integer()) -> {ok,
        parse_data(gleam@option:option(binary()))} |
    {error, lenient_parse@parse_error:parse_error()}.
parse_whitespace(Tokens, Index) ->
    parse_whitespace_loop(Tokens, Index, <<""/utf8>>).

-file("src/lenient_parse/internal/parser.gleam", 213).
?DOC(false).
-spec parse_sign(list(lenient_parse@internal@token:token()), integer()) -> {ok,
        parse_data(boolean())} |
    {error, lenient_parse@parse_error:parse_error()}.
parse_sign(Tokens, Index) ->
    case Tokens of
        [{unknown, Index@1, Character} | _] ->
            {error, {unknown_character, Index@1, Character}};

        [{sign, Index@2, _, Is_positive} | Rest] ->
            {ok, {parse_data, Is_positive, Index@2 + 1, Rest}};

        _ ->
            {ok, {parse_data, true, Index, Tokens}}
    end.

-file("src/lenient_parse/internal/parser.gleam", 260).
?DOC(false).
-spec base_prefix_data(
    list(lenient_parse@internal@token:token()),
    integer(),
    binary(),
    integer()
) -> parse_data(gleam@option:option({{integer(), integer()},
    binary(),
    integer()})).
base_prefix_data(Tokens, Index, Specifier, Base) ->
    {parse_data,
        {some, {{Index, Index + 2}, <<"0"/utf8, Specifier/binary>>, Base}},
        Index + 2,
        begin
            _pipe = Tokens,
            gleam@list:drop(_pipe, 1)
        end}.

-file("src/lenient_parse/internal/parser.gleam", 227).
?DOC(false).
-spec parse_base_prefix(
    list(lenient_parse@internal@token:token()),
    integer(),
    integer()
) -> {ok,
        parse_data(gleam@option:option({{integer(), integer()},
            binary(),
            integer()}))} |
    {error, lenient_parse@parse_error:parse_error()}.
parse_base_prefix(Tokens, Index, Base) ->
    case Tokens of
        [{unknown, Index@1, Character} | _] ->
            {error, {unknown_character, Index@1, Character}};

        [{digit, Index@2, <<"0"/utf8>>, _} | Rest] ->
            Lookahead = begin
                _pipe = Rest,
                gleam@list:first(_pipe)
            end,
            case Lookahead of
                {ok, {digit, _, Specifier, _}} when ((Base =:= 0) orelse (Base =:= 2)) andalso ((Specifier =:= <<"b"/utf8>>) orelse (Specifier =:= <<"B"/utf8>>)) ->
                    {ok, base_prefix_data(Rest, Index@2, Specifier, 2)};

                {ok, {digit, _, Specifier@1, _}} when ((Base =:= 0) orelse (Base =:= 8)) andalso ((Specifier@1 =:= <<"o"/utf8>>) orelse (Specifier@1 =:= <<"O"/utf8>>)) ->
                    {ok, base_prefix_data(Rest, Index@2, Specifier@1, 8)};

                {ok, {digit, _, Specifier@2, _}} when ((Base =:= 0) orelse (Base =:= 16)) andalso ((Specifier@2 =:= <<"x"/utf8>>) orelse (Specifier@2 =:= <<"X"/utf8>>)) ->
                    {ok, base_prefix_data(Rest, Index@2, Specifier@2, 16)};

                {ok, {digit, Index@3, Character@1, _}} when Base =:= 0 ->
                    {error, {unknown_character, Index@3, Character@1}};

                _ ->
                    {ok, {parse_data, none, Index@2, Tokens}}
            end;

        _ ->
            {ok, {parse_data, none, Index, Tokens}}
    end.

-file("src/lenient_parse/internal/parser.gleam", 273).
?DOC(false).
-spec parse_decimal_point(list(lenient_parse@internal@token:token()), integer()) -> {ok,
        parse_data(boolean())} |
    {error, lenient_parse@parse_error:parse_error()}.
parse_decimal_point(Tokens, Index) ->
    case Tokens of
        [{unknown, Index@1, Character} | _] ->
            {error, {unknown_character, Index@1, Character}};

        [{decimal_point, Index@2} | Rest] ->
            {ok, {parse_data, true, Index@2 + 1, Rest}};

        _ ->
            {ok, {parse_data, false, Index, Tokens}}
    end.

-file("src/lenient_parse/internal/parser.gleam", 285).
?DOC(false).
-spec parse_exponent_symbol(
    list(lenient_parse@internal@token:token()),
    integer()
) -> {ok, parse_data(gleam@option:option(binary()))} |
    {error, lenient_parse@parse_error:parse_error()}.
parse_exponent_symbol(Tokens, Index) ->
    case Tokens of
        [{unknown, Index@1, Character} | _] ->
            {error, {unknown_character, Index@1, Character}};

        [{exponent_symbol, Index@2, Exponent_symbol} | Rest] ->
            {ok, {parse_data, {some, Exponent_symbol}, Index@2 + 1, Rest}};

        _ ->
            {ok, {parse_data, none, Index, Tokens}}
    end.

-file("src/lenient_parse/internal/parser.gleam", 317).
?DOC(false).
-spec parse_digits_loop(
    list(lenient_parse@internal@token:token()),
    integer(),
    integer(),
    gleam@deque:deque(integer()),
    boolean(),
    boolean()
) -> {ok, parse_data(gleam@deque:deque(integer()))} |
    {error, lenient_parse@parse_error:parse_error()}.
parse_digits_loop(Tokens, Index, Base, Acc, At_beginning, Has_base_prefix) ->
    case Tokens of
        [{unknown, Index@1, Character} | _] ->
            {error, {unknown_character, Index@1, Character}};

        [{whitespace, Index@2, Data} | _] when At_beginning ->
            {error, {unknown_character, Index@2, erlang:element(2, Data)}};

        [{underscore, Index@3} | Rest] ->
            Lookahead = begin
                _pipe = Rest,
                gleam@list:first(_pipe)
            end,
            At_end = case Lookahead of
                {ok, {digit, _, _, _}} ->
                    false;

                _ ->
                    true
            end,
            Next_is_underscore = case Lookahead of
                {ok, {underscore, _}} ->
                    true;

                _ ->
                    false
            end,
            gleam@bool:guard(
                Next_is_underscore,
                {error, {invalid_underscore_position, Index@3 + 1}},
                fun() ->
                    gleam@bool:guard(
                        (At_beginning andalso not Has_base_prefix) orelse At_end,
                        {error, {invalid_underscore_position, Index@3}},
                        fun() ->
                            parse_digits_loop(
                                Rest,
                                Index@3 + 1,
                                Base,
                                Acc,
                                false,
                                Has_base_prefix
                            )
                        end
                    )
                end
            );

        [{digit, Index@4, Character@1, _} | _] when Base =:= 0 ->
            {error, {unknown_character, Index@4, Character@1}};

        [{digit, Index@5, _, Value} | Rest@1] when Value < Base ->
            parse_digits_loop(
                Rest@1,
                Index@5,
                Base,
                begin
                    _pipe@1 = Acc,
                    gleam@deque:push_back(_pipe@1, Value)
                end,
                false,
                Has_base_prefix
            );

        [{digit, Index@6, Character@2, Value@1} | _] ->
            {error, {out_of_base_range, Index@6, Character@2, Value@1, Base}};

        _ ->
            {ok, {parse_data, Acc, Index, Tokens}}
    end.

-file("src/lenient_parse/internal/parser.gleam", 301).
?DOC(false).
-spec parse_digits(
    list(lenient_parse@internal@token:token()),
    integer(),
    integer(),
    boolean()
) -> {ok, parse_data(gleam@deque:deque(integer()))} |
    {error, lenient_parse@parse_error:parse_error()}.
parse_digits(Tokens, Index, Base, Has_base_prefix) ->
    parse_digits_loop(
        Tokens,
        Index,
        Base,
        gleam@deque:new(),
        true,
        Has_base_prefix
    ).

-file("src/lenient_parse/internal/parser.gleam", 25).
?DOC(false).
-spec parse_float_tokens(list(lenient_parse@internal@token:token())) -> {ok,
        float()} |
    {error, lenient_parse@parse_error:parse_error()}.
parse_float_tokens(Tokens) ->
    Parse_data = parse_whitespace(Tokens, 0),
    gleam@result:'try'(
        Parse_data,
        fun(_use0) ->
            {parse_data, Leading_whitespace, Next_index, Tokens@1} = _use0,
            Parse_data@1 = parse_sign(Tokens@1, Next_index),
            gleam@result:'try'(
                Parse_data@1,
                fun(_use0@1) ->
                    {parse_data, Is_positive, Next_index@1, Tokens@2} = _use0@1,
                    Parse_data@2 = parse_digits(
                        Tokens@2,
                        Next_index@1,
                        10,
                        false
                    ),
                    gleam@result:'try'(
                        Parse_data@2,
                        fun(_use0@2) ->
                            {parse_data, Whole_digits, Next_index@2, Tokens@3} = _use0@2,
                            Parse_data@3 = parse_decimal_point(
                                Tokens@3,
                                Next_index@2
                            ),
                            gleam@result:'try'(
                                Parse_data@3,
                                fun(_use0@3) ->
                                    {parse_data,
                                        Decimal_specified,
                                        Next_index@3,
                                        Tokens@4} = _use0@3,
                                    Parse_data@4 = case Decimal_specified of
                                        true ->
                                            parse_digits(
                                                Tokens@4,
                                                Next_index@3,
                                                10,
                                                false
                                            );

                                        false ->
                                            {ok,
                                                {parse_data,
                                                    gleam@deque:new(),
                                                    Next_index@3,
                                                    Tokens@4}}
                                    end,
                                    gleam@result:'try'(
                                        Parse_data@4,
                                        fun(_use0@4) ->
                                            {parse_data,
                                                Fractional_digits,
                                                Next_index@4,
                                                Tokens@5} = _use0@4,
                                            Missing_digit_parts = gleam@deque:is_empty(
                                                Whole_digits
                                            )
                                            andalso gleam@deque:is_empty(
                                                Fractional_digits
                                            ),
                                            gleam@bool:guard(
                                                Missing_digit_parts andalso Decimal_specified,
                                                {error,
                                                    {invalid_decimal_position,
                                                        Next_index@4 - 1}},
                                                fun() ->
                                                    Parse_data@5 = parse_exponent_symbol(
                                                        Tokens@5,
                                                        Next_index@4
                                                    ),
                                                    gleam@result:'try'(
                                                        Parse_data@5,
                                                        fun(_use0@5) ->
                                                            {parse_data,
                                                                Exponent_symbol,
                                                                Next_index@5,
                                                                Tokens@6} = _use0@5,
                                                            Parse_data@9 = case {Missing_digit_parts,
                                                                Exponent_symbol} of
                                                                {true,
                                                                    {some,
                                                                        Exponent_symbol@1}} ->
                                                                    {error,
                                                                        {invalid_exponent_symbol_position,
                                                                            Next_index@5
                                                                            - 1,
                                                                            Exponent_symbol@1}};

                                                                {_, none} ->
                                                                    {ok,
                                                                        {parse_data,
                                                                            0,
                                                                            Next_index@5,
                                                                            Tokens@6}};

                                                                {_,
                                                                    {some,
                                                                        Exponent_symbol@2}} ->
                                                                    Parse_data@6 = parse_sign(
                                                                        Tokens@6,
                                                                        Next_index@5
                                                                    ),
                                                                    gleam@result:'try'(
                                                                        Parse_data@6,
                                                                        fun(
                                                                            _use0@6
                                                                        ) ->
                                                                            {parse_data,
                                                                                Exponent_digit_is_positive,
                                                                                Next_index@6,
                                                                                Tokens@7} = _use0@6,
                                                                            Parse_data@7 = parse_digits(
                                                                                Tokens@7,
                                                                                Next_index@6,
                                                                                10,
                                                                                false
                                                                            ),
                                                                            gleam@result:'try'(
                                                                                Parse_data@7,
                                                                                fun(
                                                                                    _use0@7
                                                                                ) ->
                                                                                    {parse_data,
                                                                                        Exponent_digits,
                                                                                        Next_index@7,
                                                                                        Tokens@8} = _use0@7,
                                                                                    Parse_data@8 = case begin
                                                                                        _pipe = Exponent_digits,
                                                                                        gleam@deque:is_empty(
                                                                                            _pipe
                                                                                        )
                                                                                    end of
                                                                                        true ->
                                                                                            {error,
                                                                                                {invalid_exponent_symbol_position,
                                                                                                    Next_index@7
                                                                                                    - 1,
                                                                                                    Exponent_symbol@2}};

                                                                                        false ->
                                                                                            {ok,
                                                                                                {parse_data,
                                                                                                    Exponent_digits,
                                                                                                    Next_index@7,
                                                                                                    Tokens@8}}
                                                                                    end,
                                                                                    gleam@result:'try'(
                                                                                        Parse_data@8,
                                                                                        fun(
                                                                                            _use0@8
                                                                                        ) ->
                                                                                            {parse_data,
                                                                                                Exponent_digits@1,
                                                                                                Next_index@8,
                                                                                                Tokens@9} = _use0@8,
                                                                                            Exponent_digit = begin
                                                                                                _pipe@1 = Exponent_digits@1,
                                                                                                lenient_parse@internal@convert:digits_to_int(
                                                                                                    _pipe@1
                                                                                                )
                                                                                            end,
                                                                                            Exponent = case Exponent_digit_is_positive of
                                                                                                true ->
                                                                                                    Exponent_digit;

                                                                                                false ->
                                                                                                    - Exponent_digit
                                                                                            end,
                                                                                            {ok,
                                                                                                {parse_data,
                                                                                                    Exponent,
                                                                                                    Next_index@8,
                                                                                                    Tokens@9}}
                                                                                        end
                                                                                    )
                                                                                end
                                                                            )
                                                                        end
                                                                    )
                                                            end,
                                                            gleam@result:'try'(
                                                                Parse_data@9,
                                                                fun(_use0@9) ->
                                                                    {parse_data,
                                                                        Exponent@1,
                                                                        Next_index@9,
                                                                        Tokens@10} = _use0@9,
                                                                    Parse_data@10 = parse_whitespace(
                                                                        Tokens@10,
                                                                        Next_index@9
                                                                    ),
                                                                    gleam@result:'try'(
                                                                        Parse_data@10,
                                                                        fun(
                                                                            _use0@10
                                                                        ) ->
                                                                            {parse_data,
                                                                                _,
                                                                                _,
                                                                                Tokens@11} = _use0@10,
                                                                            Remaining_token_result = case Tokens@11 of
                                                                                [] ->
                                                                                    {ok,
                                                                                        nil};

                                                                                [Token |
                                                                                    _] ->
                                                                                    {error,
                                                                                        lenient_parse@internal@token:to_error(
                                                                                            Token,
                                                                                            10
                                                                                        )}
                                                                            end,
                                                                            gleam@result:'try'(
                                                                                Remaining_token_result,
                                                                                fun(
                                                                                    _
                                                                                ) ->
                                                                                    case {Leading_whitespace,
                                                                                        Missing_digit_parts} of
                                                                                        {none,
                                                                                            true} ->
                                                                                            {error,
                                                                                                empty_string};

                                                                                        {{some,
                                                                                                _},
                                                                                            true} ->
                                                                                            {error,
                                                                                                whitespace_only_string};

                                                                                        {_,
                                                                                            _} ->
                                                                                            lenient_parse@internal@build:float_value(
                                                                                                Is_positive,
                                                                                                Whole_digits,
                                                                                                Fractional_digits,
                                                                                                Exponent@1
                                                                                            )
                                                                                    end
                                                                                end
                                                                            )
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/lenient_parse/internal/parser.gleam", 115).
?DOC(false).
-spec parse_int_tokens(list(lenient_parse@internal@token:token()), integer()) -> {ok,
        integer()} |
    {error, lenient_parse@parse_error:parse_error()}.
parse_int_tokens(Tokens, Base) ->
    Parse_data = parse_whitespace(Tokens, 0),
    gleam@result:'try'(
        Parse_data,
        fun(_use0) ->
            {parse_data, Leading_whitespace, Next_index, Tokens@1} = _use0,
            Parse_data@1 = parse_sign(Tokens@1, Next_index),
            gleam@result:'try'(
                Parse_data@1,
                fun(_use0@1) ->
                    {parse_data, Is_positive, Next_index@1, Tokens@2} = _use0@1,
                    Parse_data@3 = case Base of
                        Base@1 when (((Base@1 =:= 0) orelse (Base@1 =:= 2)) orelse (Base@1 =:= 8)) orelse (Base@1 =:= 16) ->
                            Parse_data@2 = parse_base_prefix(
                                Tokens@2,
                                Next_index@1,
                                Base@1
                            ),
                            gleam@result:'try'(
                                Parse_data@2,
                                fun(_use0@2) ->
                                    {parse_data,
                                        Base_data,
                                        Next_index@2,
                                        Tokens@3} = _use0@2,
                                    {Base@3, Prefix_data} = case Base_data of
                                        {some, {Index_range, Prefix, Base@2}} ->
                                            {Base@2,
                                                {some, {Index_range, Prefix}}};

                                        none ->
                                            Default_base = case Base@1 of
                                                0 ->
                                                    10;

                                                _ ->
                                                    Base@1
                                            end,
                                            {Default_base, none}
                                    end,
                                    {ok,
                                        {parse_data,
                                            {Base@3, Prefix_data},
                                            Next_index@2,
                                            Tokens@3}}
                                end
                            );

                        _ ->
                            {ok,
                                {parse_data,
                                    {Base, none},
                                    Next_index@1,
                                    Tokens@2}}
                    end,
                    gleam@result:'try'(
                        Parse_data@3,
                        fun(_use0@3) ->
                            {parse_data,
                                {Base@4, Prefix_data@1},
                                Next_index@3,
                                Tokens@4} = _use0@3,
                            Parse_data@4 = parse_digits(
                                Tokens@4,
                                Next_index@3,
                                Base@4,
                                begin
                                    _pipe = Prefix_data@1,
                                    gleam@option:is_some(_pipe)
                                end
                            ),
                            gleam@result:'try'(
                                Parse_data@4,
                                fun(_use0@4) ->
                                    {parse_data, Digits, Next_index@4, Tokens@5} = _use0@4,
                                    Parse_data@5 = parse_whitespace(
                                        Tokens@5,
                                        Next_index@4
                                    ),
                                    gleam@result:'try'(
                                        Parse_data@5,
                                        fun(_use0@5) ->
                                            {parse_data, _, _, Tokens@6} = _use0@5,
                                            Remaining_token_result = case Tokens@6 of
                                                [] ->
                                                    {ok, nil};

                                                [Token | _] ->
                                                    {error,
                                                        lenient_parse@internal@token:to_error(
                                                            Token,
                                                            Base@4
                                                        )}
                                            end,
                                            gleam@result:'try'(
                                                Remaining_token_result,
                                                fun(_) ->
                                                    case {Leading_whitespace,
                                                        Prefix_data@1,
                                                        begin
                                                            _pipe@1 = Digits,
                                                            gleam@deque:is_empty(
                                                                _pipe@1
                                                            )
                                                        end} of
                                                        {none, none, true} ->
                                                            {error,
                                                                empty_string};

                                                        {_,
                                                            {some,
                                                                {Index_range@1,
                                                                    Prefix@1}},
                                                            true} ->
                                                            {error,
                                                                {base_prefix_only,
                                                                    Index_range@1,
                                                                    Prefix@1}};

                                                        {{some, _}, _, true} ->
                                                            {error,
                                                                whitespace_only_string};

                                                        {_, _, _} ->
                                                            lenient_parse@internal@build:integer_value(
                                                                Digits,
                                                                Base@4,
                                                                Is_positive
                                                            )
                                                    end
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).
