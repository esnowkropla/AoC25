-module(lenient_parse@internal@tokenizer).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lenient_parse/internal/tokenizer.gleam").
-export([tokenize_float/1, tokenize_int/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lenient_parse/internal/tokenizer.gleam", 128).
?DOC(false).
-spec character_to_value(binary()) -> gleam@option:option(integer()).
character_to_value(Character) ->
    case Character of
        <<"0"/utf8>> ->
            {some, 0};

        <<"1"/utf8>> ->
            {some, 1};

        <<"2"/utf8>> ->
            {some, 2};

        <<"3"/utf8>> ->
            {some, 3};

        <<"4"/utf8>> ->
            {some, 4};

        <<"5"/utf8>> ->
            {some, 5};

        <<"6"/utf8>> ->
            {some, 6};

        <<"7"/utf8>> ->
            {some, 7};

        <<"8"/utf8>> ->
            {some, 8};

        <<"9"/utf8>> ->
            {some, 9};

        <<"a"/utf8>> ->
            {some, 10};

        <<"A"/utf8>> ->
            {some, 10};

        <<"b"/utf8>> ->
            {some, 11};

        <<"B"/utf8>> ->
            {some, 11};

        <<"c"/utf8>> ->
            {some, 12};

        <<"C"/utf8>> ->
            {some, 12};

        <<"d"/utf8>> ->
            {some, 13};

        <<"D"/utf8>> ->
            {some, 13};

        <<"e"/utf8>> ->
            {some, 14};

        <<"E"/utf8>> ->
            {some, 14};

        <<"f"/utf8>> ->
            {some, 15};

        <<"F"/utf8>> ->
            {some, 15};

        <<"g"/utf8>> ->
            {some, 16};

        <<"G"/utf8>> ->
            {some, 16};

        <<"h"/utf8>> ->
            {some, 17};

        <<"H"/utf8>> ->
            {some, 17};

        <<"i"/utf8>> ->
            {some, 18};

        <<"I"/utf8>> ->
            {some, 18};

        <<"j"/utf8>> ->
            {some, 19};

        <<"J"/utf8>> ->
            {some, 19};

        <<"k"/utf8>> ->
            {some, 20};

        <<"K"/utf8>> ->
            {some, 20};

        <<"l"/utf8>> ->
            {some, 21};

        <<"L"/utf8>> ->
            {some, 21};

        <<"m"/utf8>> ->
            {some, 22};

        <<"M"/utf8>> ->
            {some, 22};

        <<"n"/utf8>> ->
            {some, 23};

        <<"N"/utf8>> ->
            {some, 23};

        <<"o"/utf8>> ->
            {some, 24};

        <<"O"/utf8>> ->
            {some, 24};

        <<"p"/utf8>> ->
            {some, 25};

        <<"P"/utf8>> ->
            {some, 25};

        <<"q"/utf8>> ->
            {some, 26};

        <<"Q"/utf8>> ->
            {some, 26};

        <<"r"/utf8>> ->
            {some, 27};

        <<"R"/utf8>> ->
            {some, 27};

        <<"s"/utf8>> ->
            {some, 28};

        <<"S"/utf8>> ->
            {some, 28};

        <<"t"/utf8>> ->
            {some, 29};

        <<"T"/utf8>> ->
            {some, 29};

        <<"u"/utf8>> ->
            {some, 30};

        <<"U"/utf8>> ->
            {some, 30};

        <<"v"/utf8>> ->
            {some, 31};

        <<"V"/utf8>> ->
            {some, 31};

        <<"w"/utf8>> ->
            {some, 32};

        <<"W"/utf8>> ->
            {some, 32};

        <<"x"/utf8>> ->
            {some, 33};

        <<"X"/utf8>> ->
            {some, 33};

        <<"y"/utf8>> ->
            {some, 34};

        <<"Y"/utf8>> ->
            {some, 34};

        <<"z"/utf8>> ->
            {some, 35};

        <<"Z"/utf8>> ->
            {some, 35};

        _ ->
            none
    end.

-file("src/lenient_parse/internal/tokenizer.gleam", 97).
?DOC(false).
-spec common_token(
    binary(),
    integer(),
    fun((integer()) -> boolean()),
    gleam@dict:dict(binary(), lenient_parse@internal@whitespace:whitespace_data())
) -> lenient_parse@internal@token:token().
common_token(
    Character,
    Index,
    Tokenize_character_as_digit,
    Whitespace_character_dict
) ->
    case Character of
        <<"-"/utf8>> ->
            {sign, Index, <<"-"/utf8>>, false};

        <<"+"/utf8>> ->
            {sign, Index, <<"+"/utf8>>, true};

        <<"_"/utf8>> ->
            {underscore, Index};

        _ ->
            case begin
                _pipe = Whitespace_character_dict,
                gleam_stdlib:map_get(_pipe, Character)
            end of
                {ok, Whitespace_data} ->
                    {whitespace, Index, Whitespace_data};

                {error, _} ->
                    case character_to_value(Character) of
                        {some, Value} ->
                            case Tokenize_character_as_digit(Value) of
                                true ->
                                    {digit, Index, Character, Value};

                                false ->
                                    {unknown, Index, Character}
                            end;

                        none ->
                            {unknown, Index, Character}
                    end
            end
    end.

-file("src/lenient_parse/internal/tokenizer.gleam", 22).
?DOC(false).
-spec tokenize_float_loop(
    list(binary()),
    integer(),
    gleam@dict:dict(binary(), lenient_parse@internal@whitespace:whitespace_data()),
    list(lenient_parse@internal@token:token())
) -> list(lenient_parse@internal@token:token()).
tokenize_float_loop(Characters, Index, Whitespace_character_dict, Acc) ->
    case Characters of
        [] ->
            _pipe = Acc,
            lists:reverse(_pipe);

        [First | Rest] ->
            Token = case First of
                <<"."/utf8>> ->
                    {decimal_point, Index};

                <<"e"/utf8>> ->
                    {exponent_symbol, Index, First};

                <<"E"/utf8>> ->
                    {exponent_symbol, Index, First};

                _ ->
                    common_token(
                        First,
                        Index,
                        fun(Digit_value) -> Digit_value < 10 end,
                        Whitespace_character_dict
                    )
            end,
            tokenize_float_loop(
                Rest,
                Index + 1,
                Whitespace_character_dict,
                [Token | Acc]
            )
    end.

-file("src/lenient_parse/internal/tokenizer.gleam", 12).
?DOC(false).
-spec tokenize_float(binary()) -> list(lenient_parse@internal@token:token()).
tokenize_float(Text) ->
    _pipe = Text,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    tokenize_float_loop(
        _pipe@1,
        0,
        lenient_parse@internal@whitespace:character_dict(),
        []
    ).

-file("src/lenient_parse/internal/tokenizer.gleam", 67).
?DOC(false).
-spec tokenize_int_loop(
    list(binary()),
    integer(),
    gleam@dict:dict(binary(), lenient_parse@internal@whitespace:whitespace_data()),
    list(lenient_parse@internal@token:token())
) -> list(lenient_parse@internal@token:token()).
tokenize_int_loop(Characters, Index, Whitespace_character_dict, Acc) ->
    case Characters of
        [] ->
            _pipe = Acc,
            lists:reverse(_pipe);

        [First | Rest] ->
            Token = common_token(
                First,
                Index,
                fun(_) -> true end,
                Whitespace_character_dict
            ),
            tokenize_int_loop(
                Rest,
                Index + 1,
                Whitespace_character_dict,
                [Token | Acc]
            )
    end.

-file("src/lenient_parse/internal/tokenizer.gleam", 57).
?DOC(false).
-spec tokenize_int(binary()) -> list(lenient_parse@internal@token:token()).
tokenize_int(Text) ->
    _pipe = Text,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    tokenize_int_loop(
        _pipe@1,
        0,
        lenient_parse@internal@whitespace:character_dict(),
        []
    ).
