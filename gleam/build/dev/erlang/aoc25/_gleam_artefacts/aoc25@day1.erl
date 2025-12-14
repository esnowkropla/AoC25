-module(aoc25@day1).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/aoc25/day1.gleam").
-export([rotation_to_string/1, new_rotation/1, zeros_hit/2, rotate/2, update/2, main/0]).
-export_type([rotation/0, dial/0, rotation_result/0]).

-type rotation() :: {right, integer()} | {left, integer()}.

-type dial() :: {dial, integer(), integer()}.

-type rotation_result() :: {rotation_result, integer(), integer()}.

-file("src/aoc25/day1.gleam", 16).
-spec rotation_to_string(rotation()) -> binary().
rotation_to_string(Rotation) ->
    case Rotation of
        {right, Num} ->
            <<"R"/utf8, (erlang:integer_to_binary(Num))/binary>>;

        {left, Num@1} ->
            <<"L"/utf8, (erlang:integer_to_binary(Num@1))/binary>>
    end.

-file("src/aoc25/day1.gleam", 39).
-spec new_right(binary()) -> gleam@option:option(rotation()).
new_right(Num) ->
    case lenient_parse:to_int(Num) of
        {ok, Num@1} ->
            {some, {right, Num@1}};

        {error, _} ->
            none
    end.

-file("src/aoc25/day1.gleam", 46).
-spec new_left(binary()) -> gleam@option:option(rotation()).
new_left(Num) ->
    case lenient_parse:to_int(Num) of
        {ok, Num@1} ->
            {some, {left, Num@1}};

        {error, _} ->
            none
    end.

-file("src/aoc25/day1.gleam", 31).
-spec new_rotation(binary()) -> gleam@option:option(rotation()).
new_rotation(Line) ->
    case Line of
        <<"R"/utf8, Num/binary>> ->
            new_right(Num);

        <<"L"/utf8, Num@1/binary>> ->
            new_left(Num@1);

        _ ->
            none
    end.

-file("src/aoc25/day1.gleam", 53).
-spec zeros_hit(dial(), rotation()) -> integer().
zeros_hit(Dial, Rotation) ->
    case Rotation of
        {right, Num} ->
            case 100 of
                0 -> 0;
                Gleam@denominator -> (erlang:element(2, Dial) + Num) div Gleam@denominator
            end;

        {left, Num@1} when (Num@1 >= erlang:element(2, Dial)) andalso (erlang:element(
            2,
            Dial
        ) > 0) ->
            (case 100 of
                0 -> 0;
                Gleam@denominator@1 -> (Num@1 - erlang:element(2, Dial)) div Gleam@denominator@1
            end) + 1;

        {left, Num@2} when erlang:element(2, Dial) =:= 0 ->
            case 100 of
                0 -> 0;
                Gleam@denominator@2 -> Num@2 div Gleam@denominator@2
            end;

        {left, _} ->
            0
    end.

-file("src/aoc25/day1.gleam", 66).
-spec rotate(dial(), rotation()) -> rotation_result().
rotate(Dial, Rotation) ->
    Dial_position = gleam@result:unwrap(case Rotation of
            {right, Num} ->
                gleam@int:modulo(erlang:element(2, Dial) + Num, 100);

            {left, Num@1} ->
                gleam@int:modulo(erlang:element(2, Dial) - Num@1, 100)
        end, 0),
    {rotation_result, Dial_position, zeros_hit(Dial, Rotation)}.

-file("src/aoc25/day1.gleam", 79).
-spec update(dial(), rotation()) -> dial().
update(Dial, Rotation) ->
    Rotation_result = rotate(Dial, Rotation),
    {dial,
        erlang:element(2, Rotation_result),
        erlang:element(3, Dial) + erlang:element(3, Rotation_result)}.

-file("src/aoc25/day1.gleam", 88).
-spec main() -> nil.
main() ->
    Contents@1 = case simplifile:read(<<"day1-input"/utf8>>) of
        {ok, Contents} -> Contents;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"aoc25/day1"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 89,
                        value => _assert_fail,
                        start => 1934,
                        'end' => 1984,
                        pattern_start => 1945,
                        pattern_end => 1957})
    end,
    Lines = gleam@string:split(Contents@1, <<"\n"/utf8>>),
    Dial@1 = gleam@list:fold(
        Lines,
        {dial, 50, 0},
        fun(Dial, Line) ->
            echo(Line, nil, 97),
            case new_rotation(Line) of
                {some, Rotation} ->
                    _pipe = update(Dial, Rotation),
                    echo(_pipe, nil, 101);

                none ->
                    Dial
            end
        end
    ),
    echo(Dial@1, nil, 108),
    nil.

-define(is_lowercase_char(X),
    (X > 96 andalso X < 123)).

-define(is_underscore_char(X),
    (X == 95)).

-define(is_digit_char(X),
    (X > 47 andalso X < 58)).

-define(is_ascii_character(X),
    (erlang:is_integer(X) andalso X >= 32 andalso X =< 126)).

-define(could_be_record(Tuple),
    erlang:is_tuple(Tuple) andalso
        erlang:is_atom(erlang:element(1, Tuple)) andalso
        erlang:element(1, Tuple) =/= false andalso
        erlang:element(1, Tuple) =/= true andalso
        erlang:element(1, Tuple) =/= nil
).
-define(is_atom_char(C),
    (?is_lowercase_char(C) orelse
        ?is_underscore_char(C) orelse
        ?is_digit_char(C))
).

-define(grey, "\e[90m").
-define(reset_color, "\e[39m").

echo(Value, Message, Line) ->
    StringLine = erlang:integer_to_list(Line),
    StringValue = echo@inspect(Value),
    StringMessage =
        case Message of
            nil -> "";
            M -> [" ", M]
        end,

    io:put_chars(
      standard_error,
      [
        ?grey, ?FILEPATH, $:, StringLine, ?reset_color, StringMessage, $\n,
        StringValue, $\n
      ]
    ),
    Value.

echo@inspect(Value) ->
    case Value of
        nil -> "Nil";
        true -> "True";
        false -> "False";
        Int when erlang:is_integer(Int) -> erlang:integer_to_list(Int);
        Float when erlang:is_float(Float) -> io_lib_format:fwrite_g(Float);
        Binary when erlang:is_binary(Binary) -> inspect@binary(Binary);
        Bits when erlang:is_bitstring(Bits) -> inspect@bit_array(Bits);
        Atom when erlang:is_atom(Atom) -> inspect@atom(Atom);
        List when erlang:is_list(List) -> inspect@list(List);
        Map when erlang:is_map(Map) -> inspect@map(Map);
        Record when ?could_be_record(Record) -> inspect@record(Record);
        Tuple when erlang:is_tuple(Tuple) -> inspect@tuple(Tuple);
        Function when erlang:is_function(Function) -> inspect@function(Function);
        Any -> ["//erl(", io_lib:format("~p", [Any]), ")"]
    end.

inspect@bit_array(Bits) ->
    Pieces = inspect@bit_array_pieces(Bits, []),
    Inner = lists:join(", ", lists:reverse(Pieces)),
    ["<<", Inner, ">>"].

inspect@bit_array_pieces(Bits, Acc) ->
    case Bits of
        <<>> ->
            Acc;
        <<Byte, Rest/bitstring>> ->
            inspect@bit_array_pieces(Rest, [erlang:integer_to_binary(Byte) | Acc]);
        _ ->
            Size = erlang:bit_size(Bits),
            <<RemainingBits:Size>> = Bits,
            SizeString = [":size(", erlang:integer_to_binary(Size), ")"],
            Piece = [erlang:integer_to_binary(RemainingBits), SizeString],
            [Piece | Acc]
    end.

inspect@binary(Binary) ->
    case inspect@maybe_utf8_string(Binary, <<>>) of
        {ok, InspectedUtf8String} ->
            InspectedUtf8String;
        {error, not_a_utf8_string} ->
            Segments = [erlang:integer_to_list(X) || <<X>> <= Binary],
            ["<<", lists:join(", ", Segments), ">>"]
    end.

inspect@atom(Atom) ->
    Binary = erlang:atom_to_binary(Atom),
    case inspect@maybe_gleam_atom(Binary, none, <<>>) of
        {ok, Inspected} -> Inspected;
        {error, _} -> ["atom.create_from_string(\"", Binary, "\")"]
    end.

inspect@list(List) ->
    case inspect@list_loop(List, true) of
        {charlist, _} -> ["charlist.from_string(\"", erlang:list_to_binary(List), "\")"];
        {proper, Elements} -> ["[", Elements, "]"];
        {improper, Elements} -> ["//erl([", Elements, "])"]
    end.

inspect@map(Map) ->
    Fields = [
        [<<"#(">>, echo@inspect(Key), <<", ">>, echo@inspect(Value), <<")">>]
        || {Key, Value} <- maps:to_list(Map)
    ],
    ["dict.from_list([", lists:join(", ", Fields), "])"].

inspect@record(Record) ->
    [Atom | ArgsList] = Tuple = erlang:tuple_to_list(Record),
    case inspect@maybe_gleam_atom(Atom, none, <<>>) of
        {ok, Tag} ->
            Args = lists:join(", ", lists:map(fun echo@inspect/1, ArgsList)),
            [Tag, "(", Args, ")"];
        _ ->
            inspect@tuple(Tuple)
    end.

inspect@tuple(Tuple) when erlang:is_tuple(Tuple) ->
    inspect@tuple(erlang:tuple_to_list(Tuple));
inspect@tuple(Tuple) ->
    Elements = lists:map(fun echo@inspect/1, Tuple),
    ["#(", lists:join(", ", Elements), ")"].

inspect@function(Function) ->
    {arity, Arity} = erlang:fun_info(Function, arity),
    ArgsAsciiCodes = lists:seq($a, $a + Arity - 1),
    Args = lists:join(", ", lists:map(fun(Arg) -> <<Arg>> end, ArgsAsciiCodes)),
    ["//fn(", Args, ") { ... }"].

inspect@maybe_utf8_string(Binary, Acc) ->
    case Binary of
        <<>> ->
            {ok, <<$", Acc/binary, $">>};
        <<First/utf8, Rest/binary>> ->
            Escaped = inspect@escape_grapheme(First),
            inspect@maybe_utf8_string(Rest, <<Acc/binary, Escaped/binary>>);
        _ ->
            {error, not_a_utf8_string}
    end.

inspect@escape_grapheme(Char) ->
    case Char of
        $" -> <<$\\, $">>;
        $\\ -> <<$\\, $\\>>;
        $\r -> <<$\\, $r>>;
        $\n -> <<$\\, $n>>;
        $\t -> <<$\\, $t>>;
        $\f -> <<$\\, $f>>;
        X when X > 126, X < 160 -> inspect@convert_to_u(X);
        X when X < 32 -> inspect@convert_to_u(X);
        Other -> <<Other/utf8>>
    end.

inspect@convert_to_u(Code) ->
    erlang:list_to_binary(io_lib:format("\\u{~4.16.0B}", [Code])).

inspect@list_loop(List, Ascii) ->
    case List of
        [] ->
            {proper, []};
        [First] when Ascii andalso ?is_ascii_character(First) ->
            {charlist, nil};
        [First] ->
            {proper, [echo@inspect(First)]};
        [First | Rest] when erlang:is_list(Rest) ->
            StillAscii = Ascii andalso ?is_ascii_character(First),
            {Kind, Inspected} = inspect@list_loop(Rest, StillAscii),
            {Kind, [echo@inspect(First), ", " | Inspected]};
        [First | ImproperRest] ->
            {improper, [echo@inspect(First), " | ", echo@inspect(ImproperRest)]}
    end.

inspect@maybe_gleam_atom(Atom, PrevChar, Acc) when erlang:is_atom(Atom) ->
    Binary = erlang:atom_to_binary(Atom),
    inspect@maybe_gleam_atom(Binary, PrevChar, Acc);
inspect@maybe_gleam_atom(Atom, PrevChar, Acc) ->
    case {Atom, PrevChar} of
        {<<>>, none} ->
            {error, nil};
        {<<First, _/binary>>, none} when ?is_digit_char(First) ->
            {error, nil};
        {<<"_", _/binary>>, none} ->
            {error, nil};
        {<<"_">>, _} ->
            {error, nil};
        {<<"_", _/binary>>, $_} ->
            {error, nil};
        {<<First, _/binary>>, _} when not ?is_atom_char(First) ->
            {error, nil};
        {<<First, Rest/binary>>, none} ->
            inspect@maybe_gleam_atom(Rest, First, <<Acc/binary, (inspect@uppercase(First))>>);
        {<<"_", Rest/binary>>, _} ->
            inspect@maybe_gleam_atom(Rest, $_, Acc);
        {<<First, Rest/binary>>, $_} ->
            inspect@maybe_gleam_atom(Rest, First, <<Acc/binary, (inspect@uppercase(First))>>);
        {<<First, Rest/binary>>, _} ->
            inspect@maybe_gleam_atom(Rest, First, <<Acc/binary, First>>);
        {<<>>, _} ->
            {ok, Acc};
        _ ->
            erlang:throw({gleam_error, echo, Atom, PrevChar, Acc})
    end.

inspect@uppercase(X) -> X - 32.

