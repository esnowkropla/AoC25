-module(lenient_parse@internal@whitespace).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lenient_parse/internal/whitespace.gleam").
-export([character_dict/0]).
-export_type([whitespace_data/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type whitespace_data() :: {whitespace_data,
        binary(),
        binary(),
        list(integer())}.

-file("src/lenient_parse/internal/whitespace.gleam", 12).
?DOC(false).
-spec character_dict() -> gleam@dict:dict(binary(), whitespace_data()).
character_dict() ->
    _pipe = [{whitespace_data, <<"\x{0009}"/utf8>>, <<"\\t"/utf8>>, [16#9]},
        {whitespace_data, <<"\x{000A}"/utf8>>, <<"\\n"/utf8>>, [16#A]},
        {whitespace_data, <<"\x{000B}"/utf8>>, <<"\\x0b"/utf8>>, [16#B]},
        {whitespace_data, <<"\x{000C}"/utf8>>, <<"\\x0c"/utf8>>, [16#C]},
        {whitespace_data, <<"\x{000D}"/utf8>>, <<"\\r"/utf8>>, [16#D]},
        {whitespace_data, <<"\x{0020}"/utf8>>, <<" "/utf8>>, [16#20]},
        {whitespace_data, <<"\x{0085}"/utf8>>, <<"\\x85"/utf8>>, [16#85]},
        {whitespace_data, <<"\x{00A0}"/utf8>>, <<"\\xa0"/utf8>>, [16#A0]},
        {whitespace_data, <<"\x{1680}"/utf8>>, <<"\\u1680"/utf8>>, [16#1680]},
        {whitespace_data, <<"\x{2000}"/utf8>>, <<"\\u2000"/utf8>>, [16#2000]},
        {whitespace_data, <<"\x{2001}"/utf8>>, <<"\\u2001"/utf8>>, [16#2001]},
        {whitespace_data, <<"\x{2002}"/utf8>>, <<"\\u2002"/utf8>>, [16#2002]},
        {whitespace_data, <<"\x{2003}"/utf8>>, <<"\\u2003"/utf8>>, [16#2003]},
        {whitespace_data, <<"\x{2004}"/utf8>>, <<"\\u2004"/utf8>>, [16#2004]},
        {whitespace_data, <<"\x{2005}"/utf8>>, <<"\\u2005"/utf8>>, [16#2005]},
        {whitespace_data, <<"\x{2006}"/utf8>>, <<"\\u2006"/utf8>>, [16#2006]},
        {whitespace_data, <<"\x{2007}"/utf8>>, <<"\\u2007"/utf8>>, [16#2007]},
        {whitespace_data, <<"\x{2008}"/utf8>>, <<"\\u2008"/utf8>>, [16#2008]},
        {whitespace_data, <<"\x{2009}"/utf8>>, <<"\\u2009"/utf8>>, [16#2009]},
        {whitespace_data, <<"\x{200A}"/utf8>>, <<"\\u200a"/utf8>>, [16#200A]},
        {whitespace_data, <<"\x{2028}"/utf8>>, <<"\\u2028"/utf8>>, [16#2028]},
        {whitespace_data, <<"\x{2029}"/utf8>>, <<"\\u2029"/utf8>>, [16#2029]},
        {whitespace_data, <<"\x{202F}"/utf8>>, <<"\\u202f"/utf8>>, [16#202F]},
        {whitespace_data, <<"\x{205F}"/utf8>>, <<"\\u205f"/utf8>>, [16#205F]},
        {whitespace_data, <<"\x{3000}"/utf8>>, <<"\\u3000"/utf8>>, [16#3000]},
        {whitespace_data,
            <<"\x{000D}\x{000A}"/utf8>>,
            <<"\\r\\n"/utf8>>,
            [16#000D, 16#000A]}],
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(Whitespace_data) ->
            {erlang:element(2, Whitespace_data), Whitespace_data}
        end
    ),
    maps:from_list(_pipe@1).
