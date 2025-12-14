-module(lenient_parse@internal@pilkku@context).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lenient_parse/internal/pilkku/context.gleam").
-export_type([signal/0, rounding/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type signal() :: invalid_operation | division_by_zero | rounded | inexact.

-type rounding() :: away_from_zero |
    toward_zero |
    midpoint_away_from_zero |
    midpoint_nearest_even |
    midpoint_toward_zero |
    toward_positive_infinity |
    toward_negative_infinity.


