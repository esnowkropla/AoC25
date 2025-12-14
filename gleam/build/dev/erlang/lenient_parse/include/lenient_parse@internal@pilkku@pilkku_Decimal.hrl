-record(decimal, {
    sign :: bigi:big_int(),
    coefficient :: bigi:big_int(),
    exponent :: bigi:big_int(),
    flags :: gleam@set:set(lenient_parse@internal@pilkku@context:signal())
}).
