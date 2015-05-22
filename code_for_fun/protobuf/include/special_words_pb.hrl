-ifndef(MESSAGE_PB_H).
-define(MESSAGE_PB_H, true).
-record(message, {
    double = erlang:error({required, double}),
    float = erlang:error({required, float}),
    int32 = erlang:error({required, int32}),
    int64 = erlang:error({required, int64}),
    uint32 = erlang:error({required, uint32}),
    uint64 = erlang:error({required, uint64}),
    sint32 = erlang:error({required, sint32}),
    sint64 = erlang:error({required, sint64}),
    fixed32 = erlang:error({required, fixed32}),
    fixed64 = erlang:error({required, fixed64}),
    sfixed32 = erlang:error({required, sfixed32}),
    sfixed64 = erlang:error({required, sfixed64}),
    bool = erlang:error({required, bool}),
    string = erlang:error({required, string}),
    bytes = erlang:error({required, bytes}),
    package = erlang:error({required, package}),
    option = erlang:error({required, option}),
    message = erlang:error({required, message}),
    enum = erlang:error({required, enum}),
    default = erlang:error({required, default}),
    pack = erlang:error({required, pack}),
    required = erlang:error({required, required}),
    optional = erlang:error({required, optional}),
    repeated = erlang:error({required, repeated}),
    to = erlang:error({required, to}),
    extensions = erlang:error({required, extensions}),
    max = erlang:error({required, max}),
    service = erlang:error({required, service}),
    rpc = erlang:error({required, rpc}),
    returns = erlang:error({required, returns})
}).
-endif.

