-ifndef(MESSAGE_PB_H).
-define(MESSAGE_PB_H, true).
-record(message, {
    s = erlang:error({required, s})
}).
-endif.

