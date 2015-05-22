-ifndef(PERSON_PB_H).
-define(PERSON_PB_H, true).
-record(person, {
    name = erlang:error({required, name}),
    id = erlang:error({required, id}),
    email,
    phone = []
}).
-endif.

-ifndef(PERSON_PHONENUMBER_PB_H).
-define(PERSON_PHONENUMBER_PB_H, true).
-record(person_phonenumber, {
    number = erlang:error({required, number}),
    type
}).
-endif.

-ifndef(PERSON_PHONENUMBER_PHONETYPE_PB_H).
-define(PERSON_PHONENUMBER_PHONETYPE_PB_H, true).
-record(person_phonenumber_phonetype, {
    mobile,
    home,
    work
}).
-endif.

