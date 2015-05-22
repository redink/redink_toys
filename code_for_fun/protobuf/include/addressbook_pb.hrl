-ifndef(PERSON_PB_H).
-define(PERSON_PB_H, true).
-record(person, {
    name = erlang:error({required, name}),
    id = erlang:error({required, id}),
    email,
    phone = []
}).
-endif.

-ifndef(ADDRESSBOOK_PB_H).
-define(ADDRESSBOOK_PB_H, true).
-record(addressbook, {
    person = []
}).
-endif.

-ifndef(PERSON_PHONENUMBER_PB_H).
-define(PERSON_PHONENUMBER_PB_H, true).
-record(person_phonenumber, {
    number = erlang:error({required, number}),
    type = 'HOME'
}).
-endif.

