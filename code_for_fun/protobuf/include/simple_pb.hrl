-ifndef(PERSON_PB_H).
-define(PERSON_PB_H, true).
-record(person, {
    name = erlang:error({required, name}),
    address = erlang:error({required, address}),
    phone_number = erlang:error({required, phone_number}),
    age = erlang:error({required, age}),
    location
}).
-endif.

-ifndef(LOCATION_PB_H).
-define(LOCATION_PB_H, true).
-record(location, {
    region = erlang:error({required, region}),
    country = erlang:error({required, country})
}).
-endif.

