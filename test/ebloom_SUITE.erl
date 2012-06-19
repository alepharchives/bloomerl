
basic_test() ->
    {ok, Ref} = new(5, 0.01, 123),
    0 = elements(Ref),
    insert(Ref, <<"abcdef">>),
    true = contains(Ref, <<"abcdef">>),
    false = contains(Ref, <<"zzzzzz">>).

%% union_test() ->
%%     {ok, Ref} = new(5, 0.01, 123),
%%     {ok, Ref2} = new(5, 0.01, 123),
%%     insert(Ref, <<"abcdef">>),
%%     false = contains(Ref2, <<"abcdef">>),
%%     union(Ref2, Ref),
%%     true = contains(Ref2, <<"abcdef">>).

serialize_test() ->
    {ok, Ref} = new(5, 0.01, 123),
    {ok, Ref2} = new(5, 0.01, 123),
    Bin = serialize(Ref),
    Bin2 = serialize(Ref2),
    true = (Bin =:= Bin2),
    insert(Ref, <<"abcdef">>),
    Bin3 = serialize(Ref),
    {ok, Ref3} = deserialize(Bin3),
    true = contains(Ref3, <<"abcdef">>),
    false = contains(Ref3, <<"rstuvw">>).

clear_test() ->
    {ok, Ref} = new(5, 0.01, 123),
    0 = elements(Ref),
    insert(Ref, <<"1">>),
    insert(Ref, <<"2">>),
    insert(Ref, <<"3">>),
    3 = elements(Ref),
    clear(Ref),
    0 = elements(Ref),
    false = contains(Ref, <<"1">>).
