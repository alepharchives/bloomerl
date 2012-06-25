%% @doc Implementation of the Bloom filter data structure.
%% @reference [http://en.wikipedia.org/wiki/Bloom_filter]

-module(bloom).

-export([new/1, new/2, is_bloom/1, is_element/2, add_element/2, clear/1, count/1, filter_size/1]).

-import(math, [log/1, pow/2]).
-import(erlang, [phash2/2]).


-record(bloom, {
    m      = 0     :: non_neg_integer(),       % The size of the bitmap in bits.
    bitmap = <<>>  :: binary(),                % The bitmap.
    k      = 0     :: non_neg_integer(),       % The number of hashes.
    n      = 0     :: non_neg_integer(),       % The maximum number of keys.
    keys   = 0     :: non_neg_integer()        % The current number of keys.
}).

%% @equiv new(capacity, 0.001)
-spec new(non_neg_integer()) -> #bloom{}.
new(N) -> new(N, 0.001).

%% @doc Creates a new Bloom filter, given a maximum number of keys and a
%%     false-positive error rate.
-spec new(non_neg_integer(), float()) -> #bloom{}.
new(N, E) when N > 0, is_float(E), E > 0, E =< 1 ->
    {M, K} = calc_least_bits(N, E),
    #bloom{m=M, bitmap = <<0:((M+7) div 8 * 8)>>, k=K, n=N}.

%% @doc Creates a new empty Bloom filter from an existing one.
-spec clear(#bloom{}) -> #bloom{}.
clear(#bloom{bitmap=Bitmap} = B) ->
    B#bloom{bitmap = <<0:(erlang:bit_size(Bitmap))>>, n=0, keys=0}.

%% @doc Returns the number of elements encoded into this Bloom filter.
-spec count(#bloom{}) -> non_neg_integer().
count(#bloom{keys=N}) ->
    N.

%% @doc Returns the number of bits used in this Bloom filter.
-spec filter_size(#bloom{}) -> non_neg_integer().
filter_size(#bloom{bitmap=Bitmap}) ->
    bit_size(Bitmap).

%% @doc Determines if the given argument is a bloom record.
-spec is_bloom(#bloom{}) -> true | false.
is_bloom(#bloom{}) -> true;
is_bloom(_) -> false.

%% @doc Determines if the key is (probably) an element of the filter.
-spec is_element(term(), #bloom{}) -> true | false.
is_element(Key, B) -> is_element(Key, B, calc_idxs(Key, B)).
is_element(_, _, []) -> true;
is_element(Key, B, [Idx | T]) ->
    ByteIdx = Idx div 8,
    <<_:ByteIdx/binary, Byte:8, _/binary>> = B#bloom.bitmap,
    Mask = 1 bsl (Idx rem 8),
    case 0 =/= Byte band Mask of
         true -> is_element(Key, B, T);
        false -> false
    end.

%% @doc Adds the key to the filter.
-spec add_element(term(), #bloom{}) -> #bloom{}.
add_element(Key, #bloom{keys=Keys, bitmap=Bitmap}=B) -> %  TODO: when Keys < N
    Idxs = calc_idxs(Key, B),
    NewBitmap = set_bits(Bitmap, Idxs),
    case NewBitmap == Bitmap of
        true ->
            B#bloom{bitmap=NewBitmap};
        false ->
            B#bloom{bitmap=NewBitmap, keys=Keys + 1}
    end.

%% @internal
%% @doc Set the bits at the provided index(s) to "1" in the binary.
-spec set_bits(binary(), list(non_neg_integer())) -> binary().
set_bits(Bin, []) -> Bin;
set_bits(Bin, [Idx | Idxs]) ->
    ByteIdx = Idx div 8,
    <<Pre:ByteIdx/binary, Byte:8, Post/binary>> = Bin,
    Mask = 1 bsl (Idx rem 8),
    Byte0 = Byte bor Mask,
    set_bits(<<Pre/binary, Byte0:8, Post/binary>>, Idxs).

%% @internal
%% @doc Find the optimal bitmap size and number of hashes.
%TODO -spec(non_neg_integer(), number()) -> non_neg_integer().
calc_least_bits(N, E) -> calc_least_bits(N, E, 1, 0, 0).
calc_least_bits(N, E, K, MinM, BestK) ->
    M = -1 * K * N / log(1 - pow(E, 1/K)),
    {CurM, CurK} = if M < MinM -> {M, K}; true -> {MinM, BestK} end,
    case K of
          1 -> calc_least_bits(N, E, K+1, M, K);
        100 -> {trunc(CurM)+1, CurK};
          _ -> calc_least_bits(N, E, K+1, CurM, CurK)
    end.

%% @internal
%% @doc This uses the "enhanced double hashing" algorithm.
%% TODO: handle case of m > 2^32.
%TODO -spec(term(), #bloom{}) -> list(non_neg_integer()).
calc_idxs(Key, #bloom{m=M, k=K}) ->
    X = phash2(Key, M),
    Y = phash2({"salt", Key}, M),
    calc_idxs(M, K - 1, X, Y, [X]).
calc_idxs(_, 0, _, _, Acc) -> Acc;
calc_idxs(M, I, X, Y, Acc) ->
    Xi = (X+Y) rem M,
    Yi = (Y+I) rem M,
    calc_idxs(M, I-1, Xi, Yi, [Xi | Acc]).
