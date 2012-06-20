%% -------------------------------------------------------------------
%%
%% Copyright (c) 2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% This module in bloomerl serves to provide drop-in compatibility with
%% the ebloom module.

-module(ebloom).
-author('Greg Burd <greg@burd.me>').

-export([new/3,
         insert/2,
         contains/2,
         clear/1,
         size/1,
         elements/1,
         effective_fpp/1,
         intersect/2,
         union/2,
         difference/2,
         serialize/1,
         deserialize/1]).


-spec new(integer(), float(), integer()) -> {ok, reference()}.
new(Count, FalseProb, _Seed) ->
    {ok, bloom:new(Count, FalseProb)}.

-spec insert(reference(), binary()) -> ok.
insert(Ref, Bin) ->
    bloom:add_element(Bin, Ref),
    ok.

-spec contains(reference(), binary()) -> true | false.
contains(Ref, Bin) ->
    bloom:is_element(Bin, Ref).

-spec clear(reference()) -> ok.
clear(Ref) ->
    bloom:clear(Ref).

-spec size(reference()) -> integer().
size(Ref) ->
    bloom:filter_size(Ref).

-spec elements(reference()) -> integer().
elements(Ref) ->
    bloom:count(Ref).

-spec effective_fpp(reference()) -> float().
effective_fpp(_Ref) ->
    throw(not_yet_implemented).

-spec intersect(reference(), reference()) -> ok.
intersect(_Ref, _OtherRef) ->
    throw(not_yet_implemented).

-spec union(reference(), reference()) -> ok.
union(_Ref, _OtherRef) ->
    throw(not_yet_implemented).

-spec difference(reference(), reference()) -> ok.
difference(_Ref, _OtherRef) ->
    throw(not_yet_implemented).

-spec serialize(reference()) -> binary().
serialize(Ref) ->
    erlang:term_to_binary(Ref).

-spec deserialize(binary()) -> {ok, reference()}.
deserialize(Bin) ->
    erlang:binary_to_term(Bin).
