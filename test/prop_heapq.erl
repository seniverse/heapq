-module(prop_heapq).
-compile({no_auto_import,[size/1]}).
-include_lib("proper/include/proper.hrl").

from_map(Map) ->
    maps:fold(
      fun(K, P, Queue) ->
              heapq:insert(K, P, Queue)
      end,
      heapq:new(),
      Map).

is_heap(leaf) ->
    true;
is_heap({tree, _, _, leaf, leaf}) ->
    true;
is_heap({tree, _, P0, {tree, _, P1, _, _}=L, leaf}) ->
    (P0 =< P1) and is_heap(L);
is_heap({tree, _, P0, {tree, _, P1, _, _}=L, {tree, _, P2, _, _}=R}) ->
    (P0 =< P1) and (P0 =< P2) and is_heap(L) and is_heap(R);
is_heap(_) ->
    false.

prop_heap({_, Tree, _}) ->
    is_heap(Tree).

height(leaf) ->
    0;
height({tree, _, _, L, R}) ->
    max(height(L), height(R)) + 1.

prop_height({Height, Tree, _}) ->
    Height == height(Tree).

on_path(K, 1, {tree, K, _, _, _}) ->
    true;
on_path(K, Path, {tree, _, _, L, _}) when Path > 1, (Path band 1) == 0 ->
    on_path(K, Path bsr 1, L);
on_path(K, Path, {tree, _, _, _, R}) when Path > 1 ->
    on_path(K, Path bsr 1, R);
on_path(_, _, _) ->
    false.

prop_path({_, Tree, Map}) ->
    maps:fold(
      fun (_, _, false) ->
              false;
          (K, Path, true) ->
              on_path(K, Path, Tree)
      end,
      true,
      Map).

size(leaf) ->
    0;
size({tree, _, _, L, R}) ->
    1 + size(L) + size(R).

prop_size({_, Tree, Map}) ->
    size(Tree) == map_size(Map).

prop_heapq(Queue) ->
    prop_heap(Queue) and prop_height(Queue) and prop_size(Queue) and prop_path(Queue).

prop_heapq() ->
    ?FORALL(Map, map(integer(),integer()), prop_heapq(from_map(Map))).

prop_min(_, [], Rest, Q) ->
    prop_min(Rest, Q);
prop_min(P, Ks, Rest, Q) ->
    case heapq:find_min(Q) of
        {ok, K, P} ->
            case heapq:delete_min(Q) of
                {K, P, Q1} ->
                    case lists:delete(K, Ks) of
                        Ks ->
                            false;
                        Ks1 ->
                            prop_heapq(Q1) and (heapq:size(Q) == heapq:size(Q1) + 1) and prop_min(P, Ks1, Rest, Q1)
                    end;
                {_, _, _} ->
                    false
            end;
        _ ->
            false
    end.

prop_min([], _) ->
    true;
prop_min([{_,P}|_]=L, Q) ->
    {Current, Rest} = lists:splitwith(fun({_,P1}) -> P1 =:= P end, L),
    prop_min(P, [K || {K,_} <- Current], Rest, Q).

prop_min() ->
    ?FORALL(
       Map,
       map(integer(), integer()),
       prop_min(
         lists:keysort(2, maps:to_list(Map)),
         from_map(Map))).

prop_update([], _) ->
    true;
prop_update([{K, P}|T], Q) ->
    Q1 =
        heapq:update_key(
          K,
          fun(_) -> P end,
          Q),
    prop_heapq(Q1) and (heapq:size(Q) == heapq:size(Q1)) and prop_update(T, Q1).

prop_update() ->
    ?FORALL(
       {Map, List},
       ?LET(
          M,
          ?SUCHTHAT(X, map(integer(), integer()), map_size(X) > 0),
          {M, list(tuple([oneof(maps:keys(M)), integer()]))}),
       prop_update(List, from_map(Map))).
