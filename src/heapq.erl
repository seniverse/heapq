-module(heapq).

-export(
   [new/0,
    size/1,
    find_min/1,
    insert/3,
    delete_min/1,
    update_key/3]).

new() ->
    {0, leaf, #{}}.

size({_, _, Map}) ->
    map_size(Map).

find_min({_, leaf, _}) ->
    none;
find_min({_, {tree, K, P, _, _}, _}) ->
    {ok, K, P}.

insert(K, P, {Height, Tree, Map}) ->
    Position = map_size(Map) + 1,
    Height1 =
        Height +
        case Position >= 1 bsl Height of
            true -> 1;
            false -> 0
        end,
    Mask = (1 bsl Height1) bsr 1,
    {Tree1, Map1} = insert(Mask bsr 1, Position - Mask, 1, 0, K, P, Tree, Map),
    {Height1, Tree1, Map1}.

insert(_, _, Prefix, Path, K, P, leaf, Map) ->
    {{tree, K, P, leaf, leaf}, Map#{K => Prefix bor Path}};
insert(Mask, Position, Prefix, Path, K, P, {tree, K0, P0, L0, R0}, Map)
  when P < P0 ->
    insert(Mask, Position, Prefix, Path, K0, P0, {tree, K, P, L0, R0}, Map#{K => Prefix bor Path});
insert(Mask, Position, Prefix, Path, K, P, {tree, K0, P0, L0, R0}, Map)
  when (Position band Mask) == 0 ->
    {L1, Map1} = insert(Mask bsr 1, Position, Prefix bsl 1, Path, K, P, L0, Map),
    {{tree, K0, P0, L1, R0}, Map1};
insert(Mask, Position, Prefix, Path, K, P, {tree, K0, P0, L0, R0}, Map) ->
    {R1, Map1} = insert(Mask bsr 1, Position - Mask, Prefix bsl 1, Prefix bor Path, K, P, R0, Map),
    {{tree, K0, P0, L0, R1}, Map1}.

delete_last({Height, Tree, Map}) ->
    Position = map_size(Map),
    Mask = (1 bsl Height) bsr 1,
    Height1 =
        Height -
        case Position =< Mask of
            true -> 1;
            false -> 0
        end,
    {K, P, Tree1} = delete_last(Mask bsr 1, Position - Mask, Tree),
    {K, P, {Height1, Tree1, Map}}.

delete_last(_, _, {tree, K, P, leaf, leaf}) ->
    {K, P, leaf};
delete_last(Mask, Position, {tree, K0, P0, L0, R0})
  when (Position band Mask) == 0 ->
    {K, P, L1} = delete_last(Mask bsr 1, Position, L0),
    {K, P, {tree, K0, P0, L1, R0}};
delete_last(Mask, Position, {tree, K0, P0, L0, R0}) ->
    {K, P, R1} = delete_last(Mask bsr 1, Position - Mask, R0),
    {K, P, {tree, K0, P0, L0, R1}}.

siftdown(Prefix, Path, {tree, K0, P0, {tree, K1, P1, L1, R1}, leaf}, Map)
  when P0 > P1 ->
    {L, Map1} = siftdown(Prefix bsl 1, Path, {tree, K0, P0, L1, R1}, Map#{K1 => Prefix bor Path}),
    {{tree, K1, P1, L, leaf}, Map1};
siftdown(
  Prefix,
  Path,
  {tree, K0, P0,
   {tree, K1, P1, L1, R1},
   {tree, _, P2, _, _} = R0},
  Map)
  when P0 > P1, P2 >= P1 ->
    {L, Map1} = siftdown(Prefix bsl 1, Path, {tree, K0, P0, L1, R1}, Map#{K1 => Prefix bor Path}),
    {{tree, K1, P1, L, R0}, Map1};
siftdown(
  Prefix,
  Path,
  {tree, K0, P0,
   {tree, _, P1, _, _} = L0,
   {tree, K2, P2, L2, R2}},
  Map)
  when P0 > P2, P1 > P2 ->
    {R, Map1} = siftdown(Prefix bsl 1, Prefix bor Path, {tree, K0, P0, L2, R2}, Map#{K2 => Prefix bor Path}),
    {{tree, K2, P2, L0, R}, Map1};
siftdown(Prefix, Path, {tree, K, _, _, _}=Tree, Map) ->
    {Tree, Map#{K => Prefix bor Path}}.

delete_min({_, {tree, _, _, _, _}, _}=Queue) ->
    case delete_last(Queue) of
        {K, P, {Height, {tree, K1, P1, L, R}, Map}} ->
            {Tree1, Map1} = siftdown(1, 0, {tree, K, P, L, R}, Map#{K => 1}),
            {K1, P1, {Height, Tree1, maps:remove(K1, Map1)}};
        {K, P, {Height, Tree, Map}} ->
            {K, P, {Height, Tree, maps:remove(K, Map)}}
    end.

update_key(Key, Fun, {Height, Tree, Map}) ->
    Pos = maps:get(Key, Map),
    {Tree1, Map1} = update_key(Pos, 1, 0, Fun, Tree, Map),
    {Height, Tree1, Map1}.

update_key(1, Prefix, Path, Fun, {tree, K, P, L, R}, Map) ->
    siftdown(Prefix, Path, {tree, K, Fun(P), L, R}, Map);
update_key(Pos, Prefix, Path, Fun, {tree, K, P, L, R}, Map) when (Pos band 1) == 0 ->
    case update_key(Pos bsr 1, Prefix bsl 1, Path, Fun, L, Map) of
        {{tree, K1, P1, L1, R1}, Map1} when P1 < P ->
            {{tree, K1, P1, {tree, K, P, L1, R1}, R},
             Map1#{K1 => Prefix bor Path, K => (Prefix bsl 1) bor Path}};
        {L1, Map1} ->
            {{tree, K, P, L1, R}, Map1}
    end;
update_key(Pos, Prefix, Path, Fun, {tree, K, P, L, R}, Map) ->
    case update_key(Pos bsr 1, Prefix bsl 1, Prefix bor Path, Fun, R, Map) of
        {{tree, K1, P1, L1, R1}, Map1} when P1 < P ->
            {{tree, K1, P1, L, {tree, K, P, L1, R1}},
             Map1#{K1 => Prefix bor Path, K => (Prefix bsl 1) bor Prefix bor Path}};
        {R1, Map1} ->
            {{tree, K, P, L, R1}, Map1}
    end.
