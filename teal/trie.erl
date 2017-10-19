-module(trie).

-export([from_list/1]).


from_list(List) ->
    lists:foldl(
      fun ({K,V}, T) ->
              add(K,V,T)
      end,
      #{},
      List).


add([], Value, Trie) ->
    Trie#{[] => lists:append(maps:get([], Trie, []), [Value])};
add([H|T], Value, Trie) ->
    Trie#{H => add(T, Value, maps:get(H, Trie, #{}))}.
