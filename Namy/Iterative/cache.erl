-module(cache).
-export([lookup/2, new/0, add/4, remove/2]).

lookup(Path, Cache)->
    case lists:keyfind(Path, 1, Cache) of
            false -> unknown;
            {Path,TTL,Node} ->
                case time:valid(TTL,time:now()) of
                    true -> Node;
                    false -> invalid
                end
    end.

new()-> [].

add(Path, TTL, Node, Cache)->
    lists:keystore(Path, 1, Cache, {Path,TTL,Node}).

remove(Path, Cache)->
    lists:keydelete(Path, 1, Cache).
