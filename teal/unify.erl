-module(unify).

-export([unify/3, subst/2, reify/3, offset/2, alpha/1]).


lookup(K, []) ->
    {var, K};
lookup(K, [{K,V}|_]) ->
    V;
lookup(K, [_|S]) ->
    lookup(K, S).

lookup_var(V, S) ->
    case lookup(V, S) of
        {var, V1} when V1 =/= V ->
            lookup_var(V1, S);
        Other ->
            Other
    end.

subst({var, V}, S) ->
    case lookup_var(V, S) of
        {var, _} = V1 ->
            V1;
        Other ->
            subst(Other, S)
    end;
subst({term, F, A}, S) when is_atom(F) ->
    {term, F, subst(A, S)};
subst({tuple, F, A}, S) when is_atom(F) ->
    {tuple, F,
     [ E || T <- subst(A, S),
          E <- case T of
                   {tuple, F, A1} -> A1;
                   _ -> [T]
               end ]
    };
subst([], _S) ->
    [];
subst([H|T], S) ->
    [subst(H,S)|subst(T,S)];
subst(Atom, _) when is_atom(Atom) ->
    Atom.

occurs(X, {var, V}, S) ->
    case lookup_var(V ,S) of
        {var, Y} ->
            X =:= Y;
        Other ->
            occurs(X, Other, S)
    end;
occurs(X, {term, F, A}, S) ->
    occurs(X, [F|A], S);
occurs(X, [H|T], S) ->
    occurs(X, H, S) or occurs(X, T, S);
occurs(_, _, _) ->
    false.

unify({var, V1}, Y, S) ->
    case lookup_var(V1, S) of
        {var, X} ->
            case occurs(X, Y, S) of
                true ->
                    case Y of
                        {var, V2} ->
                            case lookup_var(V2, S) of
                                {var, X} ->
                                    S;
                                _ ->
                                    false
                            end;
                        _ ->
                            false
                    end;
                false ->
                    [{X,Y}|S]
            end;
        Other ->
            unify(Other, Y, S)
    end;
unify(X, {var, _} = Y, S) ->
    unify(Y, X, S);
unify({term, F1, A1}, {term, F2, A2}, S) ->
    unify([F1|A1], [F2|A2], S);
unify([], [], S) ->
    S;
unify([H1|T1], [H2|T2], S) ->
    case unify(H1, H2, S) of
    	false ->
    	    false;
    	S1 ->
    	    unify(T1, T2, S1)
    end;
unify(A, A, S) when is_atom(A) ->
    S;
unify(_, _, _) ->
    false.


reify({var, '_'}, S, C) ->
    {{var, C}, S, C+1};
reify({var, V}, S, C) ->
    case lookup_var(V, S) of
        {var, N} when is_integer(N) ->
            {{var, N}, S, C};
        {var, V1} ->
            {{var, C}, [{V1, {var, C}}|S], C+1};
        Other ->
            reify(Other, S, C)
    end;
reify({term, F, A}, S, C) ->
    {[F1|A1], S1, C1} = reify([F|A], S, C),
    {{term, F1, A1}, S1, C1};
reify([H|T], S, C) ->
    {H1, S1, C1} = reify(H, S, C),
    {T1, S2, C2} = reify(T, S1, C1),
    {[H1|T1], S2, C2};
reify(X, S, C) ->
    {X, S, C}.


offset({var, N}, C) when is_integer(N) ->
    {var, N+C};
offset({term, F, A}, C) ->
    [F1|A1] = offset([F|A], C),
    {term, F1, A1};
offset({tuple, F, A}, C) ->
    [F1|A1] = offset([F|A], C),
    {tuple, F1, A1};
offset([H|T], C) ->
    [offset(H, C)|offset(T, C)];
offset(X, _) ->
    X.

find(_, []) ->
    error;
find(K, [{K,V}|_]) ->
    {ok, V};
find(K, [_|T]) ->
    find(K,T).

alpha(Term) ->
    {T, _, C} = alpha(Term, [], 0),
    {T, C}.

alpha({var, V}, S, C) ->
    case find(V, S) of
        {ok, N} ->
            {{var, N}, S, C};
        error ->
            {{var, C}, [{V,C}|S], C+1}
    end;
alpha({tuple, F, A}, S, C) ->
    {[F1|A1], S1, C1} = alpha([F|A], S, C),
    {{tuple, F1, A1}, S1, C1};
alpha({term, F, A}, S, C) ->
    {[F1|A1], S1, C1} = alpha([F|A], S, C),
    {{term, F1, A1}, S1, C1};
alpha([H|T], S, C) ->
    {H1, S1, C1} = alpha(H, S, C),
    {T1, S2, C2} = alpha(T, S1, C1),
    {[H1|T1], S2, C2};
alpha(X, S, C) ->
    {X, S, C}.
