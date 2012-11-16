-module(pers).
-export([read/1, store/5, delete/1]).
%% dets module provides term storage on file
%% returns the object with the key ’perm’ stored in the table ’Name’
read(Name) ->
    {ok, Name} = dets:open_file(Name, []),
    case dets:lookup(Name, perm) of
        [{perm, Pr, Vt, Ac, Pn}] ->
            {Pr, Vt, Ac, Pn};
        [] ->
            {order:null(), order:null(), na, na}
    end.
%% inserts one object {Pr, Vt, Ac, Pn} into the table ’Name’
store(Name, Pr, Vt, Ac, Pn)->
    dets:insert(Name, {perm, Pr, Vt, Ac, Pn}).
delete(Name) ->
    dets:delete(Name, perm),
    dets:close(Name).
