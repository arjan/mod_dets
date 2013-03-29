
-module(m_dets).

-behaviour(gen_model).

%% interface functions
-export([
         m_find_value/3,
         m_to_list/2,
         m_value/2,

         lookup/3,
         list/2,
         insert/4,
         delete/3
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(TableName, #m{value=undefined}=M, _Context) ->
    M#m{value=TableName};
m_find_value(Argument, #m{value={Fun, TableName}}, Context) ->
    ?MODULE:Fun(TableName, Argument, Context);
m_find_value(Operation, #m{value=TableName}=M, _Context) ->
    Fun = list_to_existing_atom(z_convert:to_list(Operation)),
    M#m{value={Fun, TableName}}.


%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(_, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.


insert(TableName, Key, Value, Context) ->
    T = ensure_table(TableName, Context),
    dets:insert(T, {Key, Value}).

lookup(TableName, Key, Context) ->
    T = ensure_table(TableName, Context),
    case dets:lookup(T, Key) of
        [Value] -> Value;
        [] -> undefined
    end.

list(TableName, Context) ->
    T = ensure_table(TableName, Context),
    dets:foldr(fun(Item, Acc) ->
                       [Item|Acc]
               end,
               [],
               T).

delete(TableName, Key, Context) ->
    T = ensure_table(TableName, Context),
    dets:delete(T, Key).

ensure_table(TableName, Context) when is_atom(TableName) ->
    TN = z_utils:name_for_host(TableName, Context),
    Path = filename:join(z_path:files_subdir_ensure("dets", Context),
                         atom_to_list(TableName)),
    {ok, Handle} = dets:open_file(TN, [{file, Path}, {type, set}]),
    Handle.



