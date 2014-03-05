%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600HZ, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(kazoo_mod).

-include("kazoo_mod.hrl").

-export([get_results/3]).
-export([open_doc/2, open_doc/3, open_doc/4]).
-export([save_doc/2, save_doc/3, save_doc/4]).
-export([get_modb/1, get_modb/2, get_modb/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
get_results(Account, View, ViewOptions) ->
    get_results(Account, View, ViewOptions, 3).

get_results(Account, View, ViewOptions, 0) ->
    {'error', 'retry_exceeded'};
get_results(Account, View, ViewOptions, Retry) ->
    AccountMODb = get_modb(Account, ViewOptions),
    EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    case couch_mgr:get_results(EncodedMODb, View, ViewOptions) of
        {'error', 'not_found'} ->
            io:format("kazoo_mod.erl:MARKER:34 ~p~n", [{not_found, AccountMODb}]),
            get_results_not_found(Account, View, ViewOptions, Retry);
        Results ->
            io:format("kazoo_mod.erl:MARKER:37 ~p~n", [Results]),
            Results
    end.

get_results_not_found(Account, View, ViewOptions, Retry) ->
    AccountMODb = get_modb(Account, ViewOptions),
    EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    case couch_mgr:db_exists(EncodedMODb) of
        'true' ->
            init_db(AccountMODb),
            get_results(Account, View, ViewOptions, Retry-1);
        'false' ->
            case maybe_create(AccountMODb) of
                'true' ->
                    get_results(Account, View, ViewOptions, Retry-1);
                'false' -> {'error', 'modb_too_old'}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
open_doc(Account, DocId) ->
    AccountMODb = get_modb(Account),
    couch_open(AccountMODb, DocId).

open_doc(Account, DocId, Timestamp) ->
    AccountMODb = get_modb(Account, Timestamp),
    couch_open(AccountMODb, DocId).

open_doc(Account, DocId, Year, Month) ->
    AccountMODb = get_modb(Account, Year, Month),
    couch_open(AccountMODb, DocId).

couch_open(AccountMODb, DocId) ->
    EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    case couch_mgr:open_doc(EncodedMODb, DocId) of
        {'ok', _}=Ok -> Ok;
        {'error', _E}=Error ->
            lager:error("fail to opend doc ~p in ~p reason: ~p", [DocId, EncodedMODb, _E]),
            Error
    end.


%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
save_doc(Account, Doc) ->
    AccountMODb = get_modb(Account),
    couch_save(AccountMODb, Doc, 3).

save_doc(Account, Doc, Timestamp) ->
    AccountMODb = get_modb(Account, Timestamp),
    couch_save(AccountMODb, Doc, 3).

save_doc(Account, Doc, Year, Month) ->
    AccountMODb = get_modb(Account, Year, Month),
    couch_save(AccountMODb, Doc, 3).

couch_save(AccountMODb, Doc, 0) ->
    lager:error("failed to save doc in ~p", AccountMODb);
couch_save(AccountMODb, Doc, Retry) ->
     EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    case couch_mgr:save_doc(EncodedMODb, Doc) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            lager:warning("modb ~p not found creating...", [AccountMODb]),
            _ = maybe_create(AccountMODb),
            couch_save(AccountMODb, Doc, Retry-1);
        {'error', _E}=Error ->
            lager:error("account mod save error: ~p", [_E]),
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
maybe_create(<<_:32/binary, "-", Year:4/binary, Month:2/binary>>=AccountMODb) ->
    {Y, M, _} = erlang:date(),
    case {wh_util:to_binary(Y), wh_util:pad_month(M)} of
        {Year, Month} ->
            io:format("kazoo_mod.erl:MARKER:126 ~p~n", [AccountMODb]),
            create(AccountMODb),
            'true';
        _ ->
            io:format("kazoo_mod.erl:MARKER:129 ~p~n", [AccountMODb]),
            'false'
    end.

create(AccountMODb) ->
    io:format("kazoo_mod.erl:MARKER:134 ~p~n", [{create, AccountMODb}]),
    EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    _ = couch_mgr:db_create(EncodedMODb),
    _ = init_db(EncodedMODb),
    create_routines(AccountMODb).

init_db(AccountMODb) ->
    io:format("kazoo_mod.erl:MARKER:141 ~p~n", [{init, AccountMODb}]),
    EncodedMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    _ = couch_mgr:revise_views_from_folder(EncodedMODb, ?MODULE),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
create_routines(AccountMODb) ->
    Routines = whapps_config:get(?CONFIG_CAT, <<"routines">>, []),
    lists:foldl(
        fun(Mod, _) ->
            Module = wh_util:to_atom(Mod),
            _ = Module:modb(AccountMODb),
            'ok'
        end
        ,'ok'
        ,Routines
    ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
get_modb(Account) ->
    {Year, Month, _} = erlang:date(),
    get_modb(Account, Year, Month).

get_modb(Account, Props) when is_list(Props) ->
    case props:get_value('month', Props) of
        'undefined' -> get_modb(Account);
        Month ->
            case props:get_value('year', Props) of
                'undefined' ->
                    {Year, _, _} = erlang:date(),
                    get_modb(Account, Year, Month);
                Year ->
                    get_modb(Account, Year, Month)
             end
     end;
get_modb(Account, Timestamp) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    AccountId = wh_util:format_account_id(Account, 'raw'),
    <<AccountId/binary
      ,"-"
      ,(wh_util:to_binary(Year))/binary
      ,(wh_util:pad_month(Month))/binary>>.

get_modb(Account, Year, Month) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    <<AccountId/binary
      ,"-"
      ,(wh_util:to_binary(Year))/binary
      ,(wh_util:pad_month(Month))/binary>>.
