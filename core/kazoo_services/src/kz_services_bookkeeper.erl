%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_services_bookkeeper).

-include("services.hrl").

%%%
%%% Transaction functions
%%%
-export([commit_transactions/2]).
-export([charge_transactions/2]).

%%%
%%% Bookkeeper functions
%%%
-export([select_bookkeeper/1]).
-export([check_bookkeeper/2]).

%% applications/crossbar/src/modules/cb_service_plans.erl:198
%% applications/crossbar/src/modules/cb_braintree.erl:580
%% applications/tasks/src/kz_service_sync.erl:293
-export([sync/1
        ,sync/2
        ]).

%%%===================================================================
%%% Transaction functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec commit_transactions(services(), kz_transactions:kz_transactions()) -> ok | error.
commit_transactions(#kz_services{billing_id = BillingId}=Services, Activations) ->
    Bookkeeper = select_bookkeeper(Services),
    Transactions = [Activation
                    || Activation <- Activations,
                       kz_transaction:amount(Activation) > 0
                   ],
    Bookkeeper:commit_transactions(BillingId, Transactions).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec charge_transactions(services(), kz_transactions:kz_transactions()) -> kz_json:objects().
charge_transactions(#kz_services{billing_id = BillingId}=Services, Activations) ->
    Bookkeeper = select_bookkeeper(Services),
    Transactions = [kz_transaction:to_json(Activation)
                    || Activation <- Activations,
                       kz_transaction:amount(Activation) > 0
                   ],
    Bookkeeper:charge_transactions(BillingId, Transactions).

%%%===================================================================
%%% Bookkeeper functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec select_bookkeeper(services() | ne_binary()) -> bookkeeper().
select_bookkeeper(#kz_services{billing_id = BillingId
                              ,account_id = AccountId
                              }
                 ) ->
    BillingIdReseller = kz_services:get_reseller_id(BillingId),
    {'ok', MasterAccountId} = master_account_id(),
    case BillingIdReseller =:= MasterAccountId of
        true -> ?KZ_SERVICE_MASTER_ACCOUNT_BOOKKEEPER;
        false ->
            case BillingIdReseller =:= kz_services:get_reseller_id(AccountId) of
                'true' -> select_bookkeeper(AccountId);
                'false' -> 'kz_bookkeeper_local'
            end
    end;
select_bookkeeper(AccountId) ->
    ResellerId = kz_services:get_reseller_id(AccountId),
    {'ok', MasterAccountId} = master_account_id(),
    case ResellerId =:= MasterAccountId of
        true -> ?KZ_SERVICE_MASTER_ACCOUNT_BOOKKEEPER;
        false ->
            case ?MAYBE_RESELLER_BOOKKEEPER_LOOKUP of
                'true' -> ?KZ_LOOKUP_BOOKKEEPER(ResellerId);
                'false' -> 'kz_bookkeeper_local'
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_bookkeeper(ne_binary(), integer()) -> boolean().
check_bookkeeper(BillingId, Amount) ->
    case select_bookkeeper(BillingId) of
        'kz_bookkeeper_local' ->
            case current_balance(BillingId) of
                {'ok', Balance} -> Balance - Amount >= 0;
                {'error', _R} ->
                    ?LOG_DEBUG("error checking local bookkeeper balance: ~p", [_R]),
                    false
            end;
        Bookkeeper ->
            CurrentStatus = current_service_status(BillingId),
            Bookkeeper:is_good_standing(BillingId, CurrentStatus)
    end.

-spec current_service_status(ne_binary()) -> ne_binary().
current_service_status(AccountId) ->
    {'ok', ServicesJObj} = kz_services:fetch_services_doc(AccountId),
    kzd_services:status(ServicesJObj).

-ifdef(TEST).
current_balance(?UNRELATED_ACCOUNT_ID) -> {ok, 100}.
-else.
current_balance(AccountId) ->
    wht_util:current_balance(AccountId).
-endif.

%%%===================================================================
%%% Sync functions
%%%===================================================================
-spec sync(ne_binary()) -> kz_std_return().
sync(Account) ->
    AccountId = kz_util:format_account_id(Account),
    kz_util:put_callid(<<AccountId/binary, "-sync">>),
    case kz_services:fetch_services_doc(AccountId, 'true') of
        {'error', _}=E -> E;
        {'ok', ServicesJObj} ->
            sync(AccountId, ServicesJObj)
    end.

-spec sync(ne_binary(), kz_json:object()) -> kz_std_return().
sync(AccountId, ServicesJObj) ->
    case kz_services:get_billing_id(AccountId, ServicesJObj) of
        AccountId -> maybe_sync_services(AccountId, ServicesJObj);
        BillingId ->
            io:format("Account ~s is configured to use the credit card of ~s, following billing tree~n"
                     ,[AccountId, BillingId]),
            lager:debug("account ~s is configured to use the credit card of ~s, following billing tree"
                       ,[AccountId, BillingId]),
            sync(BillingId)
    end.

-spec maybe_sync_services(ne_binary(), kzd_services:doc()) -> kz_std_return().
maybe_sync_services(AccountId, ServicesJObj) ->
    case kz_service_plans:create_items(ServicesJObj) of
        {'error', 'no_plans'} ->
            lager:debug("no services plans found"),
            _ = maybe_sync_transactions(AccountId, ServicesJObj),
            _ = mark_clean_and_status(kzd_services:status_good(), ServicesJObj),
            maybe_sync_reseller(AccountId, ServicesJObj);
        {'ok', ServiceItems} ->
            sync_services(AccountId, ServicesJObj, ServiceItems)
    end.

-spec sync_services(ne_binary(), kzd_services:doc(), kz_service_items:items()) -> kz_std_return().
sync_services(AccountId, ServicesJObj, ServiceItems) ->
    try sync_services_bookkeeper(AccountId, ServicesJObj, ServiceItems) of
        'ok' ->
            _ = mark_clean_and_status(kzd_services:status_good(), ServicesJObj),
            io:format("synchronization with bookkeeper complete (good-standing)~n"),
            lager:debug("synchronization with bookkeeper complete (good-standing)"),
            maybe_sync_reseller(AccountId, ServicesJObj);
        'delinquent' ->
            _ = mark_clean_and_status(kzd_services:status_delinquent(), ServicesJObj),
            io:format("synchronization with bookkeeper complete (delinquent)~n"),
            lager:debug("synchronization with bookkeeper complete (delinquent)"),
            maybe_sync_reseller(AccountId, ServicesJObj);
        'retry' ->
            io:format("synchronization with bookkeeper complete (retry)~n"),
            lager:debug("synchronization with bookkeeper complete (retry)"),
            {'error', 'retry'}
    catch
        'throw':{Reason, _}=_R ->
            lager:info("bookkeeper error: ~p", [_R]),
            _ = mark_clean_and_status(kz_term:to_binary(Reason), ServicesJObj),
            maybe_sync_reseller(AccountId, ServicesJObj);
        _E:R ->
            lager:info("unable to sync services(~p): ~p", [_E, R]),
            kz_util:log_stacktrace(),
            {'error', R}
    end.

-spec sync_services_bookkeeper(ne_binary(), kz_json:object(), kz_service_items:items()) -> 'ok' | 'delinquent' | 'retry'.
sync_services_bookkeeper(AccountId, ServicesJObj, ServiceItems) ->
    Bookkeeper = ?MODULE:select_bookkeeper(AccountId),
    lager:debug("attempting to sync with bookkeeper ~s", [Bookkeeper]),
    Result = Bookkeeper:sync(ServiceItems, AccountId),
    maybe_sync_transactions(AccountId, ServicesJObj, Bookkeeper),
    Result.

-spec maybe_sync_transactions(ne_binary(), kzd_services:doc()) -> 'ok'.
-spec maybe_sync_transactions(ne_binary(), kzd_services:doc(), atom()) -> 'ok'.
maybe_sync_transactions(AccountId, ServicesJObj) ->
    Bookkeeper = ?MODULE:select_bookkeeper(AccountId),
    maybe_sync_transactions(AccountId, ServicesJObj, Bookkeeper).

maybe_sync_transactions(AccountId, ServicesJObj, Bookkeeper) ->
    case kzd_services:transactions(ServicesJObj) of
        [] -> 'ok';
        Trs ->
            Transactions = maybe_delete_topup_transaction(AccountId, Trs),
            sync_transactions(AccountId, ServicesJObj, Bookkeeper, Transactions)
    end.

-spec maybe_delete_topup_transaction(ne_binary(), kz_json:objects()) -> kz_json:objects().
maybe_delete_topup_transaction(AccountId, Transactions) ->
    NonTopup = lists:filter(
                 fun(J) ->
                         kz_json:get_integer_value(<<"pvt_code">>, J) =/= ?CODE_TOPUP
                 end, Transactions
                ),
    case NonTopup of
        Transactions -> Transactions;
        _Other ->
            case kz_topup:should_topup(AccountId) of
                'true' -> Transactions;
                'false' -> NonTopup
            end
    end.

-spec sync_transactions(ne_binary(), kzd_services:doc(), atom(), kz_json:objects()) ->
                               'ok'.
sync_transactions(AccountId, ServicesJObj, Bookkeeper, Transactions) ->
    BillingId = kzd_services:billing_id(ServicesJObj),
    FailedTransactions = Bookkeeper:charge_transactions(BillingId, Transactions),
    case kz_datamgr:save_doc(?KZ_SERVICES_DB
                            ,kzd_services:set_transactions(ServicesJObj, FailedTransactions)
                            )
    of
        {'error', _E} ->
            lager:warning("failed to clean pending transactions ~p", [_E]);
        {'ok', _} ->
            handle_topup_transactions(AccountId, Transactions, FailedTransactions)
    end.

-spec handle_topup_transactions(ne_binary(), kz_json:objects(), kz_json:objects() | integer()) -> 'ok'.
handle_topup_transactions(Account, JObjs, Failed) when is_list(Failed) ->
    case did_topup_failed(Failed) of
        'true' -> 'ok';
        'false' -> handle_topup_transactions(Account, JObjs, 3)
    end;
handle_topup_transactions(_, [], _) -> 'ok';
handle_topup_transactions(Account, [JObj|JObjs]=List, Retry) when Retry > 0 ->
    case kz_json:get_integer_value(<<"pvt_code">>, JObj) of
        ?CODE_TOPUP ->
            Amount = kz_json:get_value(<<"pvt_amount">>, JObj),
            Transaction = kz_transaction:credit(Account, Amount),
            Transaction1 = kz_transaction:set_reason(wht_util:topup(), Transaction),
            case kz_transaction:save(Transaction1) of
                {'ok', _} -> 'ok';
                {'error', 'conflict'} ->
                    lager:warning("did not write top up transaction for account ~s already exist for today", [Account]);
                {'error', _E} ->
                    lager:error("failed to write top up transaction ~p , for account ~s (amount: ~p), retrying ~p..."
                               ,[_E, Account, Amount, Retry]
                               ),
                    handle_topup_transactions(Account, List, Retry-1)
            end;
        _ -> handle_topup_transactions(Account, JObjs, 3)
    end;
handle_topup_transactions(Account, _, _) ->
    lager:error("failed to write top up transaction for account ~s too many retries", [Account]).

-spec did_topup_failed(kz_json:objects()) -> boolean().
did_topup_failed(JObjs) ->
    lists:foldl(
      fun(JObj, Acc) ->
              case kz_json:get_integer_value(<<"pvt_code">>, JObj) of
                  ?CODE_TOPUP -> 'true';
                  _ -> Acc
              end
      end
               ,'false'
               ,JObjs
     ).

-spec maybe_sync_reseller(ne_binary(), kzd_services:doc()) -> kz_std_return().
maybe_sync_reseller(AccountId, ServicesJObj) ->
    case kzd_services:reseller_id(ServicesJObj, AccountId) of
        AccountId -> {'ok', ServicesJObj};
        ResellerId ->
            lager:debug("marking reseller ~s as dirty", [ResellerId]),
            kz_services:mark_dirty(ResellerId)
    end.

-spec mark_clean_and_status(ne_binary(), kzd_services:doc()) -> kz_std_return().
mark_clean_and_status(Status, ServicesJObj) ->
    lager:debug("marking services clean with status ~s", [Status]),
    Values = [{?SERVICES_PVT_IS_DIRTY, 'false'}
             ,{?SERVICES_PVT_STATUS, Status}
             ],
    kz_datamgr:save_doc(?KZ_SERVICES_DB, kz_json:set_values(Values, ServicesJObj)).


%% TODO: this is copy/paste from kz_services
-ifdef(TEST).
master_account_id() -> {ok, ?A_MASTER_ACCOUNT_ID}.
-else.
master_account_id() -> kapps_util:get_master_account_id().
-endif.
