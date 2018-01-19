-module(gen_attachment).

%% Setter
-export([error_response/2]).
%% Getters
-export([error_code/1, error_body/1]).
%% Helper(s)
-export([is_error_response/1]).

-include("kz_att.hrl").

-type settings()         :: kz_data:connection().
-type db_name()          :: kz_term:ne_binary().
-type doc_id()           :: kz_term:ne_binary().
-type att_name()         :: kz_term:ne_binary().
-type contents()         :: kz_term:ne_binary().
-type options()          :: kz_data:options().
-type handler_props()    :: kz_data:connection().
-type error_code()       :: pos_integer() | atom(). % 400, 404, 409, etc.
-type error_body()       :: binary() | % encoded map()
                            bitstring() | % <<"example">>
                            atom(). % 'not_found' | 'return_id_missing' | etc.
-opaque error_response() :: {'error', [{'error_code', error_code()} |
                                       {'error_body', error_body()}]}.
-type put_response()     :: {'ok', [{atom(), [{binary(), binary() | kz_json:object()}]}]} |
                            error_response().
-type fetch_response()   :: {'ok', iodata()} | error_response().

-export_type([settings/0
             ,db_name/0
             ,doc_id/0
             ,att_name/0
             ,contents/0
             ,options/0
             ,handler_props/0
             ,error_response/0
             ,put_response/0
             ,fetch_response/0
             ]).

%% =======================================================================================
%% Callbacks
%% =======================================================================================
-callback put_attachment(settings()
                        ,db_name()
                        ,doc_id()
                        ,att_name()
                        ,contents()
                        ,options()
                        ) -> put_response().

-callback fetch_attachment(handler_props()
                          ,db_name()
                          ,doc_id()
                          ,att_name()
                          ) -> fetch_response().

%% =======================================================================================
%% API
%% =======================================================================================
%% Setter
-spec error_response(error_code(), error_body()) -> error_response().
error_response(ErrorCode, ErrorBody) ->
    {'error', [{'error_code', ErrorCode}, {'error_body', ErrorBody}]}.

%% Getters
-spec error_code(error_response()) -> error_code().
error_code({'error', Reason}) ->
    props:get_value('error_code', Reason).

-spec error_body(error_response()) -> error_body().
error_body({'error', Reason}) ->
    props:get_value('error_body', Reason).

%% Helper(s)
-spec is_error_response(error_response() | any()) -> boolean().
is_error_response({'error', [{'error_code', _Code}, {'error_body', _Body}]})
  when is_atom(_Code); is_integer(_Code) ->
    true;
is_error_response({'error', [{'error_body', _Body}, {'error_code', _Code}]})
  when is_atom(_Code); is_integer(_Code) ->
    true;
is_error_response(_Any) ->
    false.
