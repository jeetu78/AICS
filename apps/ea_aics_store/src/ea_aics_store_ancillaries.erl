%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System
%%% ancillaries store interface
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_store_ancillaries).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([create/1,
         read/0,
         read/1,
         update/2,
         delete/1,
         record_fields_keys/1,
         result_fields_keys/0,
         parse_query_result_row/1]).

-export_type([]).

-include_lib("mysql/include/mysql.hrl").
-include_lib("ea_aics_core/include/ea_aics_core.hrl").

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Creates an ancillary in the store.
%% @end
%%------------------------------------------------------------------------------

-spec create(#ea_aics_ancillary{}) -> {ok, #ea_aics_ancillary{}}.

create(AncillaryInput) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_create_transaction(ConnectionPid, AncillaryInput)
        end).

%%------------------------------------------------------------------------------
%% @doc Reads the ancillaries from the store.
%% @end
%%------------------------------------------------------------------------------

-spec read() -> {ok, [#ea_aics_ancillary{}]}.

read() ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_read(ConnectionPid)
        end).

%%------------------------------------------------------------------------------
%% @doc Reads the ancillary from the store.
%% @end
%%------------------------------------------------------------------------------

-spec read(binary()) -> {ok, #ea_aics_ancillary{}} | {error, not_found}.

read(AncillaryId) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_read(ConnectionPid, AncillaryId)
        end).

%%------------------------------------------------------------------------------
%% @doc Updates the ancillary in the store.
%% @end
%%------------------------------------------------------------------------------

-spec update(binary(), term()) -> {ok, #ea_aics_ancillary{}} | {error, not_found}.

update(AncillaryId, AncillaryUpdates) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_update_transaction(ConnectionPid, AncillaryId, AncillaryUpdates)
        end).

%%------------------------------------------------------------------------------
%% @doc Deletes the ancillary from the store.
%% @end
%%------------------------------------------------------------------------------

-spec delete(binary()) -> ok | {error, not_found}.

delete(AncillaryId) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_delete(ConnectionPid, AncillaryId)
        end).

%%------------------------------------------------------------------------------
%% @doc Exports the ancillary record fields structure.
%% @end
%%------------------------------------------------------------------------------

-spec record_fields_keys(atom()) -> [{atom(), atom()}].

record_fields_keys(Prefix) ->
    [{Prefix, Key} || Key <- record_fields_keys()].

%%------------------------------------------------------------------------------
%% @doc Exports the ancillary result fields structure.
%% @end
%%------------------------------------------------------------------------------

-spec result_fields_keys() -> [{atom(), atom()}].

result_fields_keys() ->
    record_fields_keys('AM').

%% ===================================================================
%% Internal functions
%% ===================================================================

do_create_transaction(ConnectionPid, AncillaryInput) ->
    {atomic, Response} =
        mysql_conn:transaction(ConnectionPid,
            fun() ->
                {ok, AncillaryId} = do_create(ConnectionPid, AncillaryInput),
                {ok, _Ancillary} = do_read(ConnectionPid, AncillaryId)
            end, self()),
    Response.

do_update_transaction(ConnectionPid, AncillaryId, AncillaryUpdates) ->
    {atomic, Response} =
        mysql_conn:transaction(ConnectionPid,
            fun() ->
                case do_update(ConnectionPid, AncillaryId, AncillaryUpdates) of
                    ok ->
                        {ok, _Ancillary} = do_read(ConnectionPid, AncillaryId);
                    {error, not_found} ->
                        {error, not_found}
                end
            end, self()),
    Response.

do_create(ConnectionPid, AncillaryInput) ->
    AncillaryId = ea_aics_store:generate_uuid(),
    RecordFieldsInputs = record_fields(AncillaryId, AncillaryInput),
    Query = sqerl:sql({insert, 'ANCILLARY_MASTER', RecordFieldsInputs}, true),
    {updated, QueryResult} = ea_aics_store:do_fetch(ConnectionPid, Query),
    #mysql_result{affectedrows = 1} = QueryResult,
    {ok, AncillaryId}.

do_read(ConnectionPid) ->
    ResultFieldsKeys = result_fields_keys(),
    Query = sqerl:sql({select, ResultFieldsKeys,
        {from, {'ANCILLARY_MASTER', as, 'AM'}}}, true),
    {data, QueryResult} = ea_aics_store:do_fetch(ConnectionPid, Query),
    #mysql_result{fieldinfo = _Fields,
                  rows = _Rows} = QueryResult,
    Ancillaries = parse_query_result(QueryResult),
    {ok, Ancillaries}.

do_read(ConnectionPid, AncillaryId) ->
    ResultFieldsKeys = result_fields_keys(),
    Query = sqerl:sql({select, ResultFieldsKeys,
        {from, {'ANCILLARY_MASTER', as, 'AM'}},
        {where, {'UUID', '=', AncillaryId}}}, true),
    {data, QueryResult} = ea_aics_store:do_fetch(ConnectionPid, Query),
    #mysql_result{fieldinfo = _Fields,
                  rows = _Rows} = QueryResult,
    case parse_query_result(QueryResult) of
        [Ancillary] ->
            {ok, Ancillary};
        [] ->
            {error, not_found}
    end.

do_update(ConnectionPid, AncillaryId, _AncillaryUpdates) ->
    Query = sqerl:sql({update, 'ANCILLARY_MASTER', [],
        {where, {'UUID', '=', AncillaryId}}}, true),
    case ea_aics_store:do_fetch(ConnectionPid, Query) of
        {updated, #mysql_result{affectedrows = 1}} ->
            ok;
        {updated, #mysql_result{affectedrows = 0}} ->
            {error, not_found}
    end.

do_delete(ConnectionPid, AncillaryId) ->
    Query = sqerl:sql({delete, {from, 'ANCILLARY_MASTER'},
        {where, {'UUID', '=', AncillaryId}}}, true),
    case ea_aics_store:do_fetch(ConnectionPid, Query) of
        {updated, #mysql_result{affectedrows = 1}} ->
            ok;
        {updated, #mysql_result{affectedrows = 0}} ->
            {error, not_found}
    end.

parse_query_result(#mysql_result{rows = QueryResultRows} = _QueryResult) ->
    [parse_query_result_row(QueryResultRow) || QueryResultRow <- QueryResultRows].

parse_query_result_row(QueryResultRow) ->
    [AncillaryId, Anc_GroupCode, Web_Description,
        Anc_ImageThumbnailUrl, Anc_Price, Anc_Currency, Anc_Tax,
        Anc_DiscountPcnt, Anc_CommercialName] = QueryResultRow,
    #ea_aics_ancillary{id = AncillaryId,
                       group_code = Anc_GroupCode,
                       web_description = Web_Description,
                       image_thumbnail_url = Anc_ImageThumbnailUrl,
                       price = Anc_Price,
                       currency = Anc_Currency,
                       tax = Anc_Tax,
                       discount_pcnt = Anc_DiscountPcnt,
                       commercial_name = Anc_CommercialName}.

record_fields_keys() ->
    ['UUID', 'GROUP_CODE','WEB_DESCRIPTION', 'IMAGE_THUMBNAIL_URL',
        'PRICE', 'CURRENCY', 'TAX',
        'DISCOUNT_PCNT', 'COMMERCIAL_NAME'].

record_input_values(AncillaryId, #ea_aics_ancillary{} = AncillaryInput) ->
    Anc_MasterCode = record_input_value(AncillaryInput#ea_aics_ancillary.master_code),
    Anc_ServiceProviderId = record_input_value(AncillaryInput#ea_aics_ancillary.service_provider_id),
    Anc_SubCode = record_input_value(AncillaryInput#ea_aics_ancillary.sub_code),
    Anc_GroupCode = record_input_value(AncillaryInput#ea_aics_ancillary.group_code),
    Anc_SubGroup = record_input_value(AncillaryInput#ea_aics_ancillary.sub_group),
    Anc_Description1 = record_input_value(AncillaryInput#ea_aics_ancillary.description1),
    Anc_Description2 = record_input_value(AncillaryInput#ea_aics_ancillary.description2),
    Anc_ImageThumbnailUrl = record_input_value(AncillaryInput#ea_aics_ancillary.image_thumbnail_url),
    Anc_ImageLargeUrl = record_input_value(AncillaryInput#ea_aics_ancillary.image_large_url),
    Anc_ToolTip = record_input_value(AncillaryInput#ea_aics_ancillary.tooltip),
    Anc_Price = record_input_value(AncillaryInput#ea_aics_ancillary.price),
    Anc_Currency = record_input_value(AncillaryInput#ea_aics_ancillary.currency),
    Anc_Tax = record_input_value(AncillaryInput#ea_aics_ancillary.tax),
    Anc_IsDiscount = record_input_value(AncillaryInput#ea_aics_ancillary.is_discount),
    Anc_DiscountDesc = record_input_value(AncillaryInput#ea_aics_ancillary.discount_desc),
    Anc_DiscountPcnt = record_input_value(AncillaryInput#ea_aics_ancillary.discount_pcnt),
    Anc_CommercialName = record_input_value(AncillaryInput#ea_aics_ancillary.commercial_name),
    Anc_RFIC = record_input_value(AncillaryInput#ea_aics_ancillary.rfic),
    [AncillaryId, Anc_MasterCode, Anc_ServiceProviderId, Anc_SubCode,
     Anc_GroupCode, Anc_SubGroup, Anc_Description1, Anc_Description2,
     Anc_ImageThumbnailUrl, Anc_ImageLargeUrl, Anc_ToolTip, Anc_Price,
     Anc_Currency, Anc_Tax, Anc_IsDiscount, Anc_DiscountDesc, Anc_DiscountPcnt,
     Anc_CommercialName, Anc_RFIC].

record_input_value(undefined) ->
    null;
record_input_value(Value) ->
    Value.

record_fields(AncillaryId, AncillaryInput) ->
    Fields = ['UUID', 'ANC_MASTER_CODE', 'SERVICE_PROVIDER_ID', 'SUB_CODE', 'GROUP_CODE',
        'SUB_GROUP', 'DESCRIPTION1', 'DESCRIPTION2', 'IMAGE_THUMBNAIL_URL',
        'IMAGE_LARGE_URL', 'IMAGE_TOOL_TIP', 'PRICE', 'CURRENCY', 'TAX',
        'IS_DISCOUNT', 'DISCOUNT_DESC', 'DISCOUNT_PCNT', 'COMMERCIAL_NAME', 'RFIC'],
    lists:zip(Fields, record_input_values(AncillaryId, AncillaryInput)).

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

module_test_() ->

    ConnectionPid = self(),

    {foreach,
     fun()  ->
        ok = meck:new(mysql_conn, [non_strict])
     end,
     fun(_) ->
        ?assert(meck:validate(mysql_conn)),
        ok = meck:unload(mysql_conn)
     end,
     [
        {"create",
            [
                fun() ->
                    AncillaryId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AncillaryInputs = ea_aics_store_test:ancillary_input(),

                    ok = meck:new(uuid, [non_strict, passthrough]),
                    ok = meck:expect(uuid, get_v4, [], AncillaryId),
                    ok = meck:expect(uuid, uuid_to_string, ['_', '_'], AncillaryId),

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch({ok, AncillaryId}, do_create(ConnectionPid, AncillaryInputs)),

                    ok = meck:wait(uuid, get_v4, '_', 1000),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000),

                    ?assert(meck:validate(uuid)),
                    ok = meck:unload(uuid)
                end
            ]
        },
        %%{"read",
        %%    [
        %%        fun() ->
        %%            AncillaryId_1 = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
        %%            AncillaryId_2 = <<"c25a06bb2764493d97fbecbda9300b67">>,

        %%            AncillaryRows = ea_aics_store_test:ancillary_rows([AncillaryId_1, AncillaryId_2]),
        %%            ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {data, #mysql_result{rows = AncillaryRows}}),

        %%            ?assertMatch({ok, [#ea_aics_ancillary{id = AncillaryId_1},
        %%                               #ea_aics_ancillary{id = AncillaryId_2}]}, do_read(ConnectionPid)),

        %%            ok = meck:wait(mysql_conn, fetch, '_', 1000)
        %%        end
        %%    ]
        %%},
        %%{"read",
        %%    [
        %%        fun() ->
        %%            AncillaryId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,

        %%            AncillaryRows = ea_aics_store_test:ancillary_rows([AncillaryId]),
        %%            ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {data, #mysql_result{rows = AncillaryRows}}),

        %%            ?assertMatch({ok, #ea_aics_ancillary{id = AncillaryId}}, do_read(ConnectionPid, AncillaryId)),

        %%            ok = meck:wait(mysql_conn, fetch, '_', 1000)
        %%        end
        %%    ]
        %%},
        %%{"read",
        %%    [
        %%        fun() ->
        %%            AncillaryId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,

        %%            ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {data, #mysql_result{rows = []}}),

        %%            ?assertMatch({error, not_found}, do_read(ConnectionPid, AncillaryId)),

        %%            ok = meck:wait(mysql_conn, fetch, '_', 1000)
        %%        end
        %%    ]
        %%},
        {"update",
            [
                fun() ->
                    AncillaryId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AncillaryUpdates = [],

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch(ok, do_update(ConnectionPid, AncillaryId, AncillaryUpdates)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"delete",
            [
                fun() ->
                    AncillaryId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch(ok, do_delete(ConnectionPid, AncillaryId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"delete",
            [
                fun() ->
                    AncillaryId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 0}}),

                    ?assertMatch({error, not_found}, do_delete(ConnectionPid, AncillaryId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        }
     ]
    }.

-endif.
