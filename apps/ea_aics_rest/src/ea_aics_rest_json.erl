%%%=============================================================================
%%% @author Ramon Lastres Guerrero <ramon.lastres@erlang-solutions.com>
%%% @doc AICS rest JSON validation and parsing functions. For the resources
%%% that use JSON as an input, it generates the internal records with the needed
%%% fields.
%%% @end
%%%=============================================================================
-module(ea_aics_rest_json).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([parse/2]).

%%%----------------------------------------------------------------------------
%%% Types
%%%----------------------------------------------------------------------------

-type validation_spec() :: nonempty_list({binary(), mandatory | optional,
                                          field_type()}).

-type field_type() :: 'integer' | 'boolean' | 'binary' |
    'natural' | 'string' | 'float' | json_type().

-type json_type() :: {'json', validation_spec()}.

-type external_parameters() :: nonempty_list({binary(), any()}).

-type internal_parameters() :: nonempty_list({record_field_name(),
                                              record_field_value()}).

-type record_field_name() :: binary().

-type record_field_value() :: integer() | boolean() | binary() |
    string() | float() | internal_parameters().

-type validation_error() :: {'invalid_json', {error_reason(), any()},
                             external_parameters()}.

-type error_reason() :: 'missing_parameter' | 'unknown_parameter' |
    'badtype_parameter' | 'duplicated_parameters' | 'unexpected_json'.

%%%----------------------------------------------------------------------------
%%% API functions
%%%----------------------------------------------------------------------------

%%%----------------------------------------------------------------------------
%%% @doc It receives the resource tye and the fields of the JSON (output of 
%%% jsx:decode())  and returns the internal record representation of the 
%%% resource. In the process, it validates the input values and returns an
%%% error tuple in case of invalid input.
%%% @ TODO:
%%% At the moment it just validates and returns a list of fields. It should
%%% actually generate the record (we might use exprecs for that).
%%% @end
%%%----------------------------------------------------------------------------
%-spec parse(external_spec()) -> internal_spec().
-spec parse(validation_spec(), external_parameters()) ->
    internal_parameters() | validation_error().
parse(ValidationSpec, JsonFields) ->
    try
        validate(ValidationSpec, JsonFields)
        %generate the record here
        %We need to avoid too much code, maybe use parse_transform/exprecs?
    catch
        Reason ->
            % We should maybe log something here with Lager?
            {invalid_json, Reason, JsonFields}
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions.
%%%----------------------------------------------------------------------------
-spec validate(validation_spec(), external_parameters()) ->
    internal_parameters().
validate(ValidationSpec, Fields) ->
    validate_simple_json(Fields),
    validate_mandatory(ValidationSpec, Fields),
    validate_unknown(ValidationSpec, Fields),
    validate_duplicated(ValidationSpec, Fields),
    validate_type(ValidationSpec, Fields).

% If it is a list of JSONs, we return an error.
-spec validate_simple_json(external_parameters()) -> ok | 'no_return'.
validate_simple_json(Fields) ->
    lists:foreach(fun(Field) ->
                case is_list(Field) of
                    false ->
                        ok;
                    true ->
                        throw({unexpected_json, Fields})
                end
        end, Fields).

-spec validate_mandatory(validation_spec(), external_parameters()) ->
    ok | 'no_return'.
validate_mandatory(ValidationSpec, Fields) ->
    lists:foreach(fun({Name, mandatory, _}) ->
                check_mandatory(Name, Fields);
            (_) ->
                ok
        end, ValidationSpec).

-spec check_mandatory(record_field_name(), external_parameters()) ->
    ok | 'no_return'.
check_mandatory(Name, Fields) ->
    case proplists:is_defined(Name, Fields) of
        true ->
            ok;
        false ->
            throw({missing_parameter, Name})
    end.

-spec validate_unknown(validation_spec(), external_parameters()) ->
    ok | 'no_return'.
validate_unknown(_ValidationSpec, []) ->
    ok;
validate_unknown(_ValidationSpec, [{}])->
    throw({unknown_parameter, {}});
validate_unknown(ValidationSpec, Fields) ->
    ValidFields = [Field || {Field, _, _} <- ValidationSpec],
    lists:foreach(fun({Field, _}) ->
                case lists:member(Field, ValidFields) of
                    true ->
                        ok;
                    false ->
                        throw({unknown_parameter, Field})
                end
        end, Fields).

-spec validate_duplicated(validation_spec(), external_parameters()) ->
    ok | 'no_return'.
validate_duplicated(_ValidationSpec, Fields) ->
    GivenFields = [GivenField || {GivenField, _} <- Fields],
    case lists:sort(GivenFields) =:= lists:usort(GivenFields) of
        true ->
            ok;
        _ ->
            throw({duplicated_parameters, GivenFields})
    end.

-spec validate_type(validation_spec(), external_parameters()) ->
    internal_parameters().
validate_type(ValidationSpec, Fields) ->
    lists:map(fun({Name, Value}) ->
                {_, _, Type} = lists:keyfind(Name, 1, ValidationSpec),
                ValidatedValue = type_validation(Name, Value, Type),
                {Name, ValidatedValue}
        end, Fields).

-spec type_validation(record_field_name(), any(), field_type()) ->
    record_field_value() | 'no_return'.
type_validation(_, Value, integer) when is_integer(Value) ->
    Value;
type_validation(_, Value, natural) when is_integer(Value), Value >= 0 ->
    Value;
type_validation(_, Value, boolean) when is_boolean(Value) ->
    Value;
type_validation(_, Value, binary) when is_binary(Value) ->
    Value;
type_validation(_, Value, float) when is_float(Value) ->
    Value;
% We assume input strings to be UTF8
type_validation(Name, Value, string) when is_list(Value) ->
    case io_lib:char_list(Value) of
        true ->
            Value;
        false ->
            throw({badtype_parameter, Name})
    end;
% We assume input strings to be UTF8
type_validation(Name, Value, string) when is_binary(Value) ->
    NewValue = unicode:characters_to_list(Value),
    case io_lib:char_list(NewValue) of
        true ->
            NewValue;
        false ->
            throw({badtype_parameter, Name})
    end;
%Recursive validation for nested JSONs.
type_validation(_Name, Value, {json, Spec}) ->
    validate(Spec, Value);
type_validation(Name, _, _) ->
    throw({badtype_parameter, Name}).

%%%----------------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).

validator_bad_type_test_() ->
    Spec = [{<<"value1">>, mandatory, integer},
            {<<"value2">>, optional, boolean}],
    Fields = [{<<"value1">>, 3}, {<<"value2">>, 5}],
    ?_assertThrow({badtype_parameter, <<"value2">>}, validate(Spec, Fields)).

validator_missing_parameter_test_() ->
    Spec = [{<<"value1">>, mandatory, integer},
            {<<"value2">>, mandatory, string},
            {<<"value3">>, optional, boolean}],
    Fields = [{<<"value2">>, "string"}],
    ?_assertThrow({missing_parameter, <<"value1">>}, validate(Spec, Fields)).

validator_unknown_parameter_test_() ->
    Spec = [{<<"value1">>, mandatory, binary},
            {<<"value2">>, mandatory, boolean}],
    Fields = [{<<"value1">>, <<"binary">>},
              {<<"value2">>, true},
              {<<"value3">>, 4}],
    ?_assertThrow({unknown_parameter, <<"value3">>}, validate(Spec, Fields)).

validator_duplicated_parameter_test_() ->
    Spec = [{<<"value1">>, mandatory, binary},
            {<<"value2">>, mandatory, boolean}],
    Fields = [{<<"value1">>, <<"binary">>},
              {<<"value2">>, true},
              {<<"value2">>, false}],
    ?_assertThrow({duplicated_parameters,
                   [<<"value1">>, <<"value2">>, <<"value2">>]},
                  validate(Spec, Fields)).

validator_valid_input_test_() ->
    Spec = [{<<"value1">>, mandatory, binary},
            {<<"value2">>, mandatory, boolean},
            {<<"value3">>, optional, string},
            {<<"value4">>, mandatory, integer},
            {<<"value5">>, optional, float},
            {<<"value6">>, optional, integer}],
    Fields = [{<<"value1">>, <<"binary">>},
              {<<"value2">>, true},
              {<<"value3">>, "string"},
              {<<"value4">>, 4},
              {<<"value5">>, 4.9}],
    ?_assertMatch(Fields, validate(Spec, Fields)).


validator_valid_input_nested_test_() ->
    Spec = [{<<"value1">>, mandatory, binary},
            {<<"value2">>, optional, string},
            {<<"value3">>, mandatory,
             {json, [{<<"value2-1">>, mandatory, binary},
                     {<<"value2-2">>, mandatory, boolean}]}
            }],
    Fields = [{<<"value1">>, <<"binary">>},
              {<<"value3">>,
               [{<<"value2-1">>, <<"binary">>},
                {<<"value2-2">>, false}]
              }],
    ?_assertMatch(Fields, validate(Spec, Fields)).

validator_not_valid_input_nested_test_() ->
    Spec = [{<<"value1">>, mandatory, binary},
            {<<"value2">>, mandatory,
             {json, [{<<"value2-1">>, mandatory, float},
                     {<<"value2-2">>, mandatory,
                      {json, [{<<"value3-1">>, mandatory, string},
                             {<<"value3-2">>, optional, integer}]}
                     }]}
            }],
    Fields = [{<<"value1">>, <<"binary">>},
              {<<"value2">>,
               [{<<"value2-1">>, 3.6},
                {<<"value2-2">>, [{<<"value3-1">>, "string"},
                                  {<<"value3-2">>, 5.4}]}]
              }],
    ?_assertThrow({badtype_parameter, <<"value3-2">>}, validate(Spec, Fields)).

validator_edge_case_1_test_() ->
    Spec = [{<<"value1">>, optional, binary},
            {<<"value2">>, optional, boolean}],
    Fields = [],
    ?_assertMatch(Fields, validate(Spec, Fields)).

validator_edge_case_2_test_() ->
    Spec = [{<<"value1">>, optional, binary},
            {<<"value2">>, mandatory, boolean}],
    Fields = [],
    ?_assertThrow({missing_parameter, <<"value2">>}, validate(Spec, Fields)).

validator_edge_case_3_test_() ->
    Spec = [{<<"value1">>, optional, binary},
            {<<"value2">>, mandatory, boolean}],
    Fields = [{}],
    ?_assertThrow({missing_parameter, <<"value2">>}, validate(Spec, Fields)).

validator_edge_case_4_test_() ->
    Spec = [{<<"value1">>, optional, binary}],
    Fields = [{}],
    ?_assertThrow({unknown_parameter, {}}, validate(Spec, Fields)).

%We are not accepting lists of JSONs, just plain objects.
validator_list_of_jsons_test_() ->
    Spec = [{<<"value1">>, optional, binary}],
    Fields = [[{}],[{}],[{<<"spain">>,<<"lost">>}]],
    ?_assertThrow({unexpected_json, Fields}, validate(Spec, Fields)).

-endif.
