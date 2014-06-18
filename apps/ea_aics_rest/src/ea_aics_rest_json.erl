%%%=============================================================================
%%% @author Ramon Lastres Guerrero <ramon.lastres@erlang-solutions.com>
%%% @doc AICS rest JSON validation and parsing functions. For the resources
%%% that use JSON as an input, it generates the internal records with the needed
%%% fields.
%%% @end
%%%=============================================================================
-module(ea_aics_rest_json).

-export([parse/2,
        validation_spec/1]).

%%%----------------------------------------------------------------------------
%%% Types
%%%----------------------------------------------------------------------------
-type resource_type() :: 'ancillary' | 'ancillary_booking' |
    'allocated_ancillary'.

-type validation_spec() :: nonempty_list({binary(), mandatory | optional,
                                          field_type()}).

-type field_type() :: 'integer' | 'boolean' | 'binary' |
    'natural' | 'string' | 'float' | resource_type().

-type external_parameters() :: nonempty_list({binary(), any()}).

-type internal_parameters() :: nonempty_list({record_field_name(),
                                              record_field_value()}).

-type record_field_name() :: binary().

-type record_field_value() :: integer() | boolean() | binary() |
    string() | float().

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
-spec parse(validation_spec(), external_parameters()) -> internal_parameters().
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
%%% @doc It returns the validation spec for each resource defined. This might
%%% be removed and each resource should be exporting its own validation_spec
%%% function in its own module instead.
%%% @end
%%%----------------------------------------------------------------------------
-spec validation_spec(resource_type()) -> validation_spec().
validation_spec(ancillary) ->
    [{<<"masterCode">>, mandatory, integer},
     {<<"serviceProviderId">>, mandatory, string},
     {<<"subCode">>, mandatory, string},
     {<<"groupCode">>, mandatory, string},
     {<<"subGroup">>, mandatory, string},
     {<<"description1">>, mandatory, string},
     {<<"description2">>, mandatory, string},
     {<<"imageThumbnailUrl">>, mandatory, string},
     {<<"toolTip">>, mandatory, string},
     {<<"price">>, mandatory, float},
     {<<"currency">>, mandatory, string},
     {<<"tax">>, mandatory, float},
     {<<"isDiscount">>, mandatory, string},
     {<<"discountDesc">>, mandatory, string},
     {<<"iscountPcnt">>, mandatory, float},
     {<<"commercialName">>, mandatory, string},
     {<<"RFIC">>, mandatory, string}].

%%%----------------------------------------------------------------------------
%%% Internal functions.
%%%----------------------------------------------------------------------------
-spec validate(validation_spec(), external_parameters()) ->
    internal_parameters().
validate(ValidationSpec, Fields) ->
    %ValidationSpec = validation_spec(ResourceType),
    validate_mandatory(ValidationSpec, Fields),
    validate_unknown(ValidationSpec, Fields),
    validate_duplicated(ValidationSpec, Fields),
    validate_type(ValidationSpec, Fields).

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
type_validation(Name, _, _) ->
    throw({badtype_parameter, Name}).
