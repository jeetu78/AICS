%% @author M1020387
%% @doc @todo Add description to ea_aics_service.


-module(ea_aics_service).
-include_lib("mysql/include/mysql.hrl").
-include_lib("ea_aics_store.hrl").
-export([getAncillariesForFlight/1,getAncillaryAvailableQuantity/1,bookAncillaries/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%%------------------------------------------------------------------------------
%% @doc Reads the available ancillaries for a Flight.
%% 
%% @end
%%------------------------------------------------------------------------------

getAncillariesForFlight(FlightUUID)->
	if 
		FlightUUID /= [] ->
			{ok,Records}=do_read({'ANCILLARY_MASTER',[],[{'FLIGHT_UUID',FlightUUID}]}),
			createInputForJsonEncode(Records);
		true->
			[{<<"error">>,[[{<<"couldnt read">>,<<"from ancillaries">>}]]}]
	end.

%%------------------------------------------------------------------------------
%% @doc Books ancillary/ancillaries if available against a Customer
%%
%% @end
%%------------------------------------------------------------------------------

bookAncillaries(JsonStructure)->
	TxIDs=bookMultipleAncillaries(parse_json_structure_to_records(JsonStructure)),
	create_tx_id_json(TxIDs).
	


%% ====================================================================
%% Internal functions
%% ====================================================================

%% getAncillaries()->
%% 	ea_aics_store:do_query(
%% 			  fun(DBConfig) ->
%% 					  do_read(DBConfig, {'ANCILLARY_MASTER',undefined,undefined})
%% 			  end).

%%------------------------------------------------------------------------------
%% @doc Reads the available quantity of an ancillary.
%%
%% @end
%%------------------------------------------------------------------------------

getAncillaryAvailableQuantity(AncillaryInventoryUUID) ->
	if 
		AncillaryInventoryUUID /= [] ->
			do_read({'ANCILLARY_INVENTORY',['AVAILABLE_QUANTITY'],[{'UUID',AncillaryInventoryUUID}]});
		true->
			{error,invalid_input_get_available_quantity}
	end.


%%------------------------------------------------------------------------------
%% @doc Books multiple ancillaries if available against a Customer
%%
%% @end
%%------------------------------------------------------------------------------

bookMultipleAncillaries(BookAncillariesInput)->
	if 
		BookAncillariesInput /=[] ->
			lists:map(fun(#ancillary_booking_input{ancillary_inventory_uuid=AncillaryInventoryUUId,customer_uuid=CustomerUUId,quantity=Quantity}) ->
								  bookAncillary(AncillaryInventoryUUId,CustomerUUId,Quantity)
						  end,
						  BookAncillariesInput);
		true ->
			{error,invalid_input_for_booking_ancillaries}
	end.

%%------------------------------------------------------------------------------
%% @doc Books an ancillary if available against a Customer
%%
%% @end
%%------------------------------------------------------------------------------

bookAncillary(AncillaryInventoryUUId,CustomerUUId,Quantity) ->
	if 
		((AncillaryInventoryUUId /= []) and (CustomerUUId /= []) and (Quantity >= 0)) ->
			{ok,AvailabaleQuantity}=getAncillaryAvailableQuantity(AncillaryInventoryUUId),
			if
				AvailabaleQuantity>=Quantity ->
					UUID=ea_aics_store:generate_uuid(),
					{ok,NumberOfRows}=do_update({'ANCILLARY_INVENTORY',[{'AVAILABLE_QUANTITY',Quantity}],[{'UUID',AncillaryInventoryUUId}]}),
					io:format("~p",[NumberOfRows]),
					do_insert({'ANCILLARY_TX',[{'UUID',UUID},{'ANC_INVENTORY_UUID',AncillaryInventoryUUId},{'CUSTOMER_UUID',CustomerUUId},{'QUANTITY',Quantity}]});
				true->
					{error,not_enough_ancillaries}
			end;
		true->
			{error,invalid_set_of_argumanets}
	end.

%% ====================================================================
%% Database functions
%% ====================================================================

%%------------------------------------------------------------------------------
%% @doc Updates a database table using the connection.
%%
%% @end
%%------------------------------------------------------------------------------

do_update({Table,UpdateParameters,ClauseParameters})->
	case {Table,UpdateParameters,ClauseParameters} of
		{'ANCILLARY_INVENTORY',[{'AVAILABLE_QUANTITY',Quantity}],[{'UUID',AncillaryInventoryUUId}]} ->
			Query="UPDATE ANCILLARY_INVENTORY SET AVAILABLE_QUANTITY=AVAILABLE_QUANTITY-" ++ integer_to_list(Quantity) ++" WHERE UUID='"++ AncillaryInventoryUUId ++ "'",
			QueryResult=connectionServer:execute(Query),
			#mysql_result{affectedrows=NumberOfRows} = QueryResult,
			%%io:format("\n~p\n",	[QueryResult]),
			{ok,NumberOfRows};
		_->
			{error,no_do_update_match}
	end.

%%------------------------------------------------------------------------------
%% @doc Inserts a record in database table.
%%
%% @end
%%------------------------------------------------------------------------------

do_insert({Table,Parameters})->
	case {Table,Parameters} of
		{'ANCILLARY_TX',[{'UUID',UUID},{'ANC_INVENTORY_UUID',AncillaryInventoryUUId},{'CUSTOMER_UUID',CustomerUUId},{'QUANTITY',Quantity}]} ->
			Query="INSERT INTO ANCILLARY_TX (UUID,ANC_INVENTORY_UUID,CUSTOMER_UUID,QUANTITY) VALUES ('"++ UUID ++"','"++ AncillaryInventoryUUId ++"','"++ CustomerUUId ++"',"++ integer_to_list(Quantity) ++")",
			QueryResult=connectionServer:execute(Query),
			#mysql_result{affectedrows=Rows} = QueryResult,
			io:format("\n~p\n",	[Rows]),
			UUID;
		_->
			{error,no_do_insert_match}
	end.

%%------------------------------------------------------------------------------
%% @doc Reads the records from database table.
%%
%% @end
%%------------------------------------------------------------------------------

do_read({Table,Fields,Parameters}) ->
	case {Table,Fields,Parameters} of
		{'ANCILLARY_MASTER',[],[{'FLIGHT_UUID',FlightUUID}]} ->
			Query="SELECT AI.UUID,AI.FLIGHT_UUID,AI.AVAILABLE_QUANTITY,AM.UUID,AM.ANC_MASTER_CODE,AM.GROUP_CODE,AM.SUB_GROUP,AM.COMMERCIAL_NAME,AM.DESCRIPTION1,AM.DESCRIPTION2,AM.WEB_DESCRIPTION,AM.IMAGE_THUBNAIL_URL,AM.IMAGE_LARGE_URL,AM.IMAGE_TOOL_TIP,AM.CURRENCY,AM.PRICE,AM.TAX,AM.IS_DISCOUNT,AM.DISCOUNT_DESC,AM.DISCOUNT_PCNT FROM ANCILLARY_MASTER AM INNER JOIN (SELECT UUID,FLIGHT_UUID,AVAILABLE_QUANTITY,ANC_MASTER_UUID FROM ANCILLARY_INVENTORY WHERE FLIGHT_UUID='"++ FlightUUID ++"')	AI ON AM.UUID=AI.ANC_MASTER_UUID",
			QueryResult=connectionServer:execute(Query),
			#mysql_result{rows = QueryResultRows}=QueryResult,
		    {ok, parse_rows_into_records(ancillary,QueryResultRows)};
		{'ANCILLARY_INVENTORY',['AVAILABLE_QUANTITY'],[{'UUID',AncillaryInventoryUUID}]} ->
			Query="SELECT AVAILABLE_QUANTITY FROM ANCILLARY_INVENTORY WHERE UUID='"++ AncillaryInventoryUUID ++"'",
			QueryResult=connectionServer:execute(Query),
			#mysql_result{rows = [QueryResultRows]}=QueryResult,
			%%io:format("\n~p\n",QueryResultRows),
			{ok,hd(QueryResultRows)};
%% 		{'ANCILLARY_MASTER',['*'],[undefined]} ->
%% 			Query="SELECT AM.UUID,AM.ANC_MASTER_CODE,AM.GROUP_CODE,AM.SUB_GROUP,AM.COMMERCIAL_NAME,AM.DESCRIPTION1,AM.DESCRIPTION2,AM.IMAGE_THUBNAIL_URL,AM.IMAGE_LARGE_URL,AM.IMAGE_TOOL_TIP,AM.CURRENCY,AM.PRICE,AM.TAX,AM.IS_DISCOUNT,AM.DISCOUNT_DESC,AM.DISCOUNT_PCNT FROM ANCILLARY_MASTER AM",
%% 			{data, QueryResult}=mysql:fetch(connPool,Query),
%% 			QueryResult=connectionServer:execute(Query),
%% 			%%io:format("\n~p\n",	[QueryResultRows]),
%% 		    {ok, QueryResultRows};
		_->
			{ok,"error"}
	end.


parse_rows_into_records(ancillary,Rows)->
	[#ancillary_info{anc_inv_uuid=AncInvUUID,flight_uuid=FlightUUID,available_quantity=AvailableQuantity,anc_uuid=AncUUID,code=Code, group=Group, subGroup=SubGroup,commercialName=CommercialName,description1=Description1, description2=Description2, webDescription=WebDescription, imageThumbnailURL=ImageThumbnailURL, imageLargeURL=ImageLargeURL, imageToolTip=ImageToolTip, currency=Currency, basePrice=Price, tax=Tax, isDiscount=IsDiscount, discDescription=DiscDescription, discPercentage=DiscPercentage}
    || [AncInvUUID,FlightUUID,AvailableQuantity,AncUUID,Code, Group, SubGroup,CommercialName, Description1,WebDescription,Description2, ImageThumbnailURL, ImageLargeURL, ImageToolTip, Currency, Price, Tax, IsDiscount, DiscDescription, DiscPercentage] <- Rows].

createInputForJsonEncode(Records)->
	[{<<"ancillaries">>,[createListofFieldsFromRecord(Record)||Record<-Records]}].
        
createListofFieldsFromRecord(Record) ->
	[
	{<<"anc_inv_uuid">>,Record#ancillary_info.anc_inv_uuid},
	{<<"flight_uuid">>,Record#ancillary_info.flight_uuid},
	{<<"available_quantity">>,Record#ancillary_info.available_quantity},
 	{<<"anc_uuid">>,Record#ancillary_info.anc_uuid},
 	{<<"code">>,Record#ancillary_info.code},
 	{<<"group">>,Record#ancillary_info.group},
 	{<<"subGroup">>,Record#ancillary_info.subGroup},
 	{<<"commercialName">>,Record#ancillary_info.commercialName},
 	{<<"description1">>,Record#ancillary_info.description1},
 	{<<"description2">>,Record#ancillary_info.description2},
	{<<"web_description">>,Record#ancillary_info.webDescription},
 	{<<"imageThumbnailURL">>,Record#ancillary_info.imageThumbnailURL},
 	{<<"imageLargeURL">>,Record#ancillary_info.imageLargeURL},
 	{<<"imageToolTip">>,Record#ancillary_info.imageToolTip},
 	{<<"currency">>,Record#ancillary_info.currency},
 	{<<"basePrice">>,Record#ancillary_info.basePrice},
 	{<<"tax">>,Record#ancillary_info.tax},
 	{<<"isDiscount">>,Record#ancillary_info.isDiscount},
 	{<<"discDescription">>,Record#ancillary_info.discDescription},
 	{<<"discPercentage">>,Record#ancillary_info.discPercentage}
 	].

parse_json_structure_to_records(JsonStructure)->
	[{<<"data">>,List}]=JsonStructure,
	[create_book_ancillary_record(Element)||Element<-List].

create_book_ancillary_record(Element)->
	[{<<"anc_inv_uuid">>,AncInvUUID},{<<"customer_uuid">>,CustomerUUID},{<<"quantity">>,Quantity}]=Element,
	#ancillary_booking_input{ancillary_inventory_uuid=binary_to_list(AncInvUUID),customer_uuid=binary_to_list(CustomerUUID),quantity=Quantity}.

create_tx_id_json(TxIDs)->
	[{<<"Transaction IDs">>,[[{<<"TxID">>,TxID}]||TxID<-TxIDs]}].
