%% @author M1020387
%% @doc @todo Add description to ea_aics_service.


-module(ea_aics_service).
-include_lib("mysql/include/mysql.hrl").
-include_lib("ea_aics_store.hrl").
-export([getAncillariesForFlight/1,getAncillaryAvailableQuantity/1,bookAncillaries/1,createInputForJsonEncode/2]).

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
			case do_read({'ANCILLARY_MASTER',[],[{'FLIGHT_UUID',FlightUUID}]}) of
				{ok,Records} ->
					createInputForJsonEncode(Records);
				{error,ErrorMsg}->
					createInputForJsonEncode(error,ErrorMsg)
			end;
		true->
			createInputForJsonEncode(error,"Flight UUID missing.")
	end.

%%------------------------------------------------------------------------------
%% @doc Books ancillary/ancillaries if available against a Customer
%%
%% @end
%%------------------------------------------------------------------------------

bookAncillaries(JsonStructure)->
	TxOutput=bookMultipleAncillaries(parse_json_structure_to_records(JsonStructure)),
	create_tx_json(TxOutput).



%% ====================================================================
%% Internal functions
%% ====================================================================

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
			case getAncillaryAvailableQuantity(AncillaryInventoryUUId) of
				{ok,[[AvailabaleQuantity]]}->
					if
						AvailabaleQuantity>=Quantity ->
							UUID=ea_aics_store:generate_uuid(),
							
							case do_update({'ANCILLARY_INVENTORY',[{'AVAILABLE_QUANTITY',Quantity}],[{'UUID',AncillaryInventoryUUId}]}) of
								{ok,NumberOfRows}->
									if 
										NumberOfRows>0 ->
											do_insert({'ANCILLARY_TX',[{'UUID',UUID},{'ANC_INVENTORY_UUID',AncillaryInventoryUUId},{'CUSTOMER_UUID',CustomerUUId},{'QUANTITY',Quantity}]});
										true ->
											{error,"Couldn't update Ancillary Inventory"}
									end;
								{error,ErrorMsg}->
									{error,ErrorMsg}
							end;
						
						
						true->
							{unavailable_quantity,{AvailabaleQuantity,AncillaryInventoryUUId,CustomerUUId}}
					end;
				{error,ErrorMsg}->
					{error,ErrorMsg}
			end;
		true->
			{error,"bookAncillary one or more invalid argument"}
	end.

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
			{error,"getAncillaryAvailableQuantity invalid argument"}
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
			case connectionServer:execute(Query) of
				{no_of_rows,NumberOfRows}->
					if
						NumberOfRows>0 ->
							{ok,NumberOfRows};
						true->
							{error,"Ancillary Inventory Update Failed"}
					end;
				{error,Error}->
					{error,Error}
			end;
		_->
			{error,"do-update case clause mismatch"}
	end.

%%------------------------------------------------------------------------------
%% @doc Inserts a record in database table.
%%
%% @end
%%------------------------------------------------------------------------------

do_insert({Table,Parameters})->
	case {Table,Parameters} of
		{'ANCILLARY_TX',[{'UUID',UUID},{'ANC_INVENTORY_UUID',AncillaryInventoryUUId},{'CUSTOMER_UUID',CustomerUUId},{'QUANTITY',Quantity}]} ->
			Query="INSERT INTO ANCILLARY_TX (UUID,ANC_INVENTORY_UUID,CUSTOMER_UUID,QUANTITY) VALUES ('"++ binary_to_list(UUID) ++"','"++ AncillaryInventoryUUId ++"','"++ CustomerUUId ++"',"++ integer_to_list(Quantity) ++")",
			case connectionServer:execute(Query) of
				{no_of_rows,NumberOfRows}->
					if
						NumberOfRows>0->
							{ok,{UUID,AncillaryInventoryUUId,CustomerUUId}};
						true->
							{error,{"create anc transaction failed",{AncillaryInventoryUUId,CustomerUUId}}}
					end;
				{error,Error}->
					{error,{Error,{AncillaryInventoryUUId,CustomerUUId}}}
			end;
		_->
			{error,"do-insert case clause mismatch"}
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
			case connectionServer:execute(Query) of
				{data,QueryResultRows}->
					{ok, parse_rows_into_records(ancillary,QueryResultRows)};
				{error,Error}->
					{error,Error}
			end;
		{'ANCILLARY_INVENTORY',['AVAILABLE_QUANTITY'],[{'UUID',AncillaryInventoryUUID}]} ->
			Query="SELECT AVAILABLE_QUANTITY FROM ANCILLARY_INVENTORY WHERE UUID='"++ AncillaryInventoryUUID ++"'",
			case connectionServer:execute(Query) of
				{data,QueryResultRows}->
					{ok,QueryResultRows};
				{error,Error}->
					{error,Error}
			end;
		_->
			{error,"do-read case clause mismatch"}
	end.

%% ====================================================================
%% Parser functions
%% ====================================================================

%%------------------------------------------------------------------------------
%% @doc converts ancillaries data returned from Database into list of ancillary_info records.
%%
%% @end
%%------------------------------------------------------------------------------
parse_rows_into_records(ancillary,Rows)->
	[#ancillary_info{anc_inv_uuid=AncInvUUID,flight_uuid=FlightUUID,available_quantity=AvailableQuantity,anc_uuid=AncUUID,code=Code, group=Group, subGroup=SubGroup,commercialName=CommercialName,description1=Description1, description2=Description2, webDescription=WebDescription, imageThumbnailURL=ImageThumbnailURL, imageLargeURL=ImageLargeURL, imageToolTip=ImageToolTip, currency=Currency, basePrice=Price, tax=Tax, isDiscount=IsDiscount, discDescription=DiscDescription, discPercentage=DiscPercentage}
					|| [AncInvUUID,FlightUUID,AvailableQuantity,AncUUID,Code, Group, SubGroup,CommercialName, Description1,WebDescription,Description2, ImageThumbnailURL, ImageLargeURL, ImageToolTip, Currency, Price, Tax, IsDiscount, DiscDescription, DiscPercentage] <- Rows].


%%--------------------------------------------------------------------------------------------------
%% @doc Takes list of ancillary_info records and converts it into the Json structure
%%
%% @end
%%----------------------------------------------------------------------------------------------
createInputForJsonEncode(Records)->
	if
		Records /= [] ->
			[{<<"ancillaries">>,[createListofFieldsFromRecord(Record)||Record<-Records]}];
		true ->
			[{<<"ancillaries">>,[[{<<"Empty">>,<<"Empty">>}]]}]
	end.

%%--------------------------------------------------------------------------------------------------
%% @doc Takes error message and converts it into the Json structure
%%
%% @end
%%----------------------------------------------------------------------------------------------
createInputForJsonEncode(error,ErrorMsg)->
	[{<<"error">>,[[{<<"error_msg">>,list_to_binary(ErrorMsg)}]]}].

%%--------------------------------------------------------------------------------------------------
%% @doc Takes ancillary_info record and converts it into a list of tuples.
%%
%% @end
%%----------------------------------------------------------------------------------------------
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

%%--------------------------------------------------------------------------------------------------
%% @doc Takes the data in Json structure and converts it into list of ancillary_booking_input records
%%
%% @end
%%----------------------------------------------------------------------------------------------
parse_json_structure_to_records(JsonStructure)->
	[{<<"data">>,List}]=JsonStructure,
	[create_book_ancillary_record(Element)||Element<-List].

%%--------------------------------------------------------------------------------------------------
%% @doc Takes an object in Json structure and converts it into ancillary_booking_input record
%%
%% @end
%%----------------------------------------------------------------------------------------------
create_book_ancillary_record(Element)->
	[{<<"anc_inv_uuid">>,AncInvUUID},{<<"customer_uuid">>,CustomerUUID},{<<"quantity">>,Quantity}]=Element,
	#ancillary_booking_input{ancillary_inventory_uuid=binary_to_list(AncInvUUID),customer_uuid=binary_to_list(CustomerUUID),quantity=Quantity}.

%%--------------------------------------------------------------------------------------------------
%% @doc Takes a list of transaction outputs and covnerts it into Json strusture.
%%
%% @end
%%----------------------------------------------------------------------------------------------
create_tx_json(TxOutput)->
	[{<<"Ancillary Transactions">>,[
									case TxEntry of
										{ok,{UUID,AncillaryInventoryUUId,CustomerUUId}} ->
											[{<<"booking_status">>,<<"complete">>},{<<"transaction_id">>,UUID},{<<"anc_inv_uuid">>,list_to_binary(AncillaryInventoryUUId)},{<<"customer_uuid">>,list_to_binary(CustomerUUId)}];
										{unavailable_quantity,{AvailabaleQuantity,AncillaryInventoryUUId,CustomerUUId}} ->
											[{<<"booking_status">>,<<"incomplete">>},{<<"available_quantity">>,AvailabaleQuantity},{<<"anc_inv_uuid">>,list_to_binary(AncillaryInventoryUUId)},{<<"customer_uuid">>,list_to_binary(CustomerUUId)}];
										{error,{Error,{AncillaryInventoryUUId,CustomerUUId}}}->
											[{<<"error">>,list_to_binary(Error)},{<<"anc_inv_uuid">>,list_to_binary(AncillaryInventoryUUId)},{<<"customer_uuid">>,list_to_binary(CustomerUUId)}];
										{error,ErrorMsg} ->
											[{<<"error">>,list_to_binary(ErrorMsg)}]
									end
									||TxEntry<-TxOutput]}].

