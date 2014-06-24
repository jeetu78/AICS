%% @author M1020387
%% @doc @todo Add description to ea_aics_service.


-module(ea_aics_service).
-include_lib("ea_aics_store.hrl").
-export([getAncillariesForFlight/1,bookAncillaries/1,createInputForJsonEncode/2]).

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
			case do_read({read_ancillaries,{flight_uuid,FlightUUID}}) of
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
			Update_Output=lists:map(fun(#ancillary_booking_input{ancillary_inventory_uuid=AncillaryInventoryUUId,customer_uuid=CustomerUUId,quantity=Quantity})->
											do_update({update_ancillary_inventory,[{quantity,Quantity},{anc_inventory_uuid,AncillaryInventoryUUId},{customer_uuid,CustomerUUId}]})
									end,BookAncillariesInput),
			Failed_Updates_List=lists:filter(fun(Element)->
													 case Element of
														 {ok,{1,_}}->
															 false;
														 {ok,{0,_}}->
															 true;
														 {error,_}->
															 true
													 end
											 end,
											 Update_Output),
			if
				length(Failed_Updates_List) == 0 ->
					UUID=binary_to_list(ea_aics_store:generate_uuid()),
					Bookings=lists:map(fun(#ancillary_booking_input{ancillary_inventory_uuid=AncillaryInventoryUUId,customer_uuid=CustomerUUId,quantity=Quantity}) ->
											   case do_insert({insert_ancillary_tx,[{uuid,UUID},{anc_inventory_uuid,AncillaryInventoryUUId},{customer_uuid,CustomerUUId},{quantity,Quantity}]}) of
												   {ok,{UUID,AncillaryInventoryUUId,CustomerUUId,Quantity}}->
													   #ancillary_transaction{status="ancillary booked",availability="available",quantity=Quantity,ancillary_inventory_uuid=AncillaryInventoryUUId,customer_uuid=CustomerUUId,error_msg="-"};
												   {error,{Error,{AncillaryInventoryUUId,CustomerUUId,Quantity}}}->
													   #ancillary_transaction{status="ancillary not booked",availability="available",quantity=Quantity,ancillary_inventory_uuid=AncillaryInventoryUUId,customer_uuid=CustomerUUId,error_msg=Error}
											   end
									   end,
									   BookAncillariesInput),
					case lists:keymember("ancillary booked", 2, Bookings) of
						true->
							{UUID,"successful",Bookings};
						false->
							{"-","Failed",Bookings}
					end;
				true->
					rollback(ancillary_inventory,{"-","Failed",Update_Output})
			end;
		true ->
			[#ancillary_transaction{error_msg="Invalid input to bookMultipleAncillaries",quantity=0}]
	end.


rollback(ancillary_inventory,{UUID,Tx_Status,Update_Output})->
	{UUID,Tx_Status,[case Entry of 
					  {ok,{1,{AncillaryInventoryUUId,CustomerUUId,Quantity}}}->
						  case do_update({rollback_ancillary_inventory,[{available_quantity,Quantity},{uuid,AncillaryInventoryUUId}]}) of
							  {ok,_}->
								  #ancillary_transaction{status="ancillary not booked",availability="available",quantity=Quantity,ancillary_inventory_uuid=AncillaryInventoryUUId,customer_uuid=CustomerUUId,error_msg="-"};
							  {error,Error}->
								  #ancillary_transaction{status="ancillary not booked",availability="available",quantity=Quantity,ancillary_inventory_uuid=AncillaryInventoryUUId,customer_uuid=CustomerUUId,error_msg=Error}
						  end;
					  {ok,{0,{AncillaryInventoryUUId,CustomerUUId,Quantity}}}->
						  #ancillary_transaction{status="ancillary not booked",availability="unavailable",quantity=Quantity,ancillary_inventory_uuid=AncillaryInventoryUUId,customer_uuid=CustomerUUId,error_msg="-"};
					  {error,{Error,{AncillaryInventoryUUId,CustomerUUId,Quantity}}}->
						  #ancillary_transaction{status="ancillary not booked",availability="available",quantity=Quantity,ancillary_inventory_uuid=AncillaryInventoryUUId,customer_uuid=CustomerUUId,error_msg=Error}
				  end
				  ||Entry<-Update_Output]}.
%%------------------------------------------------------------------------------
%% @doc Books an ancillary if available against a Customer
%%
%% @end
%%------------------------------------------------------------------------------

%% bookAncillary(UUID,AncillaryInventoryUUId,CustomerUUId,Quantity) ->
%% 	if 
%% 		((AncillaryInventoryUUId /= []) and (CustomerUUId /= []) and (Quantity >= 0)) ->
%% 			do_insert({'ANCILLARY_TX',[{'UUID',UUID},{'ANC_INVENTORY_UUID',AncillaryInventoryUUId},{'CUSTOMER_UUID',CustomerUUId},{'QUANTITY',Quantity}]});
%% 		true->
%% 			{error,"bookAncillary one or more invalid argument"}
%% 	end.

%%------------------------------------------------------------------------------
%% @doc Reads the available quantity of an ancillary.
%%
%% @end
%%------------------------------------------------------------------------------

%% getAncillaryAvailableQuantity(AncillaryInventoryUUID) ->
%% 	if 
%% 		AncillaryInventoryUUID /= [] ->
%% 			do_read({'ANCILLARY_INVENTORY',['AVAILABLE_QUANTITY'],[{'UUID',AncillaryInventoryUUID}]});
%% 		true->
%% 			{error,"getAncillaryAvailableQuantity invalid argument"}
%% 	end.

%% ====================================================================
%% Database functions
%% ====================================================================

%%------------------------------------------------------------------------------
%% @doc Updates a database table using the connection.
%%
%% @end
%%------------------------------------------------------------------------------

do_update(Update_Input)->
	case Update_Input of
		{update_ancillary_inventory,[{quantity,Quantity},{anc_inventory_uuid,AncillaryInventoryUUId},{customer_uuid,CustomerUUId}]} ->
			Quantity_List=integer_to_list(Quantity),
			Query="UPDATE ANCILLARY_INVENTORY SET AVAILABLE_QUANTITY=AVAILABLE_QUANTITY-" ++ Quantity_List ++" WHERE UUID='"++ AncillaryInventoryUUId ++ "' AND (AVAILABLE_QUANTITY-"++ Quantity_List ++")>=0",
			case connectionServer:execute(Query) of
				{no_of_rows,NumberOfRows}->
					{ok,{NumberOfRows,{AncillaryInventoryUUId,CustomerUUId,Quantity}}};
				{error,Error}->
					{error,{Error,{AncillaryInventoryUUId,CustomerUUId,Quantity}}}
			end;
		{rollback_ancillary_inventory,[{available_quantity,Quantity},{uuid,AncillaryInventoryUUId}]} ->
			Query="UPDATE ANCILLARY_INVENTORY SET AVAILABLE_QUANTITY=AVAILABLE_QUANTITY+" ++ integer_to_list(Quantity) ++" WHERE UUID='"++ AncillaryInventoryUUId ++ "'",
			case connectionServer:execute(Query) of
				{no_of_rows,Number_of_Rows}->
					{ok,Number_of_Rows};
				{error,Error}->
					{error,Error}
			end
	end.

%%------------------------------------------------------------------------------
%% @doc Inserts a record in database table.
%%
%% @end
%%------------------------------------------------------------------------------

do_insert(Insert_Input)->
	case Insert_Input of
		{insert_ancillary_tx,[{uuid,UUID},{anc_inventory_uuid,AncillaryInventoryUUId},{customer_uuid,CustomerUUId},{quantity,Quantity}]} ->
			Query="INSERT INTO ANCILLARY_TX (UUID,ANC_INVENTORY_UUID,CUSTOMER_UUID,QUANTITY) VALUES ('"++ UUID ++"','"++ AncillaryInventoryUUId ++"','"++ CustomerUUId ++"',"++ integer_to_list(Quantity) ++")",
			case connectionServer:execute(Query) of
				{no_of_rows,NumberOfRows}->
					if
						NumberOfRows>0->
							{ok,{UUID,AncillaryInventoryUUId,CustomerUUId,Quantity}};
						true->
							{error,{"create anc transaction failed",{AncillaryInventoryUUId,CustomerUUId,Quantity}}}
					end;
				{error,Error}->
					{error,{Error,{AncillaryInventoryUUId,CustomerUUId,Quantity}}}
			end
	end.

%%------------------------------------------------------------------------------
%% @doc Reads the records from database table.
%%
%% @end
%%------------------------------------------------------------------------------

do_read(Read_Input) ->
	case Read_Input of
		{read_ancillaries,{flight_uuid,FlightUUID}} ->
			Query="SELECT AI.UUID,AI.FLIGHT_UUID,AI.AVAILABLE_QUANTITY,AM.UUID,AM.ANC_MASTER_CODE,AM.GROUP_CODE,AM.SUB_GROUP,AM.COMMERCIAL_NAME,AM.DESCRIPTION1,AM.DESCRIPTION2,AM.WEB_DESCRIPTION,AM.IMAGE_THUBNAIL_URL,AM.IMAGE_LARGE_URL,AM.IMAGE_TOOL_TIP,AM.CURRENCY,AM.PRICE,AM.TAX,AM.IS_DISCOUNT,AM.DISCOUNT_DESC,AM.DISCOUNT_PCNT FROM ANCILLARY_MASTER AM INNER JOIN (SELECT UUID,FLIGHT_UUID,AVAILABLE_QUANTITY,ANC_MASTER_UUID FROM ANCILLARY_INVENTORY WHERE FLIGHT_UUID='"++ FlightUUID ++"')	AI ON AM.UUID=AI.ANC_MASTER_UUID",
			case connectionServer:execute(Query) of
				{data,QueryResultRows}->
					{ok, parse_rows_into_records(ancillary,QueryResultRows)};
				{error,Error}->
					{error,Error}
			end
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
create_tx_json({UUID,Tx_Status,TxRecords})->
	[{<<"transaction_id">>,list_to_binary(UUID)},{<<"transaction_status">>,list_to_binary(Tx_Status)},{<<"ancillary_bookings">>,[[{<<"status">>,list_to_binary(Status)},{<<"availability">>,list_to_binary(Availibility)},{<<"quantity">>,Quantity},{<<"ancillary_inventory_uuid">>,list_to_binary(AncillaryInventoryUUId)},{<<"customer_uuid">>,list_to_binary(CustomerUUId)},{<<"error_msg">>,list_to_binary(Error_Msg)}]
																											  || #ancillary_transaction{status=Status,availability=Availibility,quantity=Quantity,ancillary_inventory_uuid=AncillaryInventoryUUId,customer_uuid=CustomerUUId,error_msg=Error_Msg}<-TxRecords]}].

