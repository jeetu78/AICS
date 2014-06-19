%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System
%%% ancillaries store interface test helpers
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_store_test).

-ifdef(TEST).

-export([ancillary_input/0,
         ancillary_row/1,
         ancillary_rows/1,
         allocated_ancillary_input/2,
         allocated_ancillary_row/3,
         allocated_ancillary_rows/1,
         ancillary_booking_input/1,
         ancillary_booking_row/4,
         ancillary_booking_rows/1]).

-include_lib("ea_aics_core/include/ea_aics_core.hrl").

ancillary_input() ->
    #ea_aics_ancillary{master_code = 111,
                       service_provider_id = <<"foo">>,
                       sub_code = <<"foo">>,
                       group_code = <<"foo">>,
                       sub_group = <<"foo">>,
                       description1 = <<"foo">>,
                       description2 = <<"foo">>,
                       image_thumbnail_url = <<"foo">>,
                       image_large_url = <<"foo">>,
                       tooltip = <<"foo">>,
                       price = 1.0,
                       currency = <<"foo">>,
                       tax = 1.0,
                       is_discount = <<"foo">>,
                       discount_desc = <<"foo">>,
                       discount_pcnt = 1.0,
                       commercial_name = <<"foo">>,
                       rfic = <<"foo">>}.

ancillary_row(AncillaryId) ->
    [AncillaryId,
     _AncillaryMasterCode = 111,
     _AncillaryServiceProviderId = <<"foo">>,
     _AncillarySubCode = <<"foo">>,
     _AncillaryGroupCode = <<"foo">>,
     _AncillarySubGroup = <<"foo">>,
     _AncillaryDescription1 = <<"foo">>,
     _AncillaryDescription2 = <<"foo">>,
     _AncillaryImageThumbnailUrl = <<"foo">>,
     _AncillaryImageLargeUrl = <<"foo">>,
     _AncillaryToolTip = <<"foo">>,
     _AncillaryPrice = 1.0,
     _AncillaryCurrency = <<"foo">>,
     _AncillaryTax = 1.0,
     _AncillaryIsDiscount = <<"foo">>,
     _AncillaryDiscountDesc = <<"foo">>,
     _AncillaryDiscountPcnt = 1.0,
     _AncillaryCommercialName = <<"foo">>,
     _AncillaryRFIC = <<"foo">>,
     _AncillaryModifiedTime = <<"foo">>].

ancillary_rows(AncillaryIds) ->
    [ancillary_row(AncillaryId) || AncillaryId <- AncillaryIds].

allocated_ancillary_input(FlightId, AncillaryId) ->
    #ea_aics_allocated_ancillary{inventory_id = <<"foo">>,
                                 allocated_quantity = 1,
                                 available_quantity = 1,
                                 flight = FlightId,
                                 ancillary = AncillaryId}.

allocated_ancillary_row(AllocatedAncillaryId, FlightId, AncillaryId) ->
    AllocatedAncillaryRow =
        [AllocatedAncillaryId,
         AncillaryId,
         FlightId,
         _AllocatedAncillaryInventoryId = <<"foo">>,
         _AllocatedAncillaryAllocatedQuantity = 1,
         _AllocatedAncillaryAvailableQuantity = 1,
         _AllocatedAncillaryModifiedTime = <<"foo">>],
    AncillaryRow = ancillary_row(AncillaryId),
    lists:append(AllocatedAncillaryRow, AncillaryRow).

allocated_ancillary_rows(AllocatedAncillaryIds) ->
    [allocated_ancillary_row(AllocatedAncillaryId, FlightId, AncillaryId) ||
        {AllocatedAncillaryId, FlightId, AncillaryId} <- AllocatedAncillaryIds].

ancillary_booking_input(AllocatedAncillaryId) ->
    #ea_aics_ancillary_booking{customer_id = <<"foo">>,
                               txn_id = 111,
                               operation_type = <<"foo">>,
                               booking_time = <<"foo">>,
                               quantity = 1,
                               allocated_ancillary = AllocatedAncillaryId}.

ancillary_booking_row(AncillaryBookingId, FlightId, AllocatedAncillaryId, AncillaryId) ->
    AncillaryBookingRow =
        [AncillaryBookingId,
         AncillaryId,
         _AncillaryBookingCustomerId = <<"foo">>,
         _AncillaryBookingTransactionId = 111,
         _AncillaryBookingOperationType = <<"foo">>,
         _AncillaryBookingBookingTime = <<"foo">>,
         _AncillaryBookingQuantity = 1,
         _AncillaryBookingModifiedTime = <<"foo">>],
    AllocatedAncillaryRow = allocated_ancillary_row(AllocatedAncillaryId,
        FlightId, AncillaryId),
    lists:append(AncillaryBookingRow, AllocatedAncillaryRow).

ancillary_booking_rows(AncillaryBookingIds) ->
    [ancillary_booking_row(AncillaryBookingId, FlightId, AllocatedAncillaryId, AncillaryId) ||
        {AncillaryBookingId, FlightId, AllocatedAncillaryId, AncillaryId} <- AncillaryBookingIds].

-endif.
