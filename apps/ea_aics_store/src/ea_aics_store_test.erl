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
         allocated_ancillary_input/0,
         allocated_ancillary_row/3,
         allocated_ancillary_rows/1,
         ancillary_booking_input/0,
         ancillary_booking_row/4,
         ancillary_booking_rows/1]).

ancillary_input() ->
    [_AncillaryMasterCode = <<"foo">>,
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

ancillary_row(AncillaryId) ->
    [AncillaryId | ancillary_input()].

ancillary_rows(AncillaryIds) ->
    [ancillary_row(AncillaryId) || AncillaryId <- AncillaryIds].

allocated_ancillary_input() ->
    [_AllocatedAncillaryInventoryId = <<"foo">>,
     _AllocatedAncillaryAllocatedQuantity = 1,
     _AllocatedAncillaryAvailableQuantity = 1,
     _AllocatedAncillaryModifiedTime = <<"foo">>].

allocated_ancillary_row(AllocatedAncillaryId, FlightId, AncillaryId) ->
    AncillaryRow = ancillary_row(AncillaryId),
    lists:append([AllocatedAncillaryId, AncillaryId, FlightId |
        allocated_ancillary_input()], AncillaryRow).

allocated_ancillary_rows(AllocatedAncillaryIds) ->
    [allocated_ancillary_row(AllocatedAncillaryId, FlightId, AncillaryId) ||
        {AllocatedAncillaryId, FlightId, AncillaryId} <- AllocatedAncillaryIds].

ancillary_booking_input() ->
    [_AncillaryBookingCustomerId = <<"foo">>,
     _AncillaryBookingTransactionId = <<"foo">>,
     _AncillaryBookingOperationType = <<"foo">>,
     _AncillaryBookingBookingTime = <<"foo">>,
     _AncillaryBookingQuantity = 1,
     _AncillaryBookingModifiedTime = <<"foo">>].

ancillary_booking_row(AncillaryBookingId, FlightId, AllocatedAncillaryId, AncillaryId) ->
    AllocatedAncillaryRow = allocated_ancillary_row(AllocatedAncillaryId,
        FlightId, AncillaryId),
    lists:append([AncillaryBookingId, AllocatedAncillaryId |
        ancillary_booking_input()], AllocatedAncillaryRow).

ancillary_booking_rows(AncillaryBookingIds) ->
    [ancillary_booking_row(AncillaryBookingId, FlightId, AllocatedAncillaryId, AncillaryId) ||
        {AncillaryBookingId, FlightId, AllocatedAncillaryId, AncillaryId} <- AncillaryBookingIds].

-endif.
