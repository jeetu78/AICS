-record(ea_aics_ancillary, {id  :: binary()}).

-record(ea_aics_allocated_ancillary, {id        :: binary(),
                                      ancillary :: #ea_aics_ancillary{}}).

-record(ea_aics_ancillary_booking, {id  :: binary(),
                                    allocated_ancillary :: #ea_aics_allocated_ancillary{}}).