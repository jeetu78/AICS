-record(ea_aics_ancillary, {id  :: binary(),
                            master_code :: binary(),
                            service_provider_id :: binary(),
                            sub_code :: binary(),
                            group_code :: binary(),
                            sub_group :: binary(),
                            description1 :: binary(),
                            description2 :: binary(),
                            image_thumbnail_url :: binary(),
                            image_large_url :: binary(),
                            tooltip :: binary(),
                            price :: float(),
                            currency :: binary(),
                            tax :: float(),
                            is_discount :: binary(),
                            discount_desc :: binary(),
                            discount_pcnt :: float(),
                            commercial_name :: binary(),
                            rfic :: binary(),
                            modified_time :: binary()}).

-record(ea_aics_flight, {id :: binary()}).

-record(ea_aics_allocated_ancillary, {id :: binary(),
                                      inventory_id :: binary(),
                                      allocated_quantity :: non_neg_integer(),
                                      available_quantity :: non_neg_integer(),
                                      modified_time :: binary(),
                                      flight    :: #ea_aics_flight{},
                                      ancillary :: #ea_aics_ancillary{}}).

-record(ea_aics_ancillary_booking, {id  :: binary(),
                                    txn_id :: integer(),
                                    customer_id :: binary(),
                                    operation_type :: binary(),
                                    booking_time :: binary(),
                                    quantity :: integer(),
                                    modified_time :: binary(),
                                    allocated_ancillary :: #ea_aics_allocated_ancillary{}}).