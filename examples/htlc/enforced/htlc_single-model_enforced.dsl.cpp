// -*-text-*-

#include "../htlc_single-model.dsl"
#include "../tx_single-spec.dsl"

// unsatifiable

property
[]({state=0} -> tx_done);
