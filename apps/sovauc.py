#!/usr/bin/env python3

from decimal import *
from pprint import pformat

SUPPLY_UNITS="SOV"
BID_UNITS="USDC"

## 9 (1B) + 4 decimal places (%) + 6 decimal places (asset precision)
getcontext().prec = 9 + 4 + 6

# # note: may want to check that supply is divisble by supply_percent's denominator
# # and warn of modulus loss if not
# # or, we could just avoid making compile-time choices about runtime division
# # and use LSr_s, LAr_s whole
# def tranche_size_func(anchor, num_tranches, initial_tranches_size, supply, supply_percent, lookback):
#     supply_prop = supply_percent.quantize(Decimal('.01'), rounding=ROUND_DOWN)
#     print("supply_percent={}%".format(supply_prop))
#     LSr_s = lookback * supply * supply_prop
#     LAr_s = lookback * anchor * supply_prop
#     if LAr_s == 0:
#         if lookback == 0:
#             raise Exception("lookback cannot be zero")
#         if supply == 0:
#             raise Exception("supply cannot be zero")
#         if supply_prop == 0:
#             raise Exception("supply proportion cannot be zero")
#         raise Exception("LAr_s computed to be zero")
#     def var_tranche_size(window_total_raise, window_total_tranche_size):
#         return ((2 * window_total_raise * (LSr_s - lookback * window_total_tranche_size)) /
#                 (LAr_s + num_tranches * window_total_raise))
#     print("var_tranche_size=(2 * window_total_raise * ({} - {} * window_total_tranche_size)) / ({} + {} * window_total_raise)".format(LSr_s, lookback, LAr_s, num_tranches))
#     return var_tranche_size

NONEXIST_STATE=0
CREATED_STATE=1
READY_STATE=2
OPEN_STATE=3
PAYOUT_STATE=4

class AuctionContractSimulator:
    def __init__(self):
        self.state = NONEXIST_STATE
        self.opted_in = {}

    def create(self, admin, args):
        if self.state != NONEXIST_STATE:
            raise Exception("self.state={}; expected NONEXIST_STATE".format(self.state))
        self.state = CREATED_STATE
        self.admin = admin
        self.args = args
        self.app_id = 42
        return self.app_id

    def _assert_self_exists(self):
        if self.state == NONEXIST_STATE:
            raise Exception("self.state=NONEXIST_STATE")
        
    def setup_escrow(self, escrow):
        self._assert_self_exists()
        self.state = READY_STATE
        self.escrow = escrow

    def optin(self, sender):
        self._assert_self_exists()
        if sender in self.opted_in and self.opted_in[sender] == True:
            raise Exception("sender {} already opted in", sender)
        self.opted_in[sender] = True

class EscrowSimulator:
    pass

def call_application(sim, sender, args, app_id):
    if app_id == 0:
        app_id = sim.create(sender, args)
        print("{} <- call_application(sender={}, args={})".format(app_id, sender, args))
        return app_id
    else:
        if app_id != sim.app_id:
            raise Exception("call_application: wrong app id {}, expected {}".format(app_id, sim.app_id))
        if args["call"] == "escrow":
            sim.setup_escrow(args["escrow"])
        elif args["call"] == "optin":
            sim.optin(sender)
        else:
            raise Exception("unknown call {}".format(args["call"]))
        print("call_application(sender={}, app_id={}, args={})".format(sender, app_id, args))

class AuctionContext:
    def __init__(self,

                 auction_administrator,
                 
                 anchor=Decimal(180000000),
                 num_tranches=Decimal(78),
                 supply=Decimal(24000000),
                 supply_percent_hths=Decimal(4000),
                 initial_tranches_size=Decimal(180000000) * 25 / 10000,
                 lookback=4,
                 min_tranche_size=1,

                 auction_duration=Decimal(604800)):
        self.auction_administrator = admin
        self.anchor = anchor
        self.num_tranches = num_tranches,
        self.supply = supply
        self.supply_percent_hths = supply_percent_hths
        self.initial_tranches_size = initial_tranches_size,
        self.lookback = lookback
        self.min_tranche_size = min_tranche_size
    
def setup_escrow(sim, app_id, sender, escrow):
    call_application(sim, sender, {"call": "escrow", "escrow": escrow}, app_id)

# returns: auction ID
def init_auction_series(sim,

                        auction_administrator,

                        anchor,
                        num_tranches,
                        initial_tranches_size,
                        supply,
                        supply_percent_hths,
                        lookback,
                        min_tranche_size,

                        auction_duration):
    
    supply_prop = supply_percent_hths.quantize(Decimal('1'), rounding=ROUND_DOWN)
    # print("supply_percent_hths={}".format(supply_prop))
    LSr_s = lookback * supply * supply_prop
    LAr_s = lookback * anchor * supply_prop
    if LAr_s == 0:
        if lookback == 0:
            raise Exception("lookback cannot be zero")
        if supply == 0:
            raise Exception("supply cannot be zero")
        if supply_prop == 0:
            raise Exception("supply proportion cannot be zero")
        raise Exception("LAr_s computed to be zero")

    if LSr_s >= Decimal(2)**64:
        raise Exception("LSr_s {} >= 2^64".format(LSr_s))
    if LAr_s >= Decimal(2)**64:
        raise Exception("LAr_s {} >= 2^64".format(LAr_s))
    if lookback > 100:
        raise Exception("lookback {} > 100".format(lookback))
    if num_tranches > 1000:
        raise Exception("num_tranches {} > 1000".format(num_tranches))

    args = {
        "call": "create",
        "LSr_s": LSr_s * Decimal(1000000),
        "LAr_s": LAr_s * Decimal(10000),
        "lookback": lookback,
        "num_tranches": num_tranches,
        "initial_tranches_size": initial_tranches_size,
        "min_tranche_size": min_tranche_size,
        "auction_duration": auction_duration,
    }

    return call_application(sim, auction_administrator, pformat(args), 0)

def setup_series(admin):
    print('setup')
    app_id = init_auction_series(sim,

                                 auction_administrator=admin,
                                 
                                 anchor=Decimal(180000000),
                                 num_tranches=Decimal(78),
                                 supply=Decimal(24000000),
                                 supply_percent_hths=Decimal(4000),
                                 initial_tranches_size=Decimal(180000000) * 25 / 10000,
                                 lookback=4,
                                 min_tranche_size=1,

                                 auction_duration=Decimal(604800), # 1 week
    )

    setup_escrow(sim, app_id, sender="ADMIN", escrow="ESCROW")
    return app_id

def setup_bidder(sim, app_id, sender):
    print('setup bidder')
    call_application(sim, sender, {"call": "optin"}, app_id)

    asset_opt_in_txn = build_asset_transaction(sender=sender,
                                               token=SUPPLY_UNITS,
                                               amount=0,
    )
    call_transaction(asset_opt_in_txn)

def setup(sim):
    app_id = setup_series(admin="ADMIN")
    setup_bidder(sim, app_id, sender="ALICE")
    setup_bidder(sim, app_id, sender="BOB")
    setup_bidder(sim, app_id, sender="CHARLIE")
    return app_id

def run_auction(ctx, admin, fund_holder):
    deposit = build_asset_transaction(sender=fund_holder,
                                      receiver=ctx.escrow(),
                                      token=SUPPLY_UNITS,
                                      amount=ctx.current_tranche_size(),
    )
    auction_open = build_application_transaction(sender=admin,
                                                 args={"call": "open"},
    )
    call_group(ctx, deposit, auction_open)

def enter_bid(ctx, bidder, amount):
    bid_place = build_asset_transaction(sender=bidder,
                                        receiver=ctx.escrow(),
                                        token=BID_UNITS,
                                        amount=amount,
    )
    bid_receipt = build_application_transaction(sender=bidder,
                                                args={"call": "bid"},
    )
    call_group(ctx, bid_place, bid_receipt)

def discharge_bid(ctx, bidder):
    pass

def payout_bid(ctx, bidder):
    pass

def conclude_auction(ctx, admin, sink_account):
    pass

sim=AuctionContractSimulator()
setup(sim)
