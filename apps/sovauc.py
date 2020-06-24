#!/usr/bin/env python3

from decimal import *
from pprint import pformat

SUPPLY_UNITS="SOV"
BID_UNITS="USDC"
ASSET_TXN="axfer"

weekly_params = {
    "anchor": Decimal(180000000 * 100),
    "num_tranches": 78,
    "supply": Decimal(24000000),
    "supply_percent_hths": Decimal(4000),
    "init_tranches_size": Decimal(24000000) * 25,
    "lookback": 4,
    "min_tranche_size": 1,
    "auction_duration": Decimal(604800),
}

indent = 0
def trace(fn):
    def inner(*args):
        global indent
        indent += 1
        try:
            res = fn(*args)
        finally:
            indent -= 1
        print(("  " * indent) + "{}.({}) -> {}".format(fn.__name__, map(str, args), res))
        return res
    return inner

AUCTION_SIM_HANDLE = -42
ESCROW_SIM_HANDLE = -9000

ERROR_STATE=-1
NONEXIST_STATE=0
CREATED_STATE=1
READY_STATE=2
OPEN_STATE=3
PAYOUT_STATE=4

time = 111
def now():
    return time
def wait(seconds):
    global time
    time += seconds

# TODO error handling

class EscrowSimulator:
    def __init__(self):
        self.state = NONEXIST_STATE

    def __str__(self):
        return "EscrowSimulator"

    @trace
    def current_state(self, auction_context, escrow_handle):
        if escrow_handle != ESCROW_SIM_HANDLE:
            return ERROR_STATE
        return self.state

    @trace
    def create(self, params):
        self.state = CREATED_STATE
        self.params = params
        self.assets = {}
        return ESCROW_SIM_HANDLE

    # @trace
    # def opt_in(self, asset):
    #     self.assets[asset] = 0

    # @trace
    # def close_out(self, asset):

class AuctionSimulator:
    def __init__(self):
        self.state = NONEXIST_STATE

        # map address -> receipt iff opted in
        self.accounts = {}

    def __str__(self):
        return "AuctionSimulator"

    @trace
    def current_state(self, auction_context, auction_handle):
        if auction_handle == AUCTION_SIM_HANDLE:
            return ERROR_STATE
        # TODO return correct other statuses
        return self.state

    @trace
    def read_auction_params(self, auction_handle):
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)
        return self.params

    @trace
    def read_tranche_index(self, auction_handle):
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)
        return self.tranche_index

    @trace
    def read_tranche_ring_sum(self, auction_handle):
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)
        return self.tranche_ring_sum()

    @trace
    def read_bid_ring_sum(self, auction_handle):
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)
        return self.bid_ring_sum()

    @trace
    def read_auction_raised(self, auction_handle):
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)
        return self.auction_raised

    @trace
    def read_tranche_supply(self, auction_handle):
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)
        return self.tranche_supply

    @trace
    def read_escrow_handle(self, auction_handle):
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)
        return self.escrow

    @trace
    def enumerate_bidders(self, auction_handle):
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)

        bidders = []
        for address in self.accounts:
            if self.accounts[address] > 0:
                bidders.append(address)
        return bidders


    # TODO what to do if bidder has not opted in?
    # current behavior raises exception
    @trace
    def read_bid_amount(self, auction_handle, address):
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)
        return self.accounts[address]

    def check_auction_handle(self, auction_handle):
        if auction_handle != AUCTION_SIM_HANDLE:
            raise(Exception("bad auction handle {}".format(auction_handle)))

    def check_auction_exists(self, auction_handle):
        if self.state == NONEXIST_STATE or self.state == ERROR_STATE:
            raise(Exception("bad auction state {}".format(self.state)))

    def check_auction_context(self, auction_context):
        if not isinstance(auction_context, AuctionContext):
            raise(Exception("bad auction context {}"))

    def tranche_ring_sum(self):
        least = self.tranche_index - self.params["lookback"]
        if least < 0:
            least = 0
        total = 0
        for i in range(least, self.tranche_index):
            total += self.tranche_sizes[i]
        return total

    def bid_ring_sum(self):
        least = self.tranche_index - self.params["lookback"]
        if least < 0:
            least = 0
        total = 0
        for i in range(least, self.tranche_index):
            total += self.amounts_raised[i]
        return total

    def auction_amount_correct(self, intended_auction_amount, remainder):
        if self.tranche_index < self.params["lookback"]:
            # TODO check remainder?
            return intended_auction_amount == self.params["init_tranches_size"]

        if (self.bid_ring_sum() == 0 or
            self.params["supply"] * self.params["supply_percent_hths"] <= self.tranche_ring_sum()):
            # TODO check remainder?
            return intended_auction_amount == self.params["min_tranche_size"]

        divisor = (self.params["lookback"] * self.params["anchor"] * self.params["supply_percent_hths"] +
                   self.params["num_tranches"] * self.bid_ring_sum())

        if remainder >= divisor:
            return False

        return (intended_auction_amount * divisor + remainder
                ==
                2 * self.bid_ring_sum() *
                (self.params["supply"] * self.params["supply_percent_hths"] -
                 self.tranche_ring_sum()))

    def payout_amount_correct(self, bid_amount, intended_payout_amount, remainder):
        if remainder >= self.auction_raised:
            return False

        return (intended_payout_amount * self.auction_raised + remainder
                ==
                self.tranche_supply * bid_amount)

    # returns auction handle
    @trace
    def create(self, params):
        self.state = CREATED_STATE
        self.params = params

        self.tranche_sizes = []
        self.amounts_raised = []
        for i in range(self.params["num_tranches"]):
            self.tranche_sizes.append(0)
            self.amounts_raised.append(0)

        # needed due to default values
        self.tranche_index = 0
        self.outstanding_receipts = 0
        self.auction_raised = 0
        self.auction_deadline = 0

        return AUCTION_SIM_HANDLE

    @trace
    def bind_escrow(self, auction_context, auction_handle, escrow_handle):
        self.check_auction_context(auction_context)
        self.check_auction_handle(auction_handle)
        if escrow_handle != ESCROW_SIM_HANDLE:
            raise(Exception("bad escrow handle {}".format(escrow_handle)))
        self.escrow = escrow_handle
        return True

    @trace
    def destroy(self, auction_context, auction_handle):
        self.check_auction_context(auction_context)
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)
        if self.params["num_tranches"] != self.tranche_index:
            raise(Exception("num_tranches != tranche_index: {} != {}".format(self.params["num_tranches"], self.tranche_index)))
        self.state = NONEXIST_STATE
        return True

    @trace
    def opt_in(self, auction_context, auction_handle, address):
        self.check_auction_context(auction_context)
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)
        if address in self.accounts:
            raise(Exception("address already opted in {}".format(address)))
        self.accounts[address] = 0
        return True

    @trace
    def close_out(self, auction_context, auction_handle, address):
        self.check_auction_context(auction_context)
        self.check_auction_handle(auction_handle)
        if address not in self.accounts:
            raise(Exception("address not opted in {}".format(address)))
        self.outstanding_receipts -= self.accounts[address]
        del self.accounts[address]
        return True

    @trace
    def opted_in(self, auction_context, auction_handle, address):
        self.check_auction_context(auction_context)
        self.check_auction_handle(auction_handle)
        return address in self.accounts

    # TODO this should only be permitted from a privileged sender
    @trace
    def open_auction(self, auction_context, auction_handle, other_transaction, remainder):
        self.check_auction_context(auction_context)
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)

        if self.auction_deadline != 0:
            raise(Exception("auction_deadline != 0".format(self.auction_deadline)))
        if self.tranche_index >= self.params["num_tranches"]:
            raise(Exception("tranche_index >= num_tranches: {} > {}".format(self.tranche_index, self.params["num_tranches"])))
        if other_transaction.type != ASSET_TXN:
            raise(Exception("other_transaction.type != ASSET_TXN: {}".format(other_transaction.type)))
        if other_transaction.asset != SUPPLY_UNITS:
            raise(Exception("other_transaction.asset != SUPPLY_UNITS: {}".format(other_transaction.asset)))
        if other_transaction.asset_receiver != self.escrow:
            raise(Exception("other_transaction.asset_receiver != escrow: {} != {}".format(other_transaction.asset_receiver, self.escrow)))
        if other_transaction.asset_close_to != 0:
            raise(Exception("other_transaction.asset_close_to != 0: {}".format(other_transaction.asset_close_to)))
        if other_transaction.asset_sender != 0:
            raise(Exception("other_transaction.asset_sender != 0: {}".format(other_transaction.asset_sender)))
        if not self.auction_amount_correct(other_transaction.asset_amount, remainder):
            raise(Exception("other_transaction.asset_amount, remainder are incorrect: {}, {}".format(other_transaction.asset_amount, remainder)))

        self.tranche_supply = other_transaction.asset_amount
        self.auction_deadline = now() + self.params["auction_duration"]

    @trace
    def close_auction(self, auction_context, auction_handle):
        self.check_auction_context(auction_context)
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)

        n = now()
        if n < self.auction_deadline:
            raise(Exception("now() < self.auction_deadline: {} < {}".format(n, self.auction_deadline)))
        if self.outstanding_receipts != 0:
            raise(Exception("outstanding_receipts != 0: {}".format(self.outstanding_receipts)))

        self.tranche_sizes[self.tranche_index] = self.tranche_supply
        self.amounts_raised[self.tranche_index] = self.auction_raised

        self.tranche_supply = 0
        self.auction_raised = 0
        self.auction_deadline = 0

        self.tranche_index += 1

    # note: bidder must be sender here
    # TODO entering lots of small bids incurs fees proportional to fees spent (worth preventing?)
    @trace
    def enter_bid(self, auction_context, auction_handle, bidder, other_transaction):
        self.check_auction_context(auction_context)
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)

        n = now()
        if n >= self.auction_deadline:
            raise(Exception("now() >= self.auction_deadline: {} >= {}".format(n, self.auction_deadline)))
        if other_transaction.type != ASSET_TXN:
            raise(Exception("other_transaction.type != ASSET_TXN: {}".format(other_transaction.type)))
        if other_transaction.asset != BID_UNITS:
            raise(Exception("other_transaction.asset != BID_UNITS: {}".format(other_transaction.asset)))
        if other_transaction.asset_receiver != self.escrow:
            raise(Exception("other_transaction.asset_receiver != escrow: {} != {}".format(other_transaction.asset_receiver, escrow)))
        if other_transaction.asset_sender != 0:
            raise(Exception("other_transaction.asset_sender != 0: {}".format(other_transaction.asset_sender)))

        self.accounts[bidder] += other_transaction.asset_amount
        self.outstanding_receipts += other_transaction.asset_amount
        self.auction_raised += other_transaction.asset_amount

    # TODO add this to contract
    @trace
    def destroy_bid(self, auction_context, auction_handle, bidder):
        self.check_auction_context(auction_context)
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)

        n = now()
        if n < self.auction_deadline:
            raise(Exception("now() < self.auction_deadline: {} < {}".format(n, self.auction_deadline)))
        if self.env.opted_in(bidder, SUPPLY_UNITS):
            raise(Exception("cannot destroy bid of bidder who has opted in to SUPPLY_UNITS"))

        # note that we cannot close out in the stead of the malicious bidder
        self.outstanding_receipts -= self.accounts[bidder]
        self.accounts[bidder] = 0

    # note: bidder cannot be sender here
    @trace
    def payout_bid(self, auction_context, auction_handle, bidder, other_transaction, remainder):
        self.check_auction_context(auction_context)
        self.check_auction_handle(auction_handle)
        self.check_auction_exists(auction_handle)

        n = now()
        if n < self.auction_deadline:
            raise(Exception("now() < self.auction_deadline: {} < {}".format(n, self.auction_deadline)))
        if other_transaction.type != ASSET_TXN:
            raise(Exception("other_transaction.type != ASSET_TXN: {}".format(other_transaction.type)))
        if other_transaction.asset != SUPPLY_UNITS:
            raise(Exception("other_transaction.asset != SUPPLY_UNITS: {}".format(other_transaction.asset)))
        if other_transaction.asset_receiver != bidder:
            raise(Exception("cannot pay out bid to non-bidder: {} != {}".format(asset_receiver, bidder)))
        if other_transaction.asset_close_to != 0:
            raise(Exception("other_transaction.asset_close_to != 0: {}".format(other_transaction.asset_close_to)))
        if other_transaction.asset_sender != 0:
            raise(Exception("other_transaction.asset_sender != 0: {}".format(other_transaction.asset_sender)))
        if not self.payout_amount_correct(self.accounts[bidder], other_transaction.asset_amount, remainder):
            raise(Exception("payout amount incorrect: bid_amount={}, payout_amount={}, remainder={}".format(self.accounts[bidder], other_transaction.asset_amount, remainder)))

        self.outstanding_receipts -= self.accounts[bidder]
        self.accounts[bidder] = 0

class Transaction:
    def __init__(self, type, asset, asset_receiver, asset_close_to, asset_sender, asset_amount):
        self.type = type
        self.asset = asset
        self.asset_receiver = asset_receiver
        self.asset_close_to = asset_close_to
        self.asset_sender = asset_sender
        self.asset_amount = asset_amount

    def __str__(self):
        return "Txn(type={},asset={},asset_receiver={},asset_close_to={},asset_sender={},asset_amount={})".format(self.type, self.asset, self.asset_receiver, self.asset_close_to, self.asset_sender, self.asset_amount)

# returns: AuctionContext
def create_auction_with_sim(sim, esim, admin, params):
    # TODO pass in admin
    auction_handle = sim.create(params)
    escrow_handle = esim.create(params)

    ctx = AuctionContext(user=admin, admin=True, auction_handle=auction_handle)

    ctx.sim = sim
    ctx.esim = esim
    ctx.params = params

    ctx.sim.bind_escrow(ctx, ctx.auction_handle, escrow_handle)

    return ctx

# returns: AuctionContext
def bind_auction_to_sim(sim, esim, ahand, bidder):
    ctx = AuctionContext(user=bidder, admin=False, auction_handle=ahand)

    ctx.sim = sim
    ctx.esim = esim

    return ctx

class AuctionContext:
    def __init__(self, user, admin, auction_handle):
        self.user = user
        self.admin = admin
        self.auction_handle = auction_handle

    def __str__(self):
        return "AuctionContext(user={},admin={},auction_handle={})".format(self.user, self.admin, self.auction_handle)

    # @trace
    def compute_new_tranche_size(self):
        params = self.sim.read_auction_params(self.auction_handle)
        tranche_index = self.sim.read_tranche_index(self.auction_handle)
        bid_ring_sum = self.sim.read_bid_ring_sum(self.auction_handle)
        tranche_ring_sum = self.sim.read_tranche_ring_sum(self.auction_handle)

        if tranche_index < params["lookback"]:
            return params["init_tranches_size"], 0

        if (bid_ring_sum == 0 or
            params["supply"] * params["supply_percent_hths"] <= tranche_ring_sum):
            return params["min_tranche_size"], 0

        divisor = (params["lookback"] * params["anchor"] * params["supply_percent_hths"] +
                   params["num_tranches"] * bid_ring_sum)

        dividend = (2 * bid_ring_sum *
                    (params["supply"] * params["supply_percent_hths"] -
                     tranche_ring_sum))

        print("+++", dividend, divisor)

        # note: dividend and divisor at this point should be of Decimal type
        return dividend // divisor, dividend % divisor

    # @trace
    def compute_payout(self, bidder):
        auction_raised = self.sim.read_auction_raised(self.auction_handle)
        tranche_supply = self.sim.read_tranche_supply(self.auction_handle)
        bid_amount = self.sim.read_bid_amount(self.auction_handle, bidder)

        # divisor = 100 * auction_raised
        divisor = auction_raised
        dividend = tranche_supply * bid_amount

        # note: dividend and divisor at this point should be of Decimal type
        return dividend // divisor, dividend % divisor

    # @trace
    def find_escrow_handle(self):
        return self.sim.read_escrow_handle(self.auction_handle)

    @trace
    def open_auction(self):
        amount, remainder = self.compute_new_tranche_size()
        other_transaction = Transaction(type=ASSET_TXN,
                                        asset=SUPPLY_UNITS,
                                        asset_receiver=self.find_escrow_handle(),
                                        asset_close_to=0,
                                        asset_sender=0,
                                        asset_amount=amount
        )
        self.sim.open_auction(self, self.auction_handle, other_transaction, remainder)

    @trace
    def close_auction(self):
        self.payout_auction()
        self.sim.close_auction(self, self.auction_handle)

    @trace
    def opt_in(self):
        # TODO must also opt in for the SUPPLY_TOKEN
        self.sim.opt_in(self, self.auction_handle, self.user)

    @trace
    def close_out(self):
        self.sim.close_out(self, self.auction_handle, self.user)

    @trace
    def enter_bid(self, amount):
        # TODO need to check if auction is accepting bids first (?)
        self.sim.enter_bid(self, self.auction_handle, self.user,
                      Transaction(type=ASSET_TXN,
                                  asset=BID_UNITS,
                                  asset_receiver=self.find_escrow_handle(),
                                  asset_close_to=0,
                                  asset_sender=0,
                                  asset_amount=amount,
        ))

    # @trace
    def payout_bid(self, bidder):
        amount, remainder = self.compute_payout(bidder)
        other_transaction = Transaction(type=ASSET_TXN,
                                        asset=SUPPLY_UNITS,
                                        asset_receiver=bidder,
                                        asset_close_to=0,
                                        asset_sender=0,
                                        asset_amount=amount,
        )
        self.sim.payout_bid(self, self.auction_handle, bidder, other_transaction, remainder)

    # @trace
    def payout_auction(self):
        bidders = self.sim.enumerate_bidders(self.auction_handle)
        for bidder in bidders:
            self.payout_bid(bidder)

    @trace
    def destroy(self):
        self.sim.destroy(self, self.auction_handle)

def run():
    sim = AuctionSimulator()
    esim = EscrowSimulator()

    alice = "alice"
    bob = "bob"
    carol = "carol"
    dave = "dave"
    eve = "eve"

    actx = create_auction_with_sim(sim=sim, esim=esim, admin="alice", params=weekly_params)
    bctx = bind_auction_to_sim(sim=sim, esim=esim, ahand=actx.auction_handle, bidder="bob")
    cctx = bind_auction_to_sim(sim=sim, esim=esim, ahand=actx.auction_handle, bidder="carol")
    dctx = bind_auction_to_sim(sim=sim, esim=esim, ahand=actx.auction_handle, bidder="dave")

    bctx.opt_in()
    cctx.opt_in()
    dctx.opt_in()
    dctx.close_out()

    for _ in range(actx.params["num_tranches"]):
        actx.open_auction()

        wait(actx.params["auction_duration"] - 100)

        try:
            actx.close_auction()
        except:
            pass
        else:
            raise(Exception("expected close to fail for deadline: now()={}, auction_deadline={}".format(now(), sim.auction_deadline)))

        bctx.enter_bid(24 * 1000000)
        cctx.enter_bid(49 * 1000000)

        wait(300)

        actx.close_auction()

    actx.destroy()

run()
