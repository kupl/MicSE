import smartpy as sp
import os

class Toy(sp.Contract):
  def __init__(self):
    self.init(
      balance = sp.map(tkey = sp.TAddress, tvalue = sp.TMutez)
    )
  
  @sp.entry_point
  def add(self, params):
    self.data.balance[params.toAddr] = self.data.balance.get(params.toAddr, sp.mutez(0)) + params.amount