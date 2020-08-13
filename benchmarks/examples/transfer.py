import smartpy as sp
import os

class MyToken(sp.Contract):
  def __init__(self, owner, initSupply):
    self.init(
      owner = owner,
      totalSupply = sp.mutez(initSupply),
      balance = sp.map(tkey = sp.TAddress, tvalue = sp.TMutez),
    )
    self.data.balance[owner] = sp.mutez(initSupply)
  
  @sp.entry_point
  def transfer(self, params):
    sp.verify(self.data.balance[sp.sender] >= params.value)
    self.data.balance[sp.sender] -= params.value
    self.data.balance[params.toAddr] += params.value    # safe