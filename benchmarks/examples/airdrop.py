import smartpy as sp
import os

class MyService(sp.Contract):
  def __init__(self, owner, initSupply):
    self.init(
      owner = owner,
      totalSupply = sp.mutez(initSupply),
      totalDistributed = sp.mutez(0),
      distributionFinished = sp.bool(False),
      balance = sp.map(tkey = sp.TAddress, tvalue = sp.TMutez)
    )
  
  def onlyOwner(self, addr):
    sp.verify(self.data.owner == addr)

  @sp.entry_point
  def doAirdrop(self, params):
    sp.verify(self.data.distributionFinished == sp.bool(False))
    self.onlyOwner(sp.sender)
    sp.verify(self.data.totalDistributed < self.data.totalSupply)  # bug: totalDistributed can be bigger than totalSupply
    self.data.balance[params.participant] = self.data.balance.get(params.participant, sp.mutez(0)) + params.amount
    self.data.totalDistributed += params.amount
    sp.if self.data.totalDistributed >= self.data.totalSupply:
      self.data.distributionFinished = sp.bool(True)
