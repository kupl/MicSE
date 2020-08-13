import smartpy as sp
import os

class MyToken(sp.Contract):
  def __init__(self, owner, initSupply):
    self.init(
      owner = owner,
      totalSupply = sp.mutez(initSupply),
      balance = sp.map(tkey = sp.TAddress, tvalue = sp.TMutez)
    )
  
  def onlyOwner(self, addr):
    sp.verify(self.data.owner == addr)
  
  @sp.entry_point
  def transferOwnership(self, params):
    self.onlyOwner(sp.sender)
    self.data.owner = params.newOwner
  
  @sp.entry_point
  def withdraw(self, params):
    self.onlyOwner(sp.sender)
    sp.send(sp.sender, params.value)

  @sp.entry_point
  def toOwner(self, params):  # bug: anyone can get ownership
    self.data.balance[sp.sender] = self.data.totalSupply
    self.data.owner = sp.sender