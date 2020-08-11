import smartpy as sp
import os

CWD = os.getcwd()
TARGET = os.path.join(CWD, "benchmarks", "examples", "transferFrom")

class MyToken(sp.Contract):
  def __init__(self, owner, initSupply):
    self.init(
      owner = owner,
      totalSupply = sp.mutez(initSupply),
      balance = sp.map(tkey = sp.TAddress, tvalue = sp.TMutez),
      allowance = sp.map(tkey = sp.TAddress, tvalue = sp.TMap(sp.TAddress, sp.TMutez))
    )
  
  @sp.entry_point
  def transfer(self, params):
    sp.verify(self.data.balance[sp.sender] >= params.value)
    self.data.balance[sp.sender] -= params.value
    self.data.balance[params.toAddr] += params.value

  @sp.entry_point
  def approve(self, params):
    self.data.allowance[sp.sender][params.proxAddr] = params.value
  
  @sp.entry_point
  def transferFrom(self, params):
    sp.verify(self.data.balance[params.fromAddr] >= params.value)
    sp.verify(self.data.balance[params.toAddr] + params.value > self.data.balance[params.toAddr])
    sp.verify(self.data.allowance[params.fromAddr][sp.sender] >= params.value)
    self.data.balance[params.fromAddr] -= params.value
    self.data.balance[params.toAddr] += params.value
    self.data.allowance[params.fromAddr][sp.sender] += params.value     # bug: '+=' should have been '-='

contract = MyToken(sp.address("tz1hdQscorfqMzFqYxnrApuS5i6QSTuoAp3w"), 6917529027641081856)

import smartpybasic as spb
spb.compileContract(contract, targetBaseFilename = TARGET)