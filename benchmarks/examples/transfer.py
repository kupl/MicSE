import smartpy as sp
import os

CWD = os.getcwd()
TARGET = os.path.join(CWD, "benchmarks", "examples", "transfer")

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
    self.data.balance[params.toAddr] += params.value

contract = MyToken(sp.address("tz1hdQscorfqMzFqYxnrApuS5i6QSTuoAp3w"), 6917529027641081856)

import smartpybasic as spb
spb.compileContract(contract, targetBaseFilename = TARGET)