import smartpy as sp

class Toy(sp.Contract):
  def __init__(self, init):
    self.init(
      total = sp.mutez(init),
      lst = sp.list(l=[], t=sp.TMutez),
    )
  
  @sp.entry_point
  def addElem(self, params):
    sp.verify(params.value < self.data.total)
    self.data.total = self.data.total - params.value
    self.data.lst.push(params.value)

  @sp.entry_point
  def retElem(self, params):
    with sp.match_cons(self.data.lst) as l1:
      self.data.total = self.data.total + l1.head
      self.data.lst = l1.tail