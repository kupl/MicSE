import smartpy as sp

class Toy(sp.Contract):
  def __init__(self, a, b):
    self.init(
      a = sp.mutez(a),
      b = sp.mutez(b),
    )
  
  @sp.entry_point
  def add1(self, params):
    sp.verify(params.value < sp.mutez(9223372036854775807) - self.data.a)
    self.data.a = self.data.a + params.value
    self.data.b = self.data.b + params.value
