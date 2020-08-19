import smartpy as sp
import os

class Toy(sp.Contract):
  def __init__(self, value):
    self.init(
      value = sp.mutez(value)
    )
  
  @sp.entry_point
  def add1(self, params):
    self.data.value = params.value + sp.mutez(1)