import smartpy as sp

class Deposit(sp.Contract):
  def __init__(self, init):
    self.init(
      deposits = sp.list(l=[], t=sp.TMutez),
      total_deposit = sp.mutez(0),
      backend = sp.none,
      fee_percent = sp.nat(0),
      fee_acc = sp.mutez(0),
      reward_flag = sp.bool(False),
      reward_size = sp.mutez(0),
      avail_reward_calc = sp.mutez(0)
    )
  
  @sp.entry_point
  def create_deposit(self):
    self.data.deposits.push(sp.amount)
    self.data.total_deposit += sp.amount # Q69

  @sp.entry_point
  def remove_deposit(self):
    with sp.match_cons(self.data.deposits) as deposits:
      self.data.deposits = deposits.tail
      self.data.total_deposit -= deposits.head
    with sp.else_():
      self.data.deposits = sp.list(l=[], t=sp.TMutez)


  @sp.entry_point
  def get_total_deposit(self, param):
    sp.transfer(self.data.total_deposit, sp.mutez(0), sp.contract(sp.TMutez, param).open_some())
  
  @sp.entry_point
  def set_backend(self, param):
    self.data.backend = param
  
  @sp.entry_point
  def withdraw(self, param):
    with sp.match_cons(self.data.deposits) as deposits:
      #__MICSE_CHECK (sp.balance >= deposits.head) #Q123
      sp.data.deposits = deposits.tail
      sp.data.total_deposit -= deposits.head
      sp.transfer(sp.unit, deposits.head, sp.contract(sp.TUnit, param).open_some())
    with sp.else_():
      sp.failwith("%withdraw deposits=[]")
  
  @sp.entry_point
  def withdraw_fee(self):
    with sp.if_(self.data.backend.is_some()):
      #__MICSE_CHECK (sp.balance >= self.data.fee_acc) #Q160
      sp.transfer(sp.unit, self.data.fee_acc, sp.contract(sp.TUnit, sp.data.backend.open_some()).open_some())
      self.data.fee_acc = sp.mutez(0)
    with sp.else_():
      sp.failwith("%withdraw_fee backend=None")
  
  @sp.entry_point
  def set_reward_size(self, param):
    self.data.reward_size = param

  @sp.entry_point
  def set_fee_percent(self, param):
    with sp.if_(param < sp.nat(101)):
      self.data.fee_percent = param
    with sp.else_():
      sp.failwith("%set_fee_percent >= 101")
  
  @sp.entry_point
  def setup_reward(self):
    full_active_deposit = sp.mutez(0)
    deposits_copy = self.data.deposits
    loop_cond = sp.bool(True)
    with sp.while_(loop_cond):
      with sp.match_cons(deposits_copy) as deposits:
        full_active_deposit = full_active_deposit + deposits.head
        deposits_copy = deposits.tail
      with sp.else_():
        loop_cond = sp.bool(False)
    #__MICSE_CHECK (self.data.total_deposit == full_active_deposit) #Q252
    sp.verify(self.data.total_deposit == full_active_deposit)
    self.data.avail_reward_calc = sp.balance - full_active_deposit
    self.data.reward_flag = sp.bool(True)

  @sp.entry_point
  def process_reward(self):
    sp.verify(self.data.reward_flag)
    new_deposits = sp.list(l=[], t=sp.TMutez)
    new_total_deposit = sp.mutez(0)
    given_reward = sp.mutez(0)
    loop_cond = sp.bool(True)
    with sp.while_(loop_cond):
      with sp.match_cons(self.data.deposits) as deposits:
        self.data.deposits = deposits.tail
        new_deposits.push(deposits.head + self.data.reward_size)
        new_total_deposit += deposits.head + self.data.reward_size
        given_reward += self.data.reward_size
      with sp.else_():
        loop_cond = sp.bool(False)
    self.data.deposits = new_deposits
    self.data.total_deposit = new_total_deposit
    #__MICSE_CHECK (self.data.avail_reward_calc >= given_reward) #Q344
    fee = sp.fst((sp.ediv (sp.mul (self.data.avail_reward_calc, self.data.fee_percent), 100)).open_some()) #Q349
    #__MICSE_CHECK (self.data.avail_reward_calc >= fee) #Q363
    sp.verify(self.data.avail_reward_calc >= fee)
    self.data.fee_acc += fee
    self.data.reward_flag = sp.bool(False)