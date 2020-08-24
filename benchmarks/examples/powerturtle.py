import smartpy as sp

class PowerTurtle_1(sp.Contract):
    def __init__(self):
        self.init(turtleSize = 1,
                    foodTotal = 0)
                    
    @sp.entry_point
    def donateFood(self, amount):
        self.data.foodTotal += amount
        
    @sp.entry_point
    def feedTurtle(self):
        self.data.turtleSize = self.data.turtleSize << self.data.foodTotal



class PowerTurtle_2(sp.Contract):
    def __init__(self):
        self.init(turtleSize = 1,
                    tmpvar = 0,
                    foodMap = sp.map(tkey=sp.TAddress, tvalue=sp.TNat))
    
    @sp.entry_point
    def donateFood(self, amount):
        self.data.foodMap[sp.sender] = self.data.foodMap.get(sp.sender, 0) + amount

    @sp.entry_point
    def feedTurtle(self):
        self.data.tmpvar = 0
        sp.for x in self.data.foodMap.values():
            self.data.tmpvar += x
        self.data.turtleSize = self.data.turtleSize << self.data.tmpvar


class PowerTurtle_2_guarded(sp.Contract):
    def __init__(self):
        self.init(turtleSize = 1,
                    tmpvar = 0,
                    foodMap = sp.map(tkey=sp.TAddress, tvalue=sp.TNat))
    
    @sp.entry_point
    def donateFood(self, amount):
        self.data.foodMap[sp.sender] = self.data.foodMap.get(sp.sender, 0) + amount

    @sp.entry_point
    def feedTurtle(self):
        self.data.tmpvar = 0
        sp.for x in self.data.foodMap.values():
            self.data.tmpvar += x
        sp.if (self.data.tmpvar > 256):
            self.data.tmpvar = 256
        self.data.turtleSize = self.data.turtleSize << self.data.tmpvar





class PowerTurtle_3(sp.Contract):
    def __init__(self):
        self.init(turtleSize = 1,
                    tmpvar = 0,
                    foodMap = sp.map(tkey=sp.TAddress, tvalue=sp.TNat))

    @sp.entry_point
    def donateFood(self, amount):
        sp.if amount > 100:
            amount = 100
        sp.if amount < 0:
            amount = 0
        self.data.foodMap[sp.sender] = self.data.foodMap.get(sp.sender, 0) + amount

    @sp.entry_point
    def feedTurtle(self):
        self.data.tmpvar = 0
        sp.for x in self.data.foodMap.values():
            self.data.tmpvar += x
        self.data.turtleSize = self.data.turtleSize << self.data.tmpvar


