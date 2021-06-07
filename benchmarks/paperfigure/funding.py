import smartpy as sp

class Funding(sp.Contract):
  def __init__(self):
    self.init(
      journaliere = sp.hash_key(sp.key("tz3YtM37sXgGSiFX1JuQjcWVG9qFc4gbAML3")),
      fonds_restant = sp.mutez(256000000),
      duree_de_blocage = 86400,
      file = sp.pair(sp.list(l=[], t=sp.TPair(sp.TTimestamp, sp.TMutez)), sp.list(l=[], t=sp.TPair(sp.TTimestamp, sp.TMutez))),
      maitresse = sp.hash_key(sp.key("tz1RmiqaJoqfPGQmC6hXK1LkZFpoBkLWxh1m")),
      sel = sp.pair(sp.nat(0), sp.nat(1))
    )
  
  @sp.entry_point
  def default(self, params):
    self.data = self.data
  
  @sp.entry_point
  def appel_clef_maitresse(self, params):
    self.data.sel = sp.pair(sp.fst(self.data.sel) + 2, sp.snd(self.data.sel))
    sp.if self.data.maitresse != sp.hash_key(params.clef_publique):
      sp.failwith("")
    sp.if sp.check_signature (params.clef_publique, params.signature1, sp.concat([(sp.pack(params)), (sp.pack(sp.fst(self.data.sel))), (sp.pack(sp.pair(sp.self, sp.chain_id)))])):
      (params.lambda1)(sp.unit)
    sp.else:
      sp.failwith("")
  
  @sp.entry_point
  def transfer(self, params):
    sp.if self.data.maitresse != sp.hash_key(params.clef_publique):
      sp.failwith("")
    sp.if sp.check_signature (params.clef_publique, params.signature1, sp.concat([(sp.pack(sp.pair(params.beneficiaires, params.nouvelle_clef_maitresse))), (sp.pack(sp.snd(self.data.sel))), (sp.pack(sp.pair(sp.self, sp.chain_id)))])):
      flag1 = sp.local("flag1", True)
      sp.while flag1.value:
        f1, f2 = sp.match_pair(self.data.file)
        with sp.match_cons(f1) as fht1:
          sp.if sp.now >= sp.fst(fht1.head):
            self.data.file = sp.pair(fht1.tail, f2)
            self.data.fonds_restant = self.data.fonds_restant + sp.snd(fht1.head)
          sp.else:
            flag1.value = sp.bool(False)
        sp.else:
          flag2 = sp.local("flag2", True)
          sp.while flag2.value:
            with sp.match_cons(f2) as fht2:
              f1.push(fht2.head)
              self.data.file = sp.pair(f1, fht2.tail)
            sp.else:
              flag1.value = sp.bool(False)
              flag2.value = sp.bool(False)
    sp.else:
      sp.failwith("")

