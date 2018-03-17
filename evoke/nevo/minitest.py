from Nevo import NevoTemplate, nevo

tp = NevoTemplate('simple_content.evo')
print tp({})

class Someclass(object):
  @nevo
  def view(self, req):
    ""

class Req(object):
  """"""
  cookies = {}
req = Req()
x = Someclass()
print x.view(req)
