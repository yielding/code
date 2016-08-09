import json

class NullObj:
    def is_ok(self): 
        return False

    def reason(self): 
        return "JSON load error"

class JSONResult:
    def __init__(self, result):
        self._result = result

    def is_ok(self):
        return True

    def result(self):
        return self._result

class JSONWrapper:
    @staticmethod
    def loads(s):
        try:
            result = json.loads(s)
        except Exception, e:
            result = None

        if result != None:
            return JSONResult(result)

        return NullObj()


res = JSONWrapper.loads('["foo", {"bar":["baz", null, 1.0, 2]}]')
if res.is_ok():
    print res.result()

res = JSONWrapper.loads('XXXXX["foo", {"bar":["baz", null, 1.0, 2]}]')
if res.is_ok():
    print res.result()
else:
    print res.reason()
    
