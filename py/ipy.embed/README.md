## Tricks
 - global variable
   * ScriptScope.SetVariable

 - passing argv 
   * code
     IDictionary<string, object> options = new Dictionary<string, object>();
     options["Arguments"] = new [] { "foo", "bar" };
     engine = Python.CreateEngine(options);

