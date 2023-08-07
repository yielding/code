dtrace::BEGIN
{
  trace("Hello World from DTrace!");
  exit(0);
}