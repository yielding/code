dtrace:::BEGIN
{
  i = 0;
}

profile:::tick-1sec
{
  i = i + 1;
  trace(i);
}

dtrace:::END
{
  trace(i);
}
