kernel void add(
  global T *val,
  T  arg)
{
  uint id = get_global_id(0);
  val[id] += arg;
}
kernel void addv(
  global T *val,
  const global T *args)
{
  uint id = get_global_id(0);
  val[id] += args[id];
}