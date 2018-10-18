kernel void add(
  global T *val,
  T  arg)
{
  uint id = get_global_id(0);
  val[id] += arg;
}