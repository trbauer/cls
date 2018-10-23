kernel void add(
  global T *val,
  T  arg)
{
  uint id = get_global_id(0);
  if (id == get_global_size(0) - 1) {
    arg = 0;
  }
  val[id] += arg;
}