__kernel void dims(global int4 *gvals,global int4 *lvals)
{
  int id =
    (get_global_id(2) * get_global_size(1) * get_global_size(0)) +
    (get_global_id(1) * get_global_size(0)) +
    (get_global_id(0));
  gvals[id] = (int4)(get_global_size(0),get_global_size(1),get_global_size(2),0);
  lvals[id] = (int4)(get_local_size(0),get_local_size(1),get_local_size(2),0);
}