struct global_struct { int global_struct_val; };
struct { int global_anonymous_struct_val; } global_anonymous_struct_var = { 0 };

int main(int argc, char** argv)
{
  struct local_struct { int local_struct_val; };
  struct { int local_anonymous_struct_val; } local_anonymous_struct_var;

  global_struct global_struct_var = { 0 };
  local_struct local_struct_var = { 0 };

  return global_struct_var.global_struct_val +
         global_anonymous_struct_var.global_anonymous_struct_val +
         local_struct_var.local_struct_val +
         local_anonymous_struct_var.local_anonymous_struct_val;
}
