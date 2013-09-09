enum global_enum { global_enum_val };
enum { global_anonymous_enum_val } global_anonymous_enum_var;

int main(int argc, char** argv)
{
  enum local_enum { local_enum_val };
  enum { local_anonymous_enum_val } local_anonymous_enum_var;
  return global_enum_val +
         global_anonymous_enum_val +
         local_enum_val +
         local_anonymous_enum_val;
}
