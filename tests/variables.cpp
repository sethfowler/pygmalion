extern int global_extern_var;
extern const int global_extern_const_var;

int global_extern_var = 0;
const int global_extern_const_var = 0;
static int global_var = 0;
static const int global_const_var = 0;

int main(int argc, char** argv)
{
  int local_var = 0;
  const int local_const_var = 0;

  return global_extern_var +
         global_extern_const_var +
         global_var +
         global_const_var +
         local_var +
         local_const_var;
}
