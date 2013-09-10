static int global_var = 0;

int main(int argc, char** argv)
{
  int local_var = 0;
  return global_var
       + local_var;
}
