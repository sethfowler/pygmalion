#define VAR 0
#define VARF(x) (int) x

int main(int argc, char** argv)
{
  char local_var = 0;

  return VAR
       + VARF(local_var);
}
