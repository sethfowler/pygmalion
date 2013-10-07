int func() { return 0; }
int func_varargs(int n, ...) { return 0; }

int main(int argc, char** argv)
{
  int (*func_ptr)() = func;
  int (*func_ptr_varargs)(int, ...) = &func_varargs;
  int (&func_ref)() = func;
  int (&func_ref_varargs)(int, ...) = *func_varargs;

  return func() +
         func_varargs(1, 2, 3) +
         func_ptr() +
         (*func_ptr_varargs)(1, 2, 3) +
         func_ref() +
         func_ref_varargs(1, 2, 3);
}
