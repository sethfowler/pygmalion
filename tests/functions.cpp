int var() { return 0; }
int varargs(int n, ...) { return 0; }

int main(int argc, char** argv)
{
  return var() +
         varargs(1, 2, 3);
}
