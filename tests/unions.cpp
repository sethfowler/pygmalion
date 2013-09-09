union global_union
{
  int global_union_val_int;
  char global_union_val_char;
};

union
{
  int global_anonymous_union_val_int;
  char global_anonymous_union_val_char;
} global_anonymous_union_var = { 0 };

int main(int argc, char** argv)
{
  union local_union
  {
    int local_union_val_int;
    char local_union_val_char;
  };

  union
  {
    int local_anonymous_union_val_int;
    char local_anonymous_union_val_char;
  } local_anonymous_union_var = { 0 };

  global_union global_union_var = { 0 };
  local_union local_union_var = { 0 };

  return global_union_var.global_union_val_int +
         global_union_var.global_union_val_char +
         global_anonymous_union_var.global_anonymous_union_val_int +
         global_anonymous_union_var.global_anonymous_union_val_char +
         local_union_var.local_union_val_int +
         local_union_var.local_union_val_char +
         local_anonymous_union_var.local_anonymous_union_val_int +
         local_anonymous_union_var.local_anonymous_union_val_char;
}
