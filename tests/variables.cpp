extern int global_extern_var;
extern const int global_extern_const_var;
extern int* global_extern_ptr;
extern const int* global_extern_const_ptr;
extern int* const global_extern_ptr_const;
extern const int* const global_extern_const_ptr_const;
extern int& global_extern_ref;
extern const int& global_extern_const_ref;

int global_extern_var = 0;
const int global_extern_const_var = 0;
int* global_extern_ptr = &global_extern_var;
const int* global_extern_const_ptr = &global_extern_const_var;
int* const global_extern_ptr_const = &global_extern_var;
const int* const global_extern_const_ptr_const = &global_extern_const_var;
int& global_extern_ref = *global_extern_ptr;
const int& global_extern_const_ref = *global_extern_const_ptr;

static int global_var = 0;
static const int global_const_var = 0;
static int* global_ptr = &global_var;
static const int* global_const_ptr = &global_const_var;
static int* const global_ptr_const = &global_var;
static const int* const global_const_ptr_const = &global_const_var;
static int& global_ref = *global_ptr;
static const int& global_const_ref = *global_const_ptr;

int main(int argc, char** argv)
{
  int local_var = 0;
  const int local_const_var = 0;
  int* local_ptr = &local_var;
  const int* local_const_ptr = &local_const_var;
  int* const local_ptr_const = &local_var;
  const int* const local_const_ptr_const = &local_const_var;
  int& local_ref = *local_ptr;
  const int& local_const_ref = *local_const_ptr;

  return global_extern_var +
         global_extern_const_var +
         *global_extern_ptr +
         *global_extern_const_ptr +
         *global_extern_ptr_const +
         *global_extern_const_ptr_const +
         global_extern_ref +
         global_extern_const_ref +
         global_var +
         global_const_var +
         *global_ptr +
         *global_const_ptr +
         *global_ptr_const +
         *global_const_ptr_const +
         global_ref +
         global_const_ref +
         local_var +
         local_const_var +
         *local_ptr +
         *local_const_ptr +
         *local_ptr_const +
         *local_const_ptr_const +
         local_ref +
         local_const_ref;
}
