#include "virtual.h"

struct ABC_struct { ABC var; };
struct ABC_ptr_struct { ABC* ptr; };

int main(int argc, char** argv)
{
  ABC* ABC_ptr = new ABC;

  // Structs containing instance values.
  ABC_struct ABC_struct_instance;
  ABC_struct_instance.var.A_pure_method();
  ABC_struct_instance.var.AB_method(0);
  ABC_struct_instance.var.AB::AB_method(0);
  ABC_struct_instance.var.ABC::AB_method(0);

  ABC_struct* ABC_struct_ptr = new ABC_struct;
  ABC_struct_ptr->var.A_pure_method();
  ABC_struct_ptr->var.AB_method(0);
  ABC_struct_ptr->var.AB::AB_method(0);
  ABC_struct_ptr->var.ABC::AB_method(0);

  // Structs containing pointers to instances.
  ABC_ptr_struct ABC_ptr_struct_instance { ABC_ptr };
  ABC_ptr_struct_instance.ptr->A_pure_method();
  ABC_ptr_struct_instance.ptr->AB_method(0);
  ABC_ptr_struct_instance.ptr->AB::AB_method(0);
  ABC_ptr_struct_instance.ptr->ABC::AB_method(0);

  ABC_ptr_struct* ABC_ptr_struct_ptr = new ABC_ptr_struct { ABC_ptr };
  ABC_ptr_struct_ptr->ptr->A_pure_method();
  ABC_ptr_struct_ptr->ptr->AB_method(0);
  ABC_ptr_struct_ptr->ptr->AB::AB_method(0);
  ABC_ptr_struct_ptr->ptr->ABC::AB_method(0);

  return 0;
}
