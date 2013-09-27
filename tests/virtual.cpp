#include "virtual.h"

int main(int argc, char** argv)
{
  // Instance values.
  AB AB_instance;
  AB_instance.A_pure_method();
  AB_instance.AB_method(0);

  ABC ABC_instance;
  ABC_instance.A_pure_method();
  ABC_instance.AB_method(0);
  ABC_instance.AB::AB_method(0);
  ABC_instance.ABC::AB_method(0);

  // Pointers to instances.
  AB* AB_ptr = new AB;
  AB_ptr->A_pure_method();
  (*AB_ptr).A_pure_method();
  AB_ptr->AB_method(0);
  (*AB_ptr).AB_method(0);

  ABC* ABC_ptr = new ABC;
  ABC_ptr->A_pure_method();
  (*ABC_ptr).A_pure_method();
  ABC_ptr->AB_method(0);
  (*ABC_ptr).AB_method(0);
  ABC_ptr->AB::AB_method(0);
  (*ABC_ptr).AB::AB_method(0);
  ABC_ptr->ABC::AB_method(0);
  (*ABC_ptr).ABC::AB_method(0);

  AB* ABC_in_AB_ptr = ABC_ptr;
  ABC_in_AB_ptr->A_pure_method();
  (*ABC_in_AB_ptr).A_pure_method();
  ABC_in_AB_ptr->AB_method(0);
  (*ABC_in_AB_ptr).AB_method(0);
  ABC_in_AB_ptr->AB::AB_method(0);
  (*ABC_in_AB_ptr).AB::AB_method(0);

  // References to instances.
  AB& AB_instance_ref = AB_instance;
  AB_instance_ref.A_pure_method();
  AB_instance_ref.AB_method(0);

  ABC& ABC_instance_ref = ABC_instance;
  ABC_instance_ref.A_pure_method();
  ABC_instance_ref.AB_method(0);
  ABC_instance_ref.AB::AB_method(0);
  ABC_instance_ref.ABC::AB_method(0);

  AB& ABC_in_AB_ref = ABC_instance;
  ABC_in_AB_ref.A_pure_method();
  ABC_in_AB_ref.AB_method(0);
  ABC_in_AB_ref.AB::AB_method(0);

  return 0;
}
