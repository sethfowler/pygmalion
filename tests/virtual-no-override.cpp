#include "virtual.h"

int main(int argc, char** argv)
{
  // Classes which don't override a virtual method defined in an ancestor class.
  ABD ABD_instance;
  ABD_instance.A_pure_method();
  ABD_instance.AB_method(0);
  ABD_instance.AB::AB_method(0);
  ABD_instance.ABD::AB_method(0);

  ABD* ABD_ptr;
  ABD_ptr->A_pure_method();
  ABD_ptr->AB_method(0);
  ABD_ptr->AB::AB_method(0);
  ABD_ptr->ABD::AB_method(0);

  ABD& ABD_ref = ABD_instance;
  ABD_ref.A_pure_method();
  ABD_ref.AB_method(0);
  ABD_ref.AB::AB_method(0);
  ABD_ref.ABD::AB_method(0);

  // Classes which have an ancestor which didn't override a method.
  ABDE ABDE_instance;
  ABDE_instance.A_pure_method();
  ABDE_instance.AB_method(0);
  ABDE_instance.AB::AB_method(0);
  ABDE_instance.ABD::AB_method(0);
  ABDE_instance.ABDE::AB_method(0);

  ABDE* ABDE_ptr;
  ABDE_ptr->A_pure_method();
  ABDE_ptr->AB_method(0);
  ABDE_ptr->AB::AB_method(0);
  ABDE_ptr->ABD::AB_method(0);
  ABDE_ptr->ABDE::AB_method(0);

  ABDE& ABDE_ref = ABDE_instance;
  ABDE_ref.A_pure_method();
  ABDE_ref.AB_method(0);
  ABDE_ref.AB::AB_method(0);
  ABDE_ref.ABD::AB_method(0);
  ABDE_ref.ABDE::AB_method(0);

  return 0;
}
