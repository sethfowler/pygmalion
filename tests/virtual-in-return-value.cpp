#include "virtual.h"

ABDE func_ABDE_by_val()
{
  ABDE ABDE_instance;
  return ABDE_instance;
}

ABDE* func_ABDE_by_ptr()
{
  static ABDE ABDE_instance;
  return &ABDE_instance;
}

ABDE& func_ABDE_by_ref()
{
  static ABDE ABDE_instance;
  return ABDE_instance;
}

int main(int argc, char** argv)
{
  // Functions returning values or pointers.
  func_ABDE_by_val().A_pure_method();
  func_ABDE_by_val().AB_method(0);
  func_ABDE_by_val().AB::AB_method(0);
  func_ABDE_by_val().ABDE::AB_method(0);

  func_ABDE_by_ptr()->A_pure_method();
  func_ABDE_by_ptr()->AB_method(0);
  func_ABDE_by_ptr()->AB::AB_method(0);
  func_ABDE_by_ptr()->ABDE::AB_method(0);

  func_ABDE_by_ref().A_pure_method();
  func_ABDE_by_ref().AB_method(0);
  func_ABDE_by_ref().AB::AB_method(0);
  func_ABDE_by_ref().ABDE::AB_method(0);

  // Function pointers to functions returning values or pointers.
  ABDE (*func_ptr_ABDE_by_val)() = func_ABDE_by_val;
  (*func_ptr_ABDE_by_val)().A_pure_method();
  (*func_ptr_ABDE_by_val)().AB_method(0);
  (*func_ptr_ABDE_by_val)().AB::AB_method(0);
  (*func_ptr_ABDE_by_val)().ABDE::AB_method(0);

  ABDE* (*func_ptr_ABDE_by_ptr)() = func_ABDE_by_ptr;
  (*func_ptr_ABDE_by_ptr)()->A_pure_method();
  (*func_ptr_ABDE_by_ptr)()->AB_method(0);
  (*func_ptr_ABDE_by_ptr)()->AB::AB_method(0);
  (*func_ptr_ABDE_by_ptr)()->ABDE::AB_method(0);

  ABDE& (*func_ptr_ABDE_by_ref)() = func_ABDE_by_ref;
  (*func_ptr_ABDE_by_ref)().A_pure_method();
  (*func_ptr_ABDE_by_ref)().AB_method(0);
  (*func_ptr_ABDE_by_ref)().AB::AB_method(0);
  (*func_ptr_ABDE_by_ref)().ABDE::AB_method(0);

  // Function references to functions returning values or pointers.
  ABDE (&func_ref_ABDE_by_val)() = func_ABDE_by_val;
  func_ABDE_by_val().A_pure_method();
  func_ABDE_by_val().AB_method(0);
  func_ABDE_by_val().AB::AB_method(0);
  func_ABDE_by_val().ABDE::AB_method(0);

  ABDE* (&func_ref_ABDE_by_ptr)() = func_ABDE_by_ptr;
  func_ABDE_by_ptr()->A_pure_method();
  func_ABDE_by_ptr()->AB_method(0);
  func_ABDE_by_ptr()->AB::AB_method(0);
  func_ABDE_by_ptr()->ABDE::AB_method(0);

  ABDE& (&func_ref_ABDE_by_ref)() = func_ABDE_by_ref;
  func_ABDE_by_ref().A_pure_method();
  func_ABDE_by_ref().AB_method(0);
  func_ABDE_by_ref().AB::AB_method(0);
  func_ABDE_by_ref().ABDE::AB_method(0);

  return 0;
}
