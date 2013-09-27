#include "virtual.h"

ABC func_ABC_by_val()
{
  ABC ABC_instance;
  return ABC_instance;
}

ABC& func_ABC_by_ref()
{
  static ABC ABC_instance;
  return ABC_instance;
}

ABC* func_ABC_by_ptr()
{
  static ABC ABC_instance;
  return &ABC_instance;
}

int main(int argc, char** argv)
{
  // Functions returning values or pointers.
  func_ABC_by_val().A_pure_method();
  func_ABC_by_val().AB_method(0);
  func_ABC_by_val().AB::AB_method(0);
  func_ABC_by_val().ABC::AB_method(0);

  func_ABC_by_ref().A_pure_method();
  func_ABC_by_ref().AB_method(0);
  func_ABC_by_ref().AB::AB_method(0);
  func_ABC_by_ref().ABC::AB_method(0);

  func_ABC_by_ptr()->A_pure_method();
  func_ABC_by_ptr()->AB_method(0);
  func_ABC_by_ptr()->AB::AB_method(0);
  func_ABC_by_ptr()->ABC::AB_method(0);

  // Function pointers to functions returning values or pointers.
  ABC (*func_ptr_ABC_by_val)() = func_ABC_by_val;
  (*func_ptr_ABC_by_val)().A_pure_method();
  (*func_ptr_ABC_by_val)().AB_method(0);
  (*func_ptr_ABC_by_val)().AB::AB_method(0);
  (*func_ptr_ABC_by_val)().ABC::AB_method(0);

  ABC& (*func_ptr_ABC_by_ref)() = func_ABC_by_ref;
  (*func_ptr_ABC_by_ref)().A_pure_method();
  (*func_ptr_ABC_by_ref)().AB_method(0);
  (*func_ptr_ABC_by_ref)().AB::AB_method(0);
  (*func_ptr_ABC_by_ref)().ABC::AB_method(0);

  ABC* (*func_ptr_ABC_by_ptr)() = func_ABC_by_ptr;
  (*func_ptr_ABC_by_ptr)()->A_pure_method();
  (*func_ptr_ABC_by_ptr)()->AB_method(0);
  (*func_ptr_ABC_by_ptr)()->AB::AB_method(0);
  (*func_ptr_ABC_by_ptr)()->ABC::AB_method(0);

  return 0;
}
