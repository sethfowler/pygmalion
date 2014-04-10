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

struct ABDE_container
{
  ABDE method_ABDE_by_val() { return ABDE_member; }
  ABDE* method_ABDE_by_ptr() { return &ABDE_member; }
  ABDE& method_ABDE_by_ref() { return ABDE_member; }

  ABDE ABDE_member;
};

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
  func_ptr_ABDE_by_val().A_pure_method();
  func_ptr_ABDE_by_val().AB_method(0);
  func_ptr_ABDE_by_val().AB::AB_method(0);
  func_ptr_ABDE_by_val().ABDE::AB_method(0);

  ABDE* (*func_ptr_ABDE_by_ptr)() = func_ABDE_by_ptr;
  func_ptr_ABDE_by_ptr()->A_pure_method();
  func_ptr_ABDE_by_ptr()->AB_method(0);
  func_ptr_ABDE_by_ptr()->AB::AB_method(0);
  func_ptr_ABDE_by_ptr()->ABDE::AB_method(0);

  ABDE& (*func_ptr_ABDE_by_ref)() = func_ABDE_by_ref;
  func_ptr_ABDE_by_ref().A_pure_method();
  func_ptr_ABDE_by_ref().AB_method(0);
  func_ptr_ABDE_by_ref().AB::AB_method(0);
  func_ptr_ABDE_by_ref().ABDE::AB_method(0);

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

  // Methods on instances returning values or pointers.
  ABDE_container ABDE_container_instance;
  ABDE_container_instance.method_ABDE_by_val().A_pure_method();
  ABDE_container_instance.method_ABDE_by_val().AB_method(0);
  ABDE_container_instance.method_ABDE_by_val().AB::AB_method(0);
  ABDE_container_instance.method_ABDE_by_val().ABDE::AB_method(0);

  ABDE_container_instance.method_ABDE_by_ptr()->A_pure_method();
  ABDE_container_instance.method_ABDE_by_ptr()->AB_method(0);
  ABDE_container_instance.method_ABDE_by_ptr()->AB::AB_method(0);
  ABDE_container_instance.method_ABDE_by_ptr()->ABDE::AB_method(0);

  ABDE_container_instance.method_ABDE_by_ref().A_pure_method();
  ABDE_container_instance.method_ABDE_by_ref().AB_method(0);
  ABDE_container_instance.method_ABDE_by_ref().AB::AB_method(0);
  ABDE_container_instance.method_ABDE_by_ref().ABDE::AB_method(0);

  // Methods on pointers to instances returning values or pointers.
  ABDE_container* ABDE_container_ptr = &ABDE_container_instance;
  ABDE_container_ptr->method_ABDE_by_val().A_pure_method();
  ABDE_container_ptr->method_ABDE_by_val().AB_method(0);
  ABDE_container_ptr->method_ABDE_by_val().AB::AB_method(0);
  ABDE_container_ptr->method_ABDE_by_val().ABDE::AB_method(0);

  ABDE_container_ptr->method_ABDE_by_ptr()->A_pure_method();
  ABDE_container_ptr->method_ABDE_by_ptr()->AB_method(0);
  ABDE_container_ptr->method_ABDE_by_ptr()->AB::AB_method(0);
  ABDE_container_ptr->method_ABDE_by_ptr()->ABDE::AB_method(0);

  ABDE_container_ptr->method_ABDE_by_ref().A_pure_method();
  ABDE_container_ptr->method_ABDE_by_ref().AB_method(0);
  ABDE_container_ptr->method_ABDE_by_ref().AB::AB_method(0);
  ABDE_container_ptr->method_ABDE_by_ref().ABDE::AB_method(0);

  // Methods on references to instances returning values or pointers.
  ABDE_container& ABDE_container_ref = ABDE_container_instance;
  ABDE_container_ref.method_ABDE_by_val().A_pure_method();
  ABDE_container_ref.method_ABDE_by_val().AB_method(0);
  ABDE_container_ref.method_ABDE_by_val().AB::AB_method(0);
  ABDE_container_ref.method_ABDE_by_val().ABDE::AB_method(0);

  ABDE_container_ref.method_ABDE_by_ptr()->A_pure_method();
  ABDE_container_ref.method_ABDE_by_ptr()->AB_method(0);
  ABDE_container_ref.method_ABDE_by_ptr()->AB::AB_method(0);
  ABDE_container_ref.method_ABDE_by_ptr()->ABDE::AB_method(0);

  ABDE_container_ref.method_ABDE_by_ref().A_pure_method();
  ABDE_container_ref.method_ABDE_by_ref().AB_method(0);
  ABDE_container_ref.method_ABDE_by_ref().AB::AB_method(0);
  ABDE_container_ref.method_ABDE_by_ref().ABDE::AB_method(0);

  // Invocations of method pointers returning values or pointers.
  ABDE (ABDE_container::*method_ptr_ABDE_by_val)() = &ABDE_container::method_ABDE_by_val;
  (ABDE_container_instance.*method_ptr_ABDE_by_val)().A_pure_method();
  (ABDE_container_instance.*method_ptr_ABDE_by_val)().AB_method(0);
  (ABDE_container_instance.*method_ptr_ABDE_by_val)().AB::AB_method(0);
  (ABDE_container_instance.*method_ptr_ABDE_by_val)().ABDE::AB_method(0);

  ABDE* (ABDE_container::*method_ptr_ABDE_by_ptr)() = &ABDE_container::method_ABDE_by_ptr;
  (ABDE_container_instance.*method_ptr_ABDE_by_ptr)()->A_pure_method();
  (ABDE_container_instance.*method_ptr_ABDE_by_ptr)()->AB_method(0);
  (ABDE_container_instance.*method_ptr_ABDE_by_ptr)()->AB::AB_method(0);
  (ABDE_container_instance.*method_ptr_ABDE_by_ptr)()->ABDE::AB_method(0);

  ABDE& (ABDE_container::*method_ref_ABDE_by_ref)() = &ABDE_container::method_ABDE_by_ref;
  (ABDE_container_instance.*method_ref_ABDE_by_ref)().A_pure_method();
  (ABDE_container_instance.*method_ref_ABDE_by_ref)().AB_method(0);
  (ABDE_container_instance.*method_ref_ABDE_by_ref)().AB::AB_method(0);
  (ABDE_container_instance.*method_ref_ABDE_by_ref)().ABDE::AB_method(0);

  return 0;
}
