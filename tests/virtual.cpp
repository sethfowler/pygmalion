class base_class
{
  public: virtual int base_pure_method() = 0;
};

int base_class::base_pure_method() { return 0; }

class child_class : public base_class
{
  public: virtual int base_pure_method() { return 1; }
  public: virtual int child_method(int a) { return a; }
};

class grandchild_class : public child_class
{
  public: virtual int base_pure_method() { return 2; }
  public: virtual int child_method(int a) { return a + 1; }
};

class grandchild_no_override_class : public child_class
{
};

class greatgrandchild_class : public grandchild_no_override_class
{
  public: virtual int base_pure_method() { return 3; }
  public: virtual int child_method(int a) { return a + 2; }
};

class greatgreatgrandchild_class : public greatgrandchild_class
{
  public: virtual int base_pure_method() { return 5; }
  public: virtual int child_method(int a) { return a + 3; }
};

struct grandchild_struct { grandchild_class var; };
struct grandchild_ptr_struct { grandchild_class* ptr; };

grandchild_class func_grandchild_by_val()
{
  grandchild_class grandchild_instance;
  return grandchild_instance;
}

grandchild_class& func_grandchild_by_ref()
{
  static grandchild_class grandchild_instance;
  return grandchild_instance;
}

grandchild_class* func_grandchild_by_ptr()
{
  static grandchild_class grandchild_instance;
  return &grandchild_instance;
}

int main(int argc, char** argv)
{
  // Instance values.
  child_class child_instance;
  child_instance.base_pure_method();
  child_instance.child_method(0);

  grandchild_class grandchild_instance;
  grandchild_instance.base_pure_method();
  grandchild_instance.child_method(0);
  grandchild_instance.child_class::child_method(0);
  grandchild_instance.grandchild_class::child_method(0);

  // Pointers to instances.
  child_class* child_ptr = new child_class;
  child_ptr->base_pure_method();
  (*child_ptr).base_pure_method();
  child_ptr->child_method(0);
  (*child_ptr).child_method(0);

  grandchild_class* grandchild_ptr = new grandchild_class;
  grandchild_ptr->base_pure_method();
  (*grandchild_ptr).base_pure_method();
  grandchild_ptr->child_method(0);
  (*grandchild_ptr).child_method(0);
  grandchild_ptr->child_class::child_method(0);
  (*grandchild_ptr).child_class::child_method(0);
  grandchild_ptr->grandchild_class::child_method(0);
  (*grandchild_ptr).grandchild_class::child_method(0);

  child_class* grandchild_in_child_ptr = grandchild_ptr;
  grandchild_in_child_ptr->base_pure_method();
  (*grandchild_in_child_ptr).base_pure_method();
  grandchild_in_child_ptr->child_method(0);
  (*grandchild_in_child_ptr).child_method(0);
  grandchild_in_child_ptr->child_class::child_method(0);
  (*grandchild_in_child_ptr).child_class::child_method(0);

  // References to instances.
  child_class& child_instance_ref = child_instance;
  child_instance_ref.base_pure_method();
  child_instance_ref.child_method(0);

  grandchild_class& grandchild_instance_ref = grandchild_instance;
  grandchild_instance_ref.base_pure_method();
  grandchild_instance_ref.child_method(0);
  grandchild_instance_ref.child_class::child_method(0);
  grandchild_instance_ref.grandchild_class::child_method(0);

  child_class& grandchild_in_child_ref = grandchild_instance;
  grandchild_in_child_ref.base_pure_method();
  grandchild_in_child_ref.child_method(0);
  grandchild_in_child_ref.child_class::child_method(0);

  // Structs containing instance values.
  grandchild_struct grandchild_struct_instance;
  grandchild_struct_instance.var.base_pure_method();
  grandchild_struct_instance.var.child_method(0);
  grandchild_struct_instance.var.child_class::child_method(0);
  grandchild_struct_instance.var.grandchild_class::child_method(0);

  grandchild_struct* grandchild_struct_ptr = new grandchild_struct;
  grandchild_struct_ptr->var.base_pure_method();
  grandchild_struct_ptr->var.child_method(0);
  grandchild_struct_ptr->var.child_class::child_method(0);
  grandchild_struct_ptr->var.grandchild_class::child_method(0);

  // Structs containing pointers to instances.
  grandchild_ptr_struct grandchild_ptr_struct_instance { grandchild_ptr };
  grandchild_ptr_struct_instance.ptr->base_pure_method();
  grandchild_ptr_struct_instance.ptr->child_method(0);
  grandchild_ptr_struct_instance.ptr->child_class::child_method(0);
  grandchild_ptr_struct_instance.ptr->grandchild_class::child_method(0);

  grandchild_ptr_struct* grandchild_ptr_struct_ptr = new grandchild_ptr_struct { grandchild_ptr };
  grandchild_ptr_struct_ptr->ptr->base_pure_method();
  grandchild_ptr_struct_ptr->ptr->child_method(0);
  grandchild_ptr_struct_ptr->ptr->child_class::child_method(0);
  grandchild_ptr_struct_ptr->ptr->grandchild_class::child_method(0);

  // Functions returning values or pointers.
  func_grandchild_by_val().base_pure_method();
  func_grandchild_by_val().child_method(0);
  func_grandchild_by_val().child_class::child_method(0);
  func_grandchild_by_val().grandchild_class::child_method(0);

  func_grandchild_by_ref().base_pure_method();
  func_grandchild_by_ref().child_method(0);
  func_grandchild_by_ref().child_class::child_method(0);
  func_grandchild_by_ref().grandchild_class::child_method(0);

  func_grandchild_by_ptr()->base_pure_method();
  func_grandchild_by_ptr()->child_method(0);
  func_grandchild_by_ptr()->child_class::child_method(0);
  func_grandchild_by_ptr()->grandchild_class::child_method(0);

  // Function pointers to functions returning values or pointers.
  grandchild_class (*func_ptr_grandchild_by_val)() = func_grandchild_by_val;
  (*func_ptr_grandchild_by_val)().base_pure_method();
  (*func_ptr_grandchild_by_val)().child_method(0);
  (*func_ptr_grandchild_by_val)().child_class::child_method(0);
  (*func_ptr_grandchild_by_val)().grandchild_class::child_method(0);

  grandchild_class& (*func_ptr_grandchild_by_ref)() = func_grandchild_by_ref;
  (*func_ptr_grandchild_by_ref)().base_pure_method();
  (*func_ptr_grandchild_by_ref)().child_method(0);
  (*func_ptr_grandchild_by_ref)().child_class::child_method(0);
  (*func_ptr_grandchild_by_ref)().grandchild_class::child_method(0);

  grandchild_class* (*func_ptr_grandchild_by_ptr)() = func_grandchild_by_ptr;
  (*func_ptr_grandchild_by_ptr)()->base_pure_method();
  (*func_ptr_grandchild_by_ptr)()->child_method(0);
  (*func_ptr_grandchild_by_ptr)()->child_class::child_method(0);
  (*func_ptr_grandchild_by_ptr)()->grandchild_class::child_method(0);

  // Classes which don't override a virtual method defined in an ancestor class.
  grandchild_no_override_class grandchild_no_override_instance;
  grandchild_no_override_instance.base_pure_method();
  grandchild_no_override_instance.child_method(0);
  grandchild_no_override_instance.child_class::child_method(0);
  grandchild_no_override_instance.grandchild_no_override_class::child_method(0);

  grandchild_no_override_class* grandchild_no_override_ptr;
  grandchild_no_override_ptr->base_pure_method();
  grandchild_no_override_ptr->child_method(0);
  grandchild_no_override_ptr->child_class::child_method(0);
  grandchild_no_override_ptr->grandchild_no_override_class::child_method(0);

  grandchild_no_override_class& grandchild_no_override_ref = grandchild_no_override_instance;
  grandchild_no_override_ref.base_pure_method();
  grandchild_no_override_ref.child_method(0);
  grandchild_no_override_ref.child_class::child_method(0);
  grandchild_no_override_ref.grandchild_no_override_class::child_method(0);

  // Classes which have a direct ancestor which didn't override a method.
  greatgrandchild_class greatgrandchild_instance;
  greatgrandchild_instance.base_pure_method();
  greatgrandchild_instance.child_method(0);
  greatgrandchild_instance.child_class::child_method(0);
  greatgrandchild_instance.grandchild_no_override_class::child_method(0);
  greatgrandchild_instance.greatgrandchild_class::child_method(0);

  greatgrandchild_class* greatgrandchild_ptr;
  greatgrandchild_ptr->base_pure_method();
  greatgrandchild_ptr->child_method(0);
  greatgrandchild_ptr->child_class::child_method(0);
  greatgrandchild_ptr->grandchild_no_override_class::child_method(0);
  greatgrandchild_ptr->greatgrandchild_class::child_method(0);

  greatgrandchild_class& greatgrandchild_ref = greatgrandchild_instance;
  greatgrandchild_ref.base_pure_method();
  greatgrandchild_ref.child_method(0);
  greatgrandchild_ref.child_class::child_method(0);
  greatgrandchild_ref.grandchild_no_override_class::child_method(0);
  greatgrandchild_ref.greatgrandchild_class::child_method(0);

  // Classes which have an indirect ancestor which didn't override a method.
  greatgreatgrandchild_class greatgreatgrandchild_instance;
  greatgreatgrandchild_instance.base_pure_method();
  greatgreatgrandchild_instance.child_method(0);
  greatgreatgrandchild_instance.child_class::child_method(0);
  greatgreatgrandchild_instance.grandchild_no_override_class::child_method(0);
  greatgreatgrandchild_instance.greatgrandchild_class::child_method(0);
  greatgreatgrandchild_instance.greatgreatgrandchild_class::child_method(0);

  greatgreatgrandchild_class* greatgreatgrandchild_ptr;
  greatgreatgrandchild_ptr->base_pure_method();
  greatgreatgrandchild_ptr->child_method(0);
  greatgreatgrandchild_ptr->child_class::child_method(0);
  greatgreatgrandchild_ptr->grandchild_no_override_class::child_method(0);
  greatgreatgrandchild_ptr->greatgrandchild_class::child_method(0);
  greatgreatgrandchild_ptr->greatgreatgrandchild_class::child_method(0);

  greatgreatgrandchild_class& greatgreatgrandchild_ref = greatgreatgrandchild_instance;
  greatgreatgrandchild_ref.base_pure_method();
  greatgreatgrandchild_ref.child_method(0);
  greatgreatgrandchild_ref.child_class::child_method(0);
  greatgreatgrandchild_ref.grandchild_no_override_class::child_method(0);
  greatgreatgrandchild_ref.greatgrandchild_class::child_method(0);
  greatgreatgrandchild_ref.greatgreatgrandchild_class::child_method(0);

  return 0;
}
