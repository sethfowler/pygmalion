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
  (*child_ptr).base_pure_method(0);
  child_ptr->child_method(0);
  (*child_ptr).child_method(0);

  grandchild_class* grandchild_ptr = new grandchild_class;
  grandchild_ptr->base_pure_method();
  (*grandchild_ptr).base_pure_method(0);
  grandchild_ptr->child_method(0);
  (*grandchild_ptr).child_method(0);
  grandchild_ptr->child_class::child_method(0);
  (*grandchild_ptr).child_class::child_method(0);
  grandchild_ptr->grandchild_class::child_method(0);
  (*grandchild_ptr).grandchild_class::child_method(0);

  child_class* grandchild_in_child_ptr = grandchild_ptr;
  grandchild_in_child_ptr->base_pure_method();
  (*grandchild_in_child_ptr).base_pure_method(0);
  grandchild_in_child_ptr->child_method(0);
  (*grandchild_in_child_ptr).child_method(0);
  grandchild_in_child_ptr->child_class::child_method(0);
  (*grandchild_in_child_ptr).child_class::child_method(0);
  grandchild_in_child_ptr->grandchild_class::child_method(0);
  (*grandchild_in_child_ptr).grandchild_class::child_method(0);

  // References to instances.
  child_class& child_instance_ref = child_instance;
  grandchild_class& grandchild_instance_ref = grandchild_instance;
  child_class& grandchild_in_child_ref = grandchild_instance;

  // Structs containing instance values.
  grandchild_struct grandchild_struct_instance;
  grandchild_struct* grandchild_struct_ptr = new grandchild_struct;

  // Structs containing pointers to instances.
  grandchild_ptr_struct grandchild_ptr_struct_instance { grandchild_ptr };
  grandchild_ptr_struct* grandchild_ptr_struct_ptr = new grandchild_ptr_struct { grandchild_ptr };

  // Function pointers to functions returning values or pointers.
  grandchild_class (*func_ptr_grandchild_by_val)() = func_grandchild_by_val;
  grandchild_class& (*func_ptr_grandchild_by_ref)() = func_grandchild_by_ref;

  // Test cases. XXX(seth) Move up above.
  grandchild_struct_instance.var.child_method(0);
  grandchild_struct_ptr->var.child_method(0);
  grandchild_ptr_struct_instance.ptr->child_method(0);
  grandchild_ptr_struct_ptr->ptr->child_method(0);
  func_grandchild_by_ref().child_method(0);
  (*func_ptr_grandchild_by_ref)().child_method(0);
  func_grandchild_by_val().child_method(0);

  return 0;
}
