class clazz
{
public:
  static int static_method(int a) { return a; }
  int method(int a) { return a; }
  virtual int virtual_method(int a) { return a; }

  class nested_clazz
  {
  public:
    static int static_nested_method(int a) { return a; }
    int nested_method(int a) { return a; }
  };
};

int main(int argc, char** argv)
{
  // Top-level class.
  clazz clazz_instance;
  clazz* clazz_ptr = &clazz_instance;
  clazz& clazz_ref = clazz_instance;

  int (*static_method_ptr)(int) = clazz::static_method;
  static_method_ptr(0);

  int (clazz::*method_ptr)(int) = &clazz::method;
  (clazz_instance.*method_ptr)(0);
  (clazz_ptr->*method_ptr)(0);
  (clazz_ref.*method_ptr)(0);

  int (clazz::*virtual_method_ptr)(int) = &clazz::virtual_method;
  (clazz_instance.*virtual_method_ptr)(0);
  (clazz_ptr->*virtual_method_ptr)(0);
  (clazz_ref.*virtual_method_ptr)(0);

  // Nested class.
  clazz::nested_clazz nested_clazz_instance;
  clazz::nested_clazz* nested_clazz_ptr = &nested_clazz_instance;
  clazz::nested_clazz& nested_clazz_ref = nested_clazz_instance;

  int (*static_nested_method_ptr)(int) = clazz::nested_clazz::static_nested_method;
  static_nested_method_ptr(0);

  int (clazz::nested_clazz::*nested_method_ptr)(int) = &clazz::nested_clazz::nested_method;
  (nested_clazz_instance.*nested_method_ptr)(0);
  (nested_clazz_ptr->*nested_method_ptr)(0);
  (nested_clazz_ref.*nested_method_ptr)(0);

  return 0;
}
