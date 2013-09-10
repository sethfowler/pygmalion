class global_class
{
public:
  static int static_method(int a) { return a; }
  static const int static_field = 0;
  
  global_class() { }
  explicit global_class(int a) { }

  int method(int a) { return a; }
  int field;

  class nested_class
  {
  public:
    static const int nested_static_field = 0;
    int nested_method(int a) { return a; }
    int nested_field;
  };

  union nested_union
  {
    int nested_union_val_int;
    char nested_union_val_char;
  };

  enum nested_enum
  {
    nested_enum_val
  };
};

global_class global_instance;

int main(int argc, char** argv)
{
  class local_class
  {
  public:
    int local_method(int a) { return a; }
    int local_field;
  };

  class
  {
  public:
    int anonymous_method(int a) { return a; }
    int anonymous_field;
  } anonymous_instance;

  global_class local_instance;
  global_class local_instance_with_constructor(0);
  local_class local_class_instance;
  global_class::nested_class nested_instance;
  global_class::nested_union nested_union_var;
  global_class::nested_enum nested_enum_var;

  int val = global_class::static_method(0) +
            global_class::static_field +
            global_class::nested_class::nested_static_field +
            global_class::nested_enum_val;

  return global_instance.field +
         global_instance.method(0) +
         local_instance.field +
         local_instance.method(0) +
         nested_instance.nested_field +
         nested_instance.nested_method(0) +
         nested_union_var.nested_union_val_int +
         nested_union_var.nested_union_val_char +
         local_class_instance.local_field +
         local_class_instance.local_method(0) +
         anonymous_instance.anonymous_field +
         anonymous_instance.anonymous_method(0);
}
