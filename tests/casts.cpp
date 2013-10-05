typedef int int_typedef_type;
struct class_type { };
struct subclass_type : public class_type { };

int main(int argc, char** argv)
{
  int_typedef_type int_typedef_value = 0;
  int int_value = 0;

  int_typedef_value = static_cast<int_typedef_type>(int_value);
  int_typedef_value = reinterpret_cast<int_typedef_type>(int_value);
  int_typedef_value = (int_typedef_type)int_value;
  int_typedef_value = int_typedef_type(int_value);

  class_type class_instance;
  subclass_type subclass_instance;

  class_instance = static_cast<class_type>(subclass_instance);
  class_instance = (class_type)subclass_instance;
  class_instance = class_type(subclass_instance);

  class_type* class_ptr = &class_instance;
  const class_type* const_class_ptr = &class_instance;
  subclass_type* subclass_ptr = &subclass_instance;

  class_ptr = static_cast<class_type*>(subclass_ptr);
  class_ptr = dynamic_cast<class_type*>(subclass_ptr);
  class_ptr = reinterpret_cast<class_type*>(subclass_ptr);
  class_ptr = const_cast<class_type*>(const_class_ptr);
  class_ptr = (class_type*)subclass_ptr;
  class_ptr = (class_type*)(subclass_ptr);

  return 0;
}
