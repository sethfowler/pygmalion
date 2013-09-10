enum global_enum { global_enum_val };
enum { global_anonymous_enum_val } global_anonymous_enum_var;
enum class global_enum_class : int { global_enum_class_val };

int main(int argc, char** argv)
{
  enum local_enum { local_enum_val };
  enum { local_anonymous_enum_val } local_anonymous_enum_var;
  enum class local_enum_class : int { local_enum_class_val };

  global_enum global_enum_var;
  global_enum_class global_enum_class_var;
  local_enum local_enum_var;
  local_enum_class local_enum_class_var;
  int val = global_enum_var +
            global_anonymous_enum_var +
            static_cast<int>(global_enum_class_var) +
            local_enum_var +
            local_anonymous_enum_var +
            static_cast<int>(local_enum_class_var);

  return global_enum_val +
         global_anonymous_enum_val +
         static_cast<int>(global_enum_class::global_enum_class_val) +
         local_enum_val +
         local_anonymous_enum_val +
         static_cast<int>(local_enum_class::local_enum_class_val);
}
