struct clazz { typedef int clazz_typedef; };
namespace ns { typedef int ns_typedef; }
typedef int global_typedef;
typedef clazz::clazz_typedef (*func_typedef)(ns::ns_typedef a, global_typedef b);

clazz::clazz_typedef foo(ns::ns_typedef a, global_typedef b) { return a + b; }

template <typename T> struct templat { typedef T templat_typedef; };

int main(int argc, char** argv)
{
  typedef templat<clazz::clazz_typedef>::templat_typedef local_typedef;

  func_typedef foo_ptr = foo;
  return local_typedef(foo_ptr(1, 1));
};
