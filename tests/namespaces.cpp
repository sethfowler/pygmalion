int var = 0;

namespace ns_a { int var_a = 0; }

namespace ns_b
{
  int var_b = 0;
  namespace ns_c { int var_c = 0; }
  using ns_c::var_c;
}

using ns_b::var_b;

namespace
{
  int var_anon = 0;
  namespace ns_d { int var_d = 0; }
}

namespace ns_e = ns_b;

namespace ns_f { int var_f = 0; }

using namespace ns_f;

int main(int argc, char** argv)
{
  using ns_a::var_a;
  using ns_b::ns_c::var_c;
  using ns_d::var_d;

  return ns_a::var_a +
         ns_b::var_b +
         ns_b::ns_c::var_c +
         ns_b::var_c +
         var_b +
         var_anon +
         ns_d::var_d +
         ns_e::var_b +
         ns_e::ns_c::var_c +
         ns_e::var_c +
         ns_f::var_f +
         var_f +
         var_a +
         var_c +
         var_d;
}

