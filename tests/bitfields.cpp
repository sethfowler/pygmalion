enum enum_type { aleph, bet, gimel };

struct bitfield_struct
{
  bool bitfield_bool : 1;
  int bitfield_int   : 3;
  unsigned char      : 0;
  unsigned char bitfield_unsigned_char : 4;
  unsigned char nonbitfield;
  enum_type bitfield_enum : 2;
  enum { alpha, beta } bitfield_anonymous_enum : 1;
};

int main(int argc, char** argv)
{
  bitfield_struct bitfield_instance;

  return bitfield_instance.bitfield_bool +
         bitfield_instance.bitfield_int +
         bitfield_instance.bitfield_unsigned_char +
         bitfield_instance.nonbitfield +
         bitfield_instance.bitfield_enum +
         bitfield_instance.bitfield_anonymous_enum;
}
