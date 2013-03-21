#include "b.h"

using namespace ns;

#define CINST_MACRO cInstance
#define ADD(x, y) x + y

int main(int argc, char** argv)
{
  BClass bInstance;
  CClass cInstance;
  BClass* bPtr = new BClass;
  CClass* cPtr = new CClass;
  CClass* bInCPtr = bPtr;

  bInstance.CVirtualMethod(false);
  cInstance.CVirtualMethod(false);
  bPtr->CVirtualMethod(false);
  cPtr->CVirtualMethod(false);
  bInCPtr->CVirtualMethod(false);

  return ADD(b, c) + bInstance.mB + CINST_MACRO.mC;
}
