#include "b.h"

using namespace ns;

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

  return b + c + bInstance.mB + cInstance.mC;
}
