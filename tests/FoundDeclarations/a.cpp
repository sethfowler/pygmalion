#include "b.h"

using namespace ns;

#define CINST_MACRO cInstance
#define ADD(x, y) x + y

struct bStruct
{
  BClass b;
};

struct bPtrStruct
{
  BClass* b;
};

int main(int argc, char** argv)
{
  BClass bInstance;
  CClass cInstance;
  BClass* bPtr = new BClass;
  CClass* cPtr = new CClass;
  CClass* bInCPtr = bPtr;
  bStruct bS;
  bStruct* pBS = new bStruct;
  bPtrStruct bPS;
  bPS.b = bPtr;
  bPtrStruct* pBPS = new bPtrStruct;
  pBPS->b = bPtr;

  bInstance.CVirtualMethod(false);
  cInstance.CVirtualMethod(false);
  bPtr->CVirtualMethod(false);
  cPtr->CVirtualMethod(false);
  bInCPtr->CVirtualMethod(false);
  bPtr->BasePureVirtualMethod();
  bPtr->CClass::CVirtualMethod(false);
  bS.b.CVirtualMethod(false);
  pBS->b.CVirtualMethod(false);
  bPS.b->CVirtualMethod(false);
  pBPS->b->CVirtualMethod(false);

  return ADD(b, c) + bInstance.mB + CINST_MACRO.mC;
}
