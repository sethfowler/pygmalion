#include <cstdio>

#include "c.h"

namespace ns {

int c = 0;

void cfun()
{ }

CClass::CClass()
  : mC(0)
  , mCPrivate(0)
{}

CClass::~CClass()
{ }

void CClass::DoC()
{
  printf("c\n");
}

bool CClass::CVirtualMethod(bool arg)
{
  DoC();
  return arg;
}

void CClass::DoCPrivate()
{
  printf("c private\n");
}

} // namespace ns
