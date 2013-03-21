#include <cstdio>

#include "b.h"

using std::printf;

int b = 0;

void bfun()
{ }

BClass::BClass()
  : mB(0)
  , mBPrivate(0)
{}

BClass::~BClass()
{ }

void BClass::DoB()
{
  printf("b\n");
}

bool BClass::CVirtualMethod(bool arg)
{
  DoB();
  return arg;
}

void BClass::DoBPrivate()
{
  printf("b private\n");
}
