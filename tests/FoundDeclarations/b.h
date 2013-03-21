#pragma once
#include "c.h"
extern int b;

void bfun();

class BClass : public ns::CClass
{
  public:
    BClass();
    ~BClass();

    void DoB();
    virtual bool CVirtualMethod(bool arg);

    int mB;

  private:
    void DoBPrivate();

    int mBPrivate;
};
