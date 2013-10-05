#ifndef B_H
#define B_H

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
    virtual int BasePureVirtualMethod() { return 1; }

    int mB;

  private:
    void DoBPrivate();

    int mBPrivate;
};

#endif
