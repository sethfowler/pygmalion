#pragma once

namespace ns {

extern int c;

void cfun();

enum CEnum
{
  CEnum_A,
  CEnum_B
};

class CClass
{
  public:
    CClass();
    ~CClass();

    void DoC();
    virtual bool CVirtualMethod(bool arg);

    int mC;

  private:
    void DoCPrivate();

    int mCPrivate;
};

} // namespace ns
