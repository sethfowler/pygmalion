#pragma once
extern int c;

void cfun();

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
