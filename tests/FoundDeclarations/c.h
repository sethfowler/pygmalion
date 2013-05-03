#ifndef C_H
#define C_H

namespace ns {

extern int c;

void cfun();

enum CEnum
{
  CEnum_A,
  CEnum_B
};

class BaseInterface
{
  public:
    virtual int BasePureVirtualMethod() = 0;
};

class CClass : public BaseInterface
{
  public:
    CClass();
    ~CClass();

    void DoC();
    virtual bool CVirtualMethod(bool arg);
    virtual int BasePureVirtualMethod() { return 0; }

    int mC;

  private:
    void DoCPrivate();

    int mCPrivate;
};

} // namespace ns

#endif
