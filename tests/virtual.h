class A
{
  public: virtual int A_pure_method() = 0;
};

int A::A_pure_method() { return 0; }

class AB : public A
{
  public: virtual int A_pure_method();
  public: virtual int AB_method(int a);
};

int AB::A_pure_method() { return 1; }
int AB::AB_method(int a) { return a; }

class ABC : public AB
{
  public: virtual int A_pure_method() { return 2; }
  public: virtual int AB_method(int a) { return a + 1; }
};

class ABD : public AB
{
};

class ABDE : public ABD
{
  public: virtual int A_pure_method() { return 3; }
  public: virtual int AB_method(int a) { return a + 2; }
};

class ABDEF : public ABDE
{
  public: virtual int A_pure_method();
  public: virtual int AB_method(int a);
};

int ABDEF::A_pure_method() { return 5; }
int ABDEF::AB_method(int a) { return a + 3; }
