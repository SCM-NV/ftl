#ifdef __GFORTRAN__
#define ASSERT(x) call assert(x,"x",__FILE__,__LINE__)
#else
#define ASSERT(x) call assert(x,#x,__FILE__,__LINE__)
#endif
