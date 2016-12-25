#ifndef FTL_MACROS_INCLUDED
#define FTL_MACROS_INCLUDED

#ifdef __GFORTRAN__
#define PASTE(a) a
#define CAT(a,b) PASTE(a)b
#define CAT3(a,b,c) CAT(a,b)c
#define CAT4(a,b,c) CAT3(a,b,c)d
#else
#define PCAT(a,b,c,d) a ## b ## c ## d
#define CAT(a,b) PCAT(a,b,,)
#define CAT3(a,b,c) PCAT(a,b,c,)
#define CAT4(a,b,c,d) PCAT(a,b,c,d)
#endif

#endif
