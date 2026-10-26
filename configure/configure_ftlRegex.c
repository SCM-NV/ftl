#include <stdio.h>

#ifdef USE_PCRE
#include <pcreposix.h>
#else
#include <regex.h>
#endif

int main() {

   // size of the regex_t in C
   printf("integer(C_int), parameter :: sizeof_C_regex_t = %zu_C_int\n", sizeof(regex_t));

   // flags for regcomp()
   printf("integer(C_int), parameter :: REG_EXTENDED = %d_C_int\n", REG_EXTENDED);
   printf("integer(C_int), parameter :: REG_ICASE    = %d_C_int\n", REG_ICASE   );
   printf("integer(C_int), parameter :: REG_NEWLINE  = %d_C_int\n", REG_NEWLINE );
   printf("integer(C_int), parameter :: REG_NOSUB    = %d_C_int\n", REG_NOSUB   );

   // flags for regexec()
   printf("integer(C_int), parameter :: REG_NOTBOL = %d_C_int\n", REG_NOTBOL);
   printf("integer(C_int), parameter :: REG_NOTEOL = %d_C_int\n", REG_NOTEOL);

   // error codes()
   printf("integer(C_int), parameter :: REG_NOMATCH = %d_C_int\n", REG_NOMATCH);

}
