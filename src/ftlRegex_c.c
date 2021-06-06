#ifdef USE_PCRE
#include <pcreposix.h>
#else
#include <regex.h>
#endif

extern "C" {
  int c_regcomp(regex_t* preg, const char* pattern, int cflags) {
    return regcomp(preg, pattern, cflags);
  }

  void c_regfree(regex_t* preg) {
    regfree(preg);
  }

  size_t c_regerror(int errcode, const regex_t* preg, char* errbuf, size_t errbuf_size) {
    return regerror(errcode, preg, errbuf, errbuf_size);
  }

  int c_regexec(const regex_t* preg, const char* string, size_t nmatch, regmatch_t pmatch[], int eflags) {
    return regexec(preg, string, nmatch, pmatch, eflags);
  }
}

