/* Copyright (c) 2026  Software for Chemistry & Materials BV
 *
 * This file is part of of the Fortran Template Library.
 *
 * The Fortran Template Library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The Fortran Template Library is distributed in the hope that it will be
 * useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along
 * with the Fortran Template Library.  If not, see <http://www.gnu.org/licenses/>.
 */


/* File access through the C library for ftlString%ReadFile (see ftlString.F90).
 *
 * ReadFile does not use Fortran I/O, because a file can not be opened by several threads at the same time with a
 * Fortran OPEN: gfortran refuses to connect a file to a second unit while it is connected to another one, and with
 * ifort concurrent reads of one file through two units mixed up the data.
 */

#ifndef _WIN32
#define _POSIX_C_SOURCE 200809L /* for fileno and fstat */
#endif

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif


/* Opens a file for reading in binary mode. The path is given as npath bytes, not terminated by a NUL character. On
 * success, returns the FILE pointer, sets *error to 0, and sets *size to the size of the file in bytes if it is a
 * regular file, or to -1 if not (e.g. for a pipe). On failure, returns NULL and sets *error to the errno value (for a
 * directory to EISDIR on POSIX systems).
 *
 * On Windows, the path is tried as UTF-8 first. If it is not valid UTF-8, or no file was found under that name, it is
 * tried in the ANSI code page, as a Fortran OPEN interprets it.
 */
void* ftl_file_open(const char* path, size_t npath, int64_t* size, int* error)
{
   char* cpath;
   FILE* file = NULL;
   int err = 0;

   *size = -1;
   *error = 0;

   cpath = (char*)malloc(npath + 1);
   if (cpath == NULL) {
      *error = ENOMEM;
      return NULL;
   }
   memcpy(cpath, path, npath);
   cpath[npath] = '\0';
   if (strlen(cpath) != npath) { /* a NUL character in the path */
      free(cpath);
      *error = ENOENT;
      return NULL;
   }

#ifdef _WIN32
   {
      int tryAnsi = 1;
      int nwide = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, cpath, -1, NULL, 0);
      if (nwide > 0) {
         wchar_t* wpath = (wchar_t*)malloc((size_t)nwide*sizeof(wchar_t));
         if (wpath != NULL && MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, cpath, -1, wpath, nwide) == nwide) {
            file = _wfopen(wpath, L"rb");
            if (file == NULL) {
               err = errno;
               /* For any other error than "not found" (e.g. access denied), the file exists under the UTF-8 name, and
                * that error is the one to report. */
               if (err != ENOENT) tryAnsi = 0;
            }
         }
         free(wpath);
      }
      if (file == NULL && tryAnsi) {
         file = fopen(cpath, "rb");
         if (file == NULL) err = errno;
      }
   }
#else
   file = fopen(cpath, "rb");
   if (file == NULL) err = errno;
#endif
   free(cpath);
   if (file == NULL) {
      *error = (err != 0) ? err : ENOENT;
      return NULL;
   }

#ifdef _WIN32
   {
      struct _stat64 st;
      if (_fstat64(_fileno(file), &st) == 0 && (st.st_mode & _S_IFMT) == _S_IFREG) *size = (int64_t)st.st_size;
   }
#else
   {
      struct stat st;
      if (fstat(fileno(file), &st) == 0) {
         if (S_ISDIR(st.st_mode)) { /* fopen opens directories on POSIX systems, but they can not be read */
            fclose(file);
            *error = EISDIR;
            return NULL;
         }
         if (S_ISREG(st.st_mode)) *size = (int64_t)st.st_size;
      }
   }
#endif

   return file;
}


/* Reads up to n bytes from the file into buffer and returns the number of bytes read. This is less than n only at the
 * end of the file or after a read error, which ftl_file_close reports.
 */
size_t ftl_file_read(void* file, char* buffer, size_t n)
{
   if (n == 0) return 0;
   return fread(buffer, 1, n, (FILE*)file);
}


/* Closes the file. Returns 0, or the errno value if a read from the file failed.
 */
int ftl_file_close(void* file)
{
   int error = 0;
   if (ferror((FILE*)file)) error = (errno != 0) ? errno : EIO;
   fclose((FILE*)file);
   return error;
}
