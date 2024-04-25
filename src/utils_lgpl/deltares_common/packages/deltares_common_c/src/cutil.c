//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id$
// $HeadURL$
//------------------------------------------------------------------------------*/
//  Delft3D - C Utilities
//  Internal (legacy) utility functions
//
//  Irv.Elshoff@deltares.nl
//  27 apr 11
//------------------------------------------------------------------------------*/

#include "util_mf.h"
#if !defined(WIN32)
#include <sys/time.h>
#include <sys/resource.h>
#include <inttypes.h>
#endif

#if defined(_WIN32)
#define FILE_READ _read
#define FILE_WRITE _write
#define FILE_SEEK _fseeki64
#define FILE_TELL _ftelli64
#elif defined(linux)
#define FILE_READ read
#define FILE_WRITE write
#define FILE_SEEK fseeko
#define FILE_TELL ftello
#else
#define FILE_READ FILE_READ_not_defined
#define FILE_WRITE FILE_WRITE_not_defined
#define FILE_SEEK FILE_SEEK_not_defined
#define FILE_TELL FILE_TELL_not_defined
#endif

/*----- Function to determine with a path name is a directory or not.*/

int isdir(
    char *path)
{

#if defined(WIN32)
    struct _stat statbuf;
    if (_stat(path, &statbuf) == 0 && (statbuf.st_mode & _S_IFDIR))
#else
    struct stat statbuf;
    if (stat(path, &statbuf) == 0 && S_ISDIR(statbuf.st_mode))
#endif
        return 1;
    else
        return 0;
}

/*----- Function to convert a space-padded FORTRAN string to a null-terminated C string.*/

void fstr2cstr(
    char *fstr,
    int len,
    char *cstr)
{

    int i;

    /*  Look for the last non-space character in the character array */

    for (i = len; i >= 0; i--)
        if (fstr[i] != ' ')
            break;

    /*  Copy backwards all the relevant characters */

    cstr[i + 1] = '\0';
    for (; i >= 0; i--)
        cstr[i] = fstr[i];
}

/*----- Function to convert a null-terminated C string to a space-padded FORTRAN string. */

void cstr2fstr(
    char *cstr,
    int len,
    char *fstr)
{

    int i;

    /*  Copy to to but not including the C string terminator */

    for (i = 0; i < len && cstr[i] != '\0'; i++)
        fstr[i] = cstr[i];

    /*  Pad the FORTRAN character array with spaces */

    for (; i < len; i++)
        fstr[i] = ' ';
}

/*------------------------------------------------------------------------------*/

void STDCALL
CUTIL_CDATE(
#if defined(WIN32)
    char *date,
    int date_length
#else
    char *date
#endif
)
{

    time_t timer = time(NULL);
    size_t max_len = 30;
    strncpy(date, asctime(localtime(&timer)), max_len);
}

void STDCALL
CUTIL_CGETCP(
    double *cpu)
{

#if defined(WIN32)
    *cpu = (double)clock() / CLOCKS_PER_SEC;
#else
    struct tms buf;
    times(&buf);
    *cpu = (double)((buf.tms_utime + buf.tms_cutime) * 100.0 / (sysconf(_SC_CLK_TCK)));
    *cpu = *cpu / 100.0;
#endif
}

void STDCALL
CUTIL_CSTOP(
#if defined(WIN32)
    long *exitcode,
    char *message,
    int message_length
#else
    long *exitcode,
    char *message
#endif
)
{

    printf("%s\n", message);
    exit(*exitcode);
}

void STDCALL
CUTIL_GETENV(
#if !defined(WIN32)
    char *name,
    int *lenname,
    char *value,
    int *lenvalue
#else
    char *name,
    int *lenname,
    char *value,
    int *lenvalue,
    int name_LENGTH,
    int value_LENGTH
#endif
)
{

    char buf[10000];
    char *enval;

    /*----  Convert environment variable name to a C string, look it up,
            and if the result exists convert it to a Fortran string. */

    fstr2cstr(name, *lenname, buf);

    if ((enval = getenv(buf)) != NULL)
        cstr2fstr(enval, *lenvalue, value);
}

void STDCALL
CUTIL_SYSTEM(
#if !defined(WIN32)
    char *command,
    int *len
#else
    char *command,
    int *len,
    int command_LENGTH
#endif
)
{

    char localcmd[MAX_CMD + 1];
#if defined(WIN32)
    int success;
    STARTUPINFO start;
    PROCESS_INFORMATION process;
#endif

    fstr2cstr(command, *len, localcmd);

#if defined(WIN32)
    start.cb = sizeof(STARTUPINFO);
    start.lpReserved = NULL;
    start.lpDesktop = NULL;
    start.lpTitle = NULL;
    start.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
    start.cbReserved2 = 0;
    start.lpReserved2 = NULL;

    start.wShowWindow = SW_HIDE;
    start.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    start.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    start.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    success = CreateProcess(NULL, localcmd, NULL, NULL, 1, 0,
                            NULL, NULL, &start, &process);
    if (success)
    {
        WaitForSingleObject(process.hProcess, INFINITE);
        CloseHandle(process.hProcess);
        CloseHandle(process.hThread);
    }
#else
    system(localcmd);
#endif
}

void STDCALL
CUTIL_SLEEP(
    int *millisec)
{

#if defined(WIN32)
    Sleep(*millisec);
#else
    usleep(*millisec * 1000); // usleep takes microsec
#endif
}

///*------------------------------------------------------------------------------*/
//// Some routines for reading from gfortran from a single file with multiple file handles simultaneously
//
// #define _MAX_LENGTH_ 6666
//
// typedef struct FileHandle {
//	FILE * fp;
//	long prevFilePos;
//} FileHandle;
//
// long long int STDCALL
// CUTIL_OPEN (
//    char* fname
//    ) {
//	FileHandle *fh = (FileHandle*) malloc(sizeof(FileHandle));
//	fh->fp = fopen(fname,"r");
//	fh->prevFilePos = 0;
//    /*---- Open file, return filepointer */
//    return ((long long int) fh);
//    }
//
// int STDCALL
// CUTIL_BACKSPACE (
//    long long int* ifh
//    ) {
//	FileHandle* fh = (FileHandle*)ifh;
//	fseek(fh->fp, fh->prevFilePos, SEEK_SET);
//    return (0);
//    }
//
// int STDCALL
// CUTIL_EOF (
//    long long int* ifh
//    ) {
//	FileHandle* fh = (FileHandle*)ifh;
//    /*---- EOF reached ? */
//	return (feof(fh->fp));
//    }
//
// int STDCALL
// CUTIL_REWIND (
//    long long int* ifh
//    ) {
//	FileHandle* fh = (FileHandle*)ifh;
//    /*---- rewind file */
//	rewind(fh->fp);
//    }
//
// int STDCALL
// CUTIL_READ (
//    long long int*  ifh,
//    char* resultstr
//    ) {
//	FileHandle* fh = (FileHandle*)ifh;
//	fh->prevFilePos = ftell(fh->fp);
//    /*---- read a line from file */
//    resultstr = fgets(resultstr,_MAX_LENGTH_,fh->fp);
//    return(0);
//    }
//
// int STDCALL
// CUTIL_CLOSE (
//    long long int* ifh
//    ) {
//	FileHandle* fh = (FileHandle*)ifh;
//    /*---- close file */
//	fclose (fh->fp);
//	free(fh);
//    return(0);
//    }
//
///*------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------*/
// Some routines for reading from a single file with multiple file handles simultaneously

#define _MAX_LENGTH_ 6666

int STDCALL
CUTIL_MF_SETMAXSTDIO(
    int *max_size)
{
#if !defined(WIN32)
    struct rlimit old, new;

    getrlimit(RLIMIT_NOFILE, &old);
    new.rlim_max = old.rlim_max;
    new.rlim_cur = *max_size;
    if (setrlimit(RLIMIT_NOFILE, &new) == 0)
    {
        return *max_size;
    }
    else
    {
        return -1;
    }
#else
    return _setmaxstdio(*max_size);
#endif
}

long long int STDCALL
CUTIL_MF_OPEN(
    char *fname)
{
    FILE *fh;
    fh = fopen(fname, "rb");
    /*---- Open file, return filepointer */
    return ((long long int)fh);
}

int STDCALL
CUTIL_MF_BACKSPACE(
    long long int *ifh,
    long long int *prevpos)
{
    FILE_SEEK((FILE *)*ifh, *prevpos, SEEK_SET);
    return (0);
}

int STDCALL
CUTIL_MF_EOF(
    long long int *ifh)
{
    /*---- EOF reached ? */
    return (feof((FILE *)*ifh));
}

int STDCALL
CUTIL_MF_REWIND(
    long long int *ifh)
{
    /*---- rewind file */
    rewind((FILE *)*ifh);
    return (0);
}

int STDCALL
CUTIL_MF_READ(
    long long int *ifh,
    char *resultstr,
    long long int *currentpos)
{
    *currentpos = FILE_TELL((FILE *)*ifh);                    /*---- save current pos in the file b4 reading */
    resultstr = fgets(resultstr, _MAX_LENGTH_, (FILE *)*ifh); /*---- read a line from file */
    return (0);
}

int STDCALL
CUTIL_MF_GETPOS(
    long long int *ifh,
    long long int *currentpos)
{
    *currentpos = FILE_TELL((FILE *)*ifh); /*---- save current pos in the file */
    return (0);
}

int STDCALL
CUTIL_MF_CLOSE(
    long long int *ifh)
{
    /*---- close file */
    fclose((FILE *)*ifh);
    return (0);
}

/*-------------------------------------------------------------------------------------------*/
// Some routines for comparing doubles and floats by converting to their integer representation
// These two are for testing equality between floats by using integers (as a cheaper alternative
// to comparedouble in fortran) and WILL be removed as soon as they are deemed obsolete.

int STDCALL
CUTIL_CMP_DOUBLE(
    double *val1,
    double *val2,
    int *eps)
{
    long longDiff;
    if (*val1 == *val2)
        return 0; // exactly equal
    longDiff = labs(*(long *)val1 - *(long *)val2);
    if (longDiff <= *eps)
        return 0;                        // equal within tolerance
    return ((*val1) > (*val2) ? 1 : -1); // greater than, less than
}

int STDCALL
CUTIL_CMP_SINGLE(
    float *val1,
    float *val2,
    int *eps)
{
    long longDiff;
    if (*val1 == *val2)
        return 0; // exactly equal
    longDiff = labs(*(long *)val1 - *(long *)val2);
    if (longDiff <= *eps)
        return 0;                        // equal within tolerance
    return ((*val1) > (*val2) ? 1 : -1); // greater than, less than
}

/*------------------------------------------------------------------------------*/

/*  Routines to locate the "default" directory */

#define SUCCESS 0
#define FAILURE 1

static void report_error(char *);

/* FTN_CAPITAL is assumed to be the default value */

#if linux
#include "config.h"
#define STDCALL /* nothing */
#define CUTIL_GETEXEDIR FC_FUNC(cutil_getexedir, CUTIL_GETEXEDIR)
#else
// WIN32
#define STDCALL /* nothing */
#define CUTIL_GETEXEDIR CUTIL_GETEXEDIR
#endif

void STDCALL
CUTIL_GETMP(
#if !defined(WIN32)
    char *path,
    int *lenpath,
    int *result
#else
    char *path,
    int *lenpath,
    int *result,
    int path_LENGTH
#endif
)
{

    char slash; /* UNIX or Windows directory separator */
    char buf[1000];
    char path_buffer[1000];
    char drive[1000];
    char dir[1000];
    char fname[1000];
    char ext[1000];
    int err;
    int len;
    len = 1000;
    /*----  Get and validate default directory using the location of this binary */

#ifdef WIN32
    slash = '\\';
    GetModuleFileName(NULL, path_buffer, len);
    err = _splitpath_s(path_buffer, drive, len, dir, len, fname, len, ext, len);
    if (err != 0)
    {
        report_error("Unable to read/split the executable directory");
        *result = FAILURE;
        return;
    }
    sprintf(path_buffer, "%s%s", drive, dir);
#else
    slash = '/';
    readlink("/proc/self/exe", path_buffer, len);
    sprintf(path_buffer, "%s%c", dirname(path_buffer), slash);
#endif
    /*---- release version: directory default should be next to directory bin */
    sprintf(buf, "%s..%cdefault", path_buffer, slash);

    if (!isdir(buf))
    {
        /*---- Try the new (Linux) delivery location */
        sprintf(buf, "%s..%cshare%cdelft3d", path_buffer, slash, slash);
        if (!isdir(buf))
        {
            report_error("Directory \"default\" does not exist");
            *result = FAILURE;
            return;
        }
    }
    /*---- The path should end with a slash. Must be added after being checked with isdir. */
    sprintf(buf, "%s%c", buf, slash);
    cstr2fstr(buf, *lenpath, path);
    *result = SUCCESS;
}

void STDCALL
CUTIL_GETEXEDIR(
#if !defined(WIN32)
    char *path,
    int *lenpath,
    int *result
#else
    char *path,
    int *lenpath,
    int *result,
    int path_LENGTH
#endif
)
{

    char slash; /* UNIX or Windows directory separator */
    char path_buffer[1000];
    char drive[1000];
    char dir[1000];
    char fname[1000];
    char ext[1000];
    int err;
    int len;
    len = 1000;
    /*----  Get and validate default directory using the location of this binary */

#ifdef WIN32
    slash = '\\';
    GetModuleFileName(NULL, path_buffer, len);
    err = _splitpath_s(path_buffer, drive, len, dir, len, fname, len, ext, len);
    if (err != 0)
    {
        report_error("Unable to read/split the executable directory");
        *result = FAILURE;
        return;
    }
    sprintf(path_buffer, "%s%s", drive, dir);
#else
    slash = '/';
    readlink("/proc/self/exe", path_buffer, len);
    sprintf(path_buffer, "%s%c", dirname(path_buffer), slash);
#endif
    cstr2fstr(path_buffer, *lenpath, path);
    *result = SUCCESS;
}

static void
report_error(
    char *message)
{
    printf("*** ERROR %s. Check installation procedure\n", message);
    fflush(stdout);
}
