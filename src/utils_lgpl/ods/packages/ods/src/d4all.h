/* d4all.h   (c)Copyright Sequiter Software Inc., 1991-2024. All rights reserved. */

/**********************************************************************/
/**********            USER SWITCH SETTINGS AREA            ***********/

/* Index File compatibility options */
/* #define S4FOX     */
/* #define S4CLIPPER */
   #define S4MDX
/* #define S4NDX     */

/* Specify Operating Environment or Screen Output Option - Default is DOS */
/* #define S4DLL      */
/* #define S4WINDOWS  */
/* #define S4OS2      */
/* #define S4OS2DLL   */    /* OS/2 PRESENTATION MANAGER DLL LIBRARY */
/* #define S4OS2PM    */    /* OS/2 PRESENTATION MANAGER STATIC LIBRARY */
/* #define S4UNIX     */    /* requires Multi-Platform Version */
/* #define S4MACINTOSH */    /* requires Multi-Platform Version */
/* #define S4CODE_SCREENS */
/* #define S4PASCAL_DOS */  /* Borland Pascal for DOS DLL */
/* #define S4PASCAL_WIN */  /* Borland Pascal for Windows DLL */

/* General Configuration Options */
#define S4DEBUG
#define S4ERROR_HOOK
/* #define S4LOCK_CHECK   */
/* #define S4LOCK_HOOK    */
/* #define S4MAX          */
/* #define S4MEMO_OFF     */
/* #define S4OFF_ERROR    */
/* #define S4OFF_INDEX    */
/* #define S4OFF_MEMO     */
/* #define S4OFF_MULTI    */
/* #define S4OFF_OPTIMIZE */
/* #define S4OFF_WRITE    */
/* #define S4OLD_CODE     */
/* #define S4OPTIMIZE_OFF */
/* #define S4PORTABLE     */
/* #define S4SAFE         */
/* #define S4SINGLE       */
/**********************************************************************/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef S4MACINTOSH
   #define S4UNIX
#endif

#ifdef S4UNIX
   #include "p4port.h"
#else
   #include <limits.h>
   #include <io.h>
   #include <stdarg.h>
#endif

#ifdef S4VB_DOS
   #include "v4names.h"
#endif

#include "d4data.h"
#include "f4flag.h"
#include "e4expr.h"
#include "s4sort.h"
#include "e4error.h"

#include "o4opt.h"
#include "r4relate.h"

#ifdef S4VB_DOS
   #include "v4dos.h"
#endif

#ifdef S4WINDOWS
   #include <windows.h>
#endif

#include "r4report.h"

#define S4VERSION  5002
