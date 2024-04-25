//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
// $Id: map_debug.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/map_debug.cpp $
//------------------------------------------------------------------------------
//  Module:  Mapper Debug
//  Functions for debug levels
//
//  Stef.Hummel@deltares.nl
//  Adri.Mourits@deltares.nl
//  31 may 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


#define MAX_LINE_LEN    100

//
// Debug level / Function Print Level
//

static bool _initialized    =false;
static int  _debugLevel     =0;
static int  _funcPrintLevel =0;
static int  _doLoggingFor   =0;
static int  _doLoggingToFile=0;


//
// Read debug levels from file
//


static void scanDebugLevels(
    FILE      * fh,             // file handle
    int       * totLev          // DebugLevel or FuncPrintLevel
    )
{
    char    str[MAX_LINE_LEN];  // string (file-path / line from file)
    char      * s;              // string to read levels from
    int     level;              // debug level read from file

    //
    // Read 3th or 4th line from debug file, and scan for next integer
    // by skipping spaces and digit strings that have been read.
    // Store level as bits.
    //

    s = fgets(str, MAX_LINE_LEN, fh);
    if (s != NULL)
    {
        do
        {
            while ( ( !isdigit(*s) ) && ( *s != '\0' ) ) s++;

            if ( *s != '\0' )
            {
                level = atoi (s);
                if (level != 0)
                {
                    int powerOfTwo = 1;
                    int i; for ( i = 1 ; i < level ; i++ ) powerOfTwo *= 2;

                    *totLev |= powerOfTwo;
                }
            }
            while ( ( !isspace(*s) ) && ( *s != '\0' ) ) s++;

        } while ( *s != '\0' );
    }

}


void ReadDebugLevel(void)
{
    FILE  * fh = NULL;                      // file handle
    char  * dbgFile = (char*)"hymapdbg.txt";// filename for debug file
    // char    path[_POSIX_PATH_MAX];;         // filepath

    //
    // Open file on current directory or on home-directory
    //

    if ( ! _initialized )
    {
        fh = fopen( dbgFile, "r");
        if ( fh != NULL )
        {
            scanDebugLevels(fh, &_doLoggingFor);
            scanDebugLevels(fh, &_doLoggingToFile);
            scanDebugLevels(fh, &_debugLevel);
            scanDebugLevels(fh, &_funcPrintLevel);
            fclose(fh);
        }
        _initialized = true;
    }
}


//
// Check if logging should be done
//

bool DoLoggingFor(
    LogObjectType   objectType  // Context/Mapper/Gaws
    )
{
    bool   retVal = _doLoggingFor & objectType ? true : false;
    return retVal;
}


//
// Check if logging should be done to file
//

bool DoLoggingToFileFor(
    LogObjectType   objectType  // Context/Mapper/Gaws
    )
{
    bool   retVal = _doLoggingToFile & objectType ? true : false;
    return retVal;
}


//
// Check if current function print level is active
//

bool GetFuncPrintLevel(
    int     callerLevel     // debug-level of calling function
    )
{
    bool   retVal = _funcPrintLevel & callerLevel ? true : false;
    return retVal;
}


//
// Check if current debug level is active
//

bool GetDebugLevel(
    int     callerLevel     // debug-level of calling function
    )
{
    bool   retVal = _debugLevel & callerLevel ? true : false;
    return retVal;
}


//
// Function for logging output
//

void MapLog(
    char      * format,     /* I: 'fprintf-format' for print of log */
    ...             /* I: arguments of log message (should be
                    terminated with NULL)       */
    )
{
    va_list arguments;

    va_start ( arguments, format );
    vprintf ( format, arguments );
    va_end ( arguments );
    fflush(stdout);

}
