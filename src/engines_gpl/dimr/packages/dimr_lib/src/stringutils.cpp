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
// $Id: stringutils.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: $
//------------------------------------------------------------------------------
//  String Utility Routines - IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  2 jun 11
//------------------------------------------------------------------------------


#include "stringutils.h"

#include <string.h>
#include <stdlib.h>

#if defined (WIN32)
#   define strdup _strdup
#endif

#define IS_WHITESPACE(C) ( \
    (C) == '\f' || \
    (C) == '\r' || \
    (C) == '\t' || \
    (C) == '\v' || \
    (C) == ' '     \
    )
    // newlines are not considered whitespace here


//------------------------------------------------------------------------------


void
String::Chomp (
    char * string
    ) {

    // Remove trailing whitespace from a string (in place)

    int len = strlen (string);
    while (string[len-1] == '\n' || IS_WHITESPACE (string[len-1])) {
        string[len-1] = '\0';
        len--;
        }
    }


void
String::CollapseAllWhitespace (
    char * string
    ) {

    // Replace all consecutive sequences of whitespace (include newlines)
    // in the string argument with a single space (in place).

    char * original = strdup (string);

    char * np = string;
    char * op = original;

    bool lastWhite = true;

    while (*op != '\0') {
        if (IS_WHITESPACE (*op) || *op == '\n') {
            if (! lastWhite)
                *np++ = ' ';
            else
                lastWhite = true;

            op++;
            }

        else {
            *np++ = *op++;
            lastWhite = false;
            }
        }

    free (original);
    }


void
String::Tidy (
    char * string
    ) {

    // Remove leading and trailing whitespace around newlines and the beginning
    // and ends of the argument.  Also remove leading and trailing newlines.
    // This routine is primarily used to clean up character data from XML files.

    char * original = strdup (string);

    char * np = string;
    char * op = original;

    bool midline = false;   // true when we're in the middle of a line

    while (*op == '\n') op++;

    while (*op != '\0') {
        if (midline) {
            if (*op == '\n') {
                midline = false;
                while (IS_WHITESPACE (*(np-1))) np--;   // backtrack
                }
            *np++ = *op++;
            }

        else {
            if (IS_WHITESPACE (*op) || *op == '\n')
                op++;
            else {
                *np++ = *op++;
                midline = true;
                }
            }
        }

    while (IS_WHITESPACE (*(np-1)) || *(np-1) == '\n') np--;   // backtrack

    *np = '\0';
    free (original);
    }
