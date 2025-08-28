//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2025.
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
// $Id: exception.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: $
//------------------------------------------------------------------------------
//  Exception Object - Implementation
//
//  Irv.Elshoff@Deltares.NL
//  20 jan 11
//------------------------------------------------------------------------------


#include "exception.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>


Exception::Exception(bool fatal, ErrorCode errorCode, const char * format, ...) : errorCode(errorCode)
{
   const int bufsize = 256 * 1024;
   char * buffer = new char[bufsize]; // really big temporary buffer, just in case

   va_list arguments;
   va_start(arguments, format);
   int len = vsnprintf(buffer, bufsize - 1, format, arguments);
   va_end(arguments);
   buffer[bufsize - 1] = '\0';

   this->fatal = fatal;
   this->message = new char[len + 1];
   strcpy(this->message, buffer);

   delete[] buffer;
}


Exception::~Exception(void)
{
   delete[] this->message;
}
