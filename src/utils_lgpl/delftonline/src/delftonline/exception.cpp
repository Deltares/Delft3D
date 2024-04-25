//-------------------------------------------------------------------------------
//  DelftOnline -- Exceptions
//
//  Irv.Elshoff@Deltares.NL
//  25 may 12
//-------------------------------------------------------------------------------
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


#include "dol.h"

namespace DOL {


//-------------------------------------------------------------------------------
//  Exception Constructor/Destructor and Utility Routines


Exception::Exception (
    bool    fatal,
    const char *  format,
    ...
    ) {

    this->fatal = fatal;

    const int size = Exception::MaxErrorMesgLen;
    char buffer [size];     // really big temporary buffer

    va_list arguments;
    va_start (arguments, format);
    int len = vsnprintf (buffer, size, format, arguments);
    va_end (arguments);

    this->message = new char [len+1];
    strcpy (this->message, buffer);
    }


Exception::~Exception (
    void
    ) {

    delete this->message;
    }

}
