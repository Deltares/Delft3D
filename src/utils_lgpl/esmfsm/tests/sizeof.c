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
/*------------------------------------------------------------------------------
 *  Print sizes of fundamental data types and pointers
 *
 *  Irv.Elshoff@deltares.nl
 *  13 oct 05
 *----------------------------------------------------------------------------*/


#include <stdio.h>


int
main (
    int argc,
    char *argv[],
    char *envp[]
    ) {

    printf ("sizeof (char)      = %d\n",    sizeof (char));
    printf ("sizeof (short)     = %d\n",    sizeof (short));
    printf ("sizeof (int)       = %d\n",    sizeof (int));
    printf ("sizeof (long)      = %d\n",    sizeof (long));
    printf ("sizeof (long long) = %d\n",    sizeof (long long));
    printf ("sizeof (float)     = %d\n",    sizeof (float));
    printf ("sizeof (double)    = %d\n",    sizeof (double));
    printf ("sizeof (char *)    = %d\n",    sizeof (char *));
    printf ("sizeof (short *)   = %d\n",    sizeof (short *));
    printf ("sizeof (int *)     = %d\n",    sizeof (int *));
    printf ("sizeof (long *)    = %d\n",    sizeof (long *));
    printf ("sizeof (float *)   = %d\n",    sizeof (float *));
    printf ("sizeof (double *)  = %d\n",    sizeof (double *));
    printf ("sizeof (void *)    = %d\n",    sizeof (void *));

    return 0;
    }



/*------------------------------------------------------------------------------

    OUTPUT ON SGI Origin in 32-bit mode:

        sizeof (char)      = 1
        sizeof (short)     = 2
        sizeof (int)       = 4
        sizeof (long)      = 4
        sizeof (long long) = 8
        sizeof (float)     = 4
        sizeof (double)    = 8
        sizeof (char *)    = 4
        sizeof (short *)   = 4
        sizeof (int *)     = 4
        sizeof (long *)    = 4
        sizeof (float *)   = 4
        sizeof (double *)  = 4
        sizeof (void *)    = 4


    OUTPUT ON SGI Origin in 64-bit mode:

        sizeof (char)      = 1
        sizeof (short)     = 2
        sizeof (int)       = 4
        sizeof (long)      = 8
        sizeof (long long) = 8
        sizeof (float)     = 4
        sizeof (double)    = 8
        sizeof (char *)    = 8
        sizeof (short *)   = 8
        sizeof (int *)     = 8
        sizeof (long *)    = 8
        sizeof (float *)   = 8
        sizeof (double *)  = 8
        sizeof (void *)    = 8


*----------------------------------------------------------------------------*/
