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
/*                                               */
/* <nefis.h> -  Basic types                     */
/*                                               */
/*                                               */
/* J. Mooiman                                    */
/*                                               */
/* This header file defines the NEFIS functions  */
/*                                               */

#ifndef __NEFIS_H__
#define __NEFIS_H__

#include "btps.h"

#if defined(NEFIS_DLL)
#  define DLLEXPORT  __declspec( dllexport )
#else
#  define DLLEXPORT
#endif

#define DEFINE_STEPS            3
#define MAX_NAME               16
#define LENGTH_ERROR_MESSAGE 1024

extern DLLEXPORT BInt4 Cldfnf ( BInt4 *);
extern DLLEXPORT BInt4 Cldtnf ( BInt4 *);
extern DLLEXPORT BInt4 Clsdat ( BInt4 *);
extern DLLEXPORT BInt4 Clsdef ( BInt4 *);
extern DLLEXPORT BInt4 Clsnef ( BInt4 *);
extern DLLEXPORT BInt4 Crenef ( BInt4 *, BText,  BText,
                                BChar,  BChar);
extern DLLEXPORT BInt4 Credat ( BInt4 *, BText  , BText);
extern DLLEXPORT BInt4 Defcel ( BInt4 *, BText  , BInt4  , BChar [][MAX_NAME+1]);
extern DLLEXPORT BInt4 Defcel2( BInt4 *, BText  , BInt4  , BText *);
extern DLLEXPORT BInt4 Defcel3( BInt4 *, BText  , BInt4  , BText  );
extern DLLEXPORT BInt4 Defelm ( BInt4 *, BText  , BText  ,
                                BInt4  , BText  , BText  ,
                                BText  , BInt4  , BInt4 *);
extern DLLEXPORT BInt4 Defgrp ( BInt4 *, BText  , BText  ,
                                BInt4  , BInt4 *, BInt4 *);
extern DLLEXPORT BInt4 Flsdat ( BInt4 *);
extern DLLEXPORT BInt4 Flsdef ( BInt4 *);
extern DLLEXPORT BInt4 Getelt ( BInt4 *, BText  , BText  ,
                                BInt4 *, BInt4 *, BInt4 *, BData  );
extern DLLEXPORT BInt4 Gethdt ( BInt4 *, BText  );
extern DLLEXPORT BInt4 Gethdf ( BInt4 *, BText  );
extern DLLEXPORT BInt4 Inqcel ( BInt4 *, BText  , BInt4 *, BChar [][MAX_NAME+1]);
extern DLLEXPORT BInt4 Inqcel2( BInt4 *, BText  , BInt4 *, BText **);
extern DLLEXPORT BInt4 Inqcel3( BInt4 *, BText  , BInt4 *, BText   );
extern DLLEXPORT BInt4 Inqdat ( BInt4 *, BText  , BText  );
extern DLLEXPORT BInt4 Inqelm ( BInt4 *, BText  , BText  , BInt4 *, BText  , BText  ,
                                BText  , BInt4 *, BInt4 *);
extern DLLEXPORT BInt4 Inqfcl ( BInt4 *, BText  , BInt4 *, BInt4 *, BChar [][MAX_NAME+1]);
extern DLLEXPORT BInt4 Inqfcl2( BInt4 *, BText  , BInt4 *, BInt4 *, BText **);
extern DLLEXPORT BInt4 Inqfcl3( BInt4 *, BText  , BInt4 *, BInt4 *, BText * );
extern DLLEXPORT BInt4 Inqncl ( BInt4 *, BText  , BInt4 *, BInt4 *, BChar [][MAX_NAME+1]);
extern DLLEXPORT BInt4 Inqncl2( BInt4 *, BText  , BInt4 *, BInt4 *, BText **);
extern DLLEXPORT BInt4 Inqncl3( BInt4 *, BText  , BInt4 *, BInt4 *, BText * );
extern DLLEXPORT BInt4 Inqfel ( BInt4 *, BText  , BText  , BText  , BText  ,
                                BText  , BInt4 *, BInt4 *, BInt4 *, BInt4 *);
extern DLLEXPORT BInt4 Inqnel ( BInt4 *, BText  , BText  , BText  , BText  ,
                                BText  , BInt4 *, BInt4 *, BInt4 *, BInt4 *);
extern DLLEXPORT BInt4 Inqfgr ( BInt4 *, BText  , BText  , BInt4 *, BInt4 *, BInt4 *);
extern DLLEXPORT BInt4 Inqngr ( BInt4 *, BText  , BText  , BInt4 *, BInt4 *, BInt4 *);
extern DLLEXPORT BInt4 Inqfia ( BInt4 *, BText  , BText  , BInt4 *);
extern DLLEXPORT BInt4 Inqfra ( BInt4 *, BText  , BText  , BRea4 *);
extern DLLEXPORT BInt4 Inqfsa ( BInt4 *, BText  , BText  , BText  );
extern DLLEXPORT BInt4 Inqfst ( BInt4 *, BText  , BText  );
extern DLLEXPORT BInt4 Inqgrp ( BInt4 *, BText  , BText  , BInt4 *, BInt4 *, BInt4 *);
extern DLLEXPORT BInt4 Inqmxi ( BInt4 *, BText  , BInt4 *);
extern DLLEXPORT BInt4 Inqnia ( BInt4 *, BText  , BText  , BInt4 *);
extern DLLEXPORT BInt4 Inqnra ( BInt4 *, BText  , BText  , BRea4 *);
extern DLLEXPORT BInt4 Inqnsa ( BInt4 *, BText  , BText  , BText  );
extern DLLEXPORT BInt4 Inqnxt ( BInt4 *, BText  , BText  );
extern DLLEXPORT BInt4 Neferr ( BInt4  , BText  );
extern DLLEXPORT BInt4 Opndat ( BInt4 *, BText  , BChar  );
extern DLLEXPORT BInt4 Opndef ( BInt4 *, BText  , BChar  );
extern DLLEXPORT BInt4 Putelt ( BInt4 *, BText  , BText  , BInt4 *, BInt4 *, BData  );
extern DLLEXPORT BInt4 Getnfv ( BText *);
extern DLLEXPORT BInt4 Resnfv ( BInt4, BInt4);
extern DLLEXPORT BInt4 Clsanf ( );

#endif /* __NEFIS_H__ */
