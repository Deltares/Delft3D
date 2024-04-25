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
// $Id$
// $HeadURL$
/*                                               */
/* <nefis4.h> -  Basic types                     */
/*                                               */
/*                                               */
/* J. Mooiman                                    */
/*                                               */
/* This header file defines the NEFIS functions  */
/*                                               */

#ifndef __NEFIS4_H__
#define __NEFIS4_H__

#include "btps.h"

#define DEFINE_STEPS           3
#define MAX_NAME              16

extern BInt4 Clsnef ( BInt4 *);
extern BInt4 Crenef ( BInt4 *, BText,  BText,
                               BChar,  BChar);
extern BInt4 Defcel ( BInt4 *, BText  , BInt4  , BChar [][MAX_NAME+1]);
extern BInt4 Defelm ( BInt4 *, BText  , BText  ,
                      BInt4  , BText  , BText  ,
                      BText  , BInt4  , BInt4 *);
extern BInt4 Defgrp ( BInt4 *, BText  , BText  ,
                      BInt4  , BInt4 *, BInt4 *);
extern BInt4 Flsdat ( BInt4 *);
extern BInt4 Flsdef ( BInt4 *);
extern BInt4 Getelt ( BInt4 *, BText  , BText  ,
                      BInt4 [][DEFINE_STEPS]   , BInt4 *, BInt4 *, BData  );
extern BInt4 Inqcel ( BInt4 *, BText  , BInt4 *, BChar [][MAX_NAME+1]);
extern BInt4 Inqdat ( BInt4 *, BText  , BText  );
extern BInt4 Inqelm ( BInt4 *, BText  , BText  , BInt4 *, BText  , BText  ,
                      BText  , BInt4 *, BInt4 *);
extern BInt4 Inqfcl ( BInt4 *, BText  , BInt4 *, BInt4 *, BChar [][MAX_NAME+1]);
extern BInt4 Inqncl ( BInt4 *, BText  , BInt4 *, BInt4 *, BChar [][MAX_NAME+1]);
extern BInt4 Inqfel ( BInt4 *, BText  , BText  , BText  , BText  ,
                      BText  , BInt4 *, BInt4 *, BInt4 *, BInt4 *);
extern BInt4 Inqnel ( BInt4 *, BText  , BText  , BText  , BText  ,
                      BText  , BInt4 *, BInt4 *, BInt4 *, BInt4 *);
extern BInt4 Inqfgr ( BInt4 *, BText  , BText  , BInt4 *, BInt4 *, BInt4 *);
extern BInt4 Inqngr ( BInt4 *, BText  , BText  , BInt4 *, BInt4 *, BInt4 *);
extern BInt4 Inqfia ( BInt4 *, BText  , BText  , BInt4 *);
extern BInt4 Inqfra ( BInt4 *, BText  , BText  , BRea4 *);
extern BInt4 Inqfsa ( BInt4 *, BText  , BText  , BText  );
extern BInt4 Inqfst ( BInt4 *, BText  , BText  );
extern BInt4 Inqgrp ( BInt4 *, BText  , BText  , BInt4 *, BInt4 *, BInt4 *);
extern BInt4 Inqmxi ( BInt4 *, BText  , BInt4 *);
extern BInt4 Inqnia ( BInt4 *, BText  , BText  , BInt4 *);
extern BInt4 Inqnra ( BInt4 *, BText  , BText  , BRea4 *);
extern BInt4 Inqnsa ( BInt4 *, BText  , BText  , BText  );
extern BInt4 Inqnxt ( BInt4 *, BText  , BText  );
extern BInt4 Neferr ( BInt4  , BText  );
extern BInt4 Putelt ( BInt4 *, BText  , BText  ,
                      BInt4 [][DEFINE_STEPS]   , BInt4 *,  BData  );

#endif /* __NEFIS4_H__ */
