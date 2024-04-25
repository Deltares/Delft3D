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
/*                                           */
/* <btps.h> -  Basic types                   */
/*                                           */
/*                                           */
/* J. Mooiman                                */
/*                                           */
/* This header file defines the portability  */
/*                                           */

#ifndef __BTPS__
#define __BTPS__

#ifndef max
#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))
#endif

  typedef  void            BVoid    ;     /* void                      */
  typedef  void *          BData    ;     /* Pointer to void array     */
  typedef  char            BChar    ;     /* char           :  1 bytes */
  typedef  char *          BText    ;     /* Pointer to character array*/
  typedef  short           BInt2    ;     /* short          :  2 bytes */
  typedef  int             BInt4    ;     /* int            :  4 bytes */
  typedef  long            BInt8    ;     /* long           :  4 bytes */
  typedef  unsigned char   BUChar   ;     /* unsigned char  :  1 bytes */
  typedef  unsigned short  BUInt2   ;     /* unsigned short :  2 bytes */
  typedef  unsigned int    BUInt4   ;     /* unsigned       :  4 bytes */
  typedef  unsigned long   BUInt8   ;     /* unsigned long  :  4 bytes */
  typedef  float           BRea4    ;     /* float          :  4 bytes */
  typedef  double          BRea8    ;     /* double         :  8 bytes */
  typedef  long double     BRea16   ;     /* long double    : 10 bytes */

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

/* #define DO_DEBUG 1 */
#ifdef DO_DEBUG
#define DEBUG_LEVEL 5
#define DBG_NEFIS_TREE DBG_nefis_tree
#define ON_DEBUG(LEVEL, STUFF) \
  if(LEVEL<=DEBUG_LEVEL) {STUFF}
#else
#define ON_DEBUG(LEVEL, STUFF)
#define DBG_NEFIS_TREE(a,b)
#endif

#endif /* __BTPS__ */
