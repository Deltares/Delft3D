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
#  define max(a,b) (((a) > (b)) ? (a) : (b))
#  define min(a,b) (((a) < (b)) ? (a) : (b))
#endif

  typedef  void             BVoid    ;     /* void                      */
  typedef  void *           BData    ;     /* Pointer to void array     */
  typedef  char             BChar    ;     /* char           :  1 bytes */
  typedef  char *           BText    ;     /* Pointer to character array*/
  typedef  short            BInt2    ;     /* short          :  2 bytes */
  typedef  int              BInt4    ;     /* int            :  4 bytes */
  typedef  unsigned char    BUChar   ;     /* unsigned char  :  1 bytes */
  typedef  unsigned short   BUInt2   ;     /* unsigned short :  2 bytes */
  typedef  unsigned int     BUInt4   ;     /* unsigned int   :  4 bytes */
  typedef  float            BRea4    ;     /* float          :  4 bytes */
  typedef  double           BRea8    ;     /* double         :  8 bytes */
  typedef  long double      BRea16   ;     /* long double    : 10 bytes */

  typedef BText charp;
  typedef BData voidp;

#if defined PTR4
  typedef  long               BInt8    ;     /* long           :  4 bytes */
  typedef  unsigned long      BUInt8   ;     /* long           :  4 bytes */
#endif

#if defined PTR8
#if defined WIN64
  typedef  __int64            BInt8    ;     /* long           :  8 bytes */
  typedef  unsigned __int64   BUInt8   ;     /* long           :  8 bytes */
#elif defined GNU_PC
  typedef          long long  BInt8    ;     /* long           :  8 bytes */
  typedef unsigned long long  BUInt8   ;     /* long           :  8 bytes */
#elif defined HAVE_CONFIG_H
  typedef          long long  BInt8    ;     /* long           :  8 bytes */
  typedef unsigned long long  BUInt8   ;     /* long           :  8 bytes */
#elif defined USE_SUN
  typedef          long long  BInt8    ;     /* long           :  8 bytes */
  typedef unsigned long long  BUInt8   ;     /* long           :  8 bytes */
#else
  typedef          undefined  BInt8    ;     /* long           :  8 bytes */
  typedef unsigned undefined  BUInt8   ;     /* long           :  8 bytes */
#endif
#endif

#if defined PTR8
# undef  ULONG_MAX
# define ULONG_MAX (BUInt8) -1
#endif
/*
 * TRUE and FALSE defined as enumerator
 */
#if defined TRUE
#undef TRUE
#undef FALSE
#endif

#undef DO_DEBUG
#define DO_DEBUG 1

#ifdef DO_DEBUG
#define DEBUG_LEVEL 10
#define DBG_NEFIS_TREE DBG_nefis_tree
#define ON_DEBUG(LEVEL, STUFF) \
  if(LEVEL<=DEBUG_LEVEL) {STUFF}
#else
#define ON_DEBUG(LEVEL, STUFF)
#define DBG_NEFIS_TREE(a,b)
#endif

#endif /* __BTPS__ */
