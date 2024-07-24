      DOUBLE PRECISION FUNCTION GETBRK()

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         Kian Tan 
c
c Module:             GETBRK (GET BReaKdown parameter tolerance)
c
c Module description: Get breakdown parameter tolerance; for the test
c                     routine, set to machine precision.
c
c-----------------------------------------------------------------------
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: getbrk.pf,v $
c Revision 1.3  1995/10/18  08:59:39  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/09/22  10:03:00  kuipe_j
c variable dimensions, new headers
c
c
c
c***********************************************************************
*
*     Get breakdown parameter tolerance; for the test  routine,
*     set to machine precision.
*
      DOUBLE PRECISION   EPS,  DLAMCH
*
      EPS = DLAMCH('EPS')
      GETBRK = EPS**2
*
      RETURN
*
      END
