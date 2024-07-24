      function DEQUAL (dvar1, dvar2)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             DEQUAL (EQUAL test of two Double precision variables)
c
c Module description: Logical function to check if two double precision
c                     variables are identical.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  0 dequal            O  Function value, TRUE/FALSE.
c  1 dvar1             I  Double precision variable.
c  2 dvar2             I  Double precision variable.
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: dequal.pf,v $
c Revision 1.2  1995/05/30  07:02:19  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:23  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:58  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of function:
c
      logical DEQUAL
c
c     Declaration of parameters:
c
      double precision dvar1, dvar2
c
      DEQUAL = dvar1 .eq. dvar2
      end
