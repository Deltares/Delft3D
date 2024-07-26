      function FLSQRT (root)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             Flow calculate SQuare RooT
c
c Module description: Used in structure routines to evaluate the root
c                     of a value which may be negative.
c                     If x < 0 then FLSQRT = -sqrt(-x)
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flsqrt.pf,v $
c Revision 1.2  1998/06/08  12:35:53  kuipe_j
c log added
c
c
c
c***********************************************************************
c
c declare arguments
      real root

c
c declare function
      real FLSQRT

      if (root .lt. 0) then
        FLSQRT = -SQRT(-root)
      else
        FLSQRT = SQRT(root)
      endif

      end
