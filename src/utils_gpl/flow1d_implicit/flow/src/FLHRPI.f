      function FLHRPI (x,d)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLHRPI (FLow Hydraulic Radius in circular PIpe)
c
c Module description: Calculate the hydraulic radius in a circular cross
c                     section. This function is used in the discharge
c                     calculation for a culvert. Code taken from WENDY.
c                     WENDY history is:
c                       Projekt: Construction-Module
c                       Programmeur: H. van Zanten
c                       Funktie: Calculation of the hydraulic radius in
c                                a circular section
c                       No updates.
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
c $Log: flhrpi.pf,v $
c Revision 1.2  1998/06/08  12:35:34  kuipe_j
c log added
c
c
c
c***********************************************************************
*
*  Parameters  (Input / Output) :
*  ------------------------------
*
*   Number    Name         I/O    Description
*   ------    ----         ---    ------------
*      1      x             I     parameter
*      2      d             I     diameter circular section
*
************************************************************************

c
c declare arguments
      real x, d

c
c declare variables
      real x2, x3, x4, x5, corr

c
c declare function
      real FLHRPI

      if (x .lt. 0.) x = 0.
      if (x .gt. 1.) x = 1.
      x2     = x * x
      x3     = x * x2
      x4     = x * x3
      x5     = x * x4
      corr   = .152*x5 - 5.449*x4 + 11.739*x3 - 8.771*x2 + 2.5486*x+.745
      FLHRPI = (-0.3629*x3 + 0.05742*x2 + 0.55739*x) * d * corr

      return
      end
