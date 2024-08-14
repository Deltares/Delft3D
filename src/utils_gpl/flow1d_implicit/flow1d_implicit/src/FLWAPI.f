      function FLWAPI (x,d)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLWAPI (FLow Wet Area in circular PIpe)
c
c Module description: Calculate the wat area in a circular cross
c                     section. This function is used in the discharge
c                     calculation for a culvert. Code taken from WENDY.
c                     WENDY history is:
c                       Projekt: Construction-Module
c                       Programmeur: H. van Zanten
c                       Funktie: Calculation of the wet area in a 
c                                circular section
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
c $Log: flwapi.pf,v $
c Revision 1.2  1998/06/08  12:35:54  kuipe_j
c log added
c
c
c
c***********************************************************************
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
c declare function
      real FLWAPI

      if (x .lt. 0.) x = 0.
      if (x .gt. 1.) x = 1.
      FLWAPI = (-0.8895*x*x*x + 1.33425*x*x + 0.33939*x) * d * d

      return
      end
