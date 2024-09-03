      function FLFRST (q, l, r1, a1, ks1, r2, a2, ks2)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLow calculate FRiction in STructures
c
c Module description: Function is taken from WENDY as part of the new
c                     structure functionality. WENDY history is:
c                        Project: Construction-Module
c                        Programmer: G. van Driel
c                        Function: Calculation of the friction head
c                        Updates: None
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
c $Log: flfrst.pf,v $
c Revision 1.2  1998/06/08  12:35:22  kuipe_j
c log added
c
c
c
c***********************************************************************
*  Parameters  (Input / Output) :
*  ------------------------------
*
*   Number    Name     I/O    Description
*   ------    ----     ---    ------------
*      1      q         I     discharge
*      2      l         I     length
*      3      r1        I     hydraulic radius
*      4      a1        I     wet area
*      5      ks1       I     roughness coefficient
*      6      r2        I     hydraulic radius
*      7      a2        I     wet area
*      8      ks2       I     roughness coefficient
*
*  The function is used for structure type VI,VII,VIII and IX
*
************************************************************************

c
c declare arguments
      real q, l, a1, r1, ks1, a2, r2, ks2, k1, k2

c
c declare variables
      real f1root, f2root, hulp, n1, n2
      
c
c declare functions
      real FLFRST

      hulp   = 12 * r1 / ks1
      f1root = 1. / (2. * LOG10(hulp))
      n1     = r1**(1./6.) * f1root / 8.86
      k1     = a1 * r1**(2./3.) / n1

      hulp   = 12 * r2 / ks2
      f2root = 1. / (2 * LOG10(hulp))
      n2     = r2**(1./6.) * f2root / 8.86
      k2     = a2 * r2**(2./3.) / n2

      FLFRST  = l * q * q / k1 / k2

      return
      end


