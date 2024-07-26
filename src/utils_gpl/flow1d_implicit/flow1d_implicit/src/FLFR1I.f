      function FLFR1I (w, q, l, r1, ks1,alfa1, r2, ks2,alfa2,ski)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLow FRiction head 1I in structures
c
c Module description: Calculates the friction in the structure (used
c                     only for the open flume). Code is taken from 
c                     WENDY. History is:
c                       Project:  Construction-Module
c                       Programmer:  G. van Driel
c                       Function:  Calculation of the friction head
c                       Updates: none.
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
c $Log: flfr1i.pf,v $
c Revision 1.2  1998/06/08  12:35:18  kuipe_j
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
*      1      w             I     width of flume
*      2      q             I     discharge
*      3      l             I     length of section
*      4      r1           I/O    hydraulic radius
*      5      ks1           I     roughness of section
*      6      alfa1         I     parameter
*      7      r2           I/O    hydraulic radius
*      8      ks2           I     roughness of section
*      9      alfa2         I     parameter
*     10      ski           O     Ki = Ai * Ri**(2./3.) / ni
*
************************************************************************

      real q, l, w, a1, r1, ks1, a2, r2, ks2, k1, k2
      real f1root, f2root, hulp, n1, n2, rmax
      real alfa1, alfa2, ski, w2

c
c declare function
      real FLFR1I
      w2     = w * w

c
c set minimum value for r1
      rmax   = MAX(0.1, 10 * ks1)
      if (r1 .lt. rmax) then
        r1 = rmax
        a1 = alfa1 * w2 * r1 / ( w - 2.*r1)
      else
          a1 = r1 * alfa1 * w
      endif
      hulp   = 12. * r1 / ks1
      f1root = 1. / (2 * ALOG10(hulp))
      n1     = r1**(1./6.) * f1root / 8.86
      k1     = a1 * r1**(2./3.) / n1

c
c set minimum value for r2
      rmax = MAX(0.1, 10 * ks2)
      if (r2 .lt. rmax) then
        r2 = rmax
        a2 = r2 * w2 / (w - 2.*r2)
      else
        a2 = alfa2 * w
      endif
      hulp   = 12. * r2 / ks2
      f2root = 1. / (2. * ALOG10(hulp))
      n2     = r2**(1./6.) * f2root / 8.86
      k2     = a2 * r2**(2./3.) / n2
      ski    = k2

      FLFR1I  = l * q * q / k1 / k2

      return
      end
