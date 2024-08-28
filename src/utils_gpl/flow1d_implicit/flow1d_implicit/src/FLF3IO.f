      function FLF3IO (w, q, l, r1, ks1, r2, ks2, npier,
     &                 h1le, h2le)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLF3IO, FLow Friction head type 3IO
c
c Module description: Calculates the friction head 3IO in structures.
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
c $Log: flf3io.pf,v $
c Revision 1.2  1998/06/08  12:35:15  kuipe_j
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
*      1      w         I     total width of sluice
*      2      q         I     discharge
*      3      l         I     total of sluice length up- and downstream
*      4      r1       I/O    hydraulic radius inlet
*      5      ks1       I     roughness value sluice section upstream
*  (   6      alfa1     I     ratio width of approach section vs.
*                             total width of sluice)
*      7      r2       I/O    hydraulic radius outlet
*      8      ks2       I     roughness value sluice section downstream
*      9      n         I     number of piers
*     10      h1le      I     waterdepth upstream
*     11      h2le      I     waterdepth downstream
*
*  The function is used for structure type III
*
************************************************************************

c
c declare arguments
      integer npier
      real    w, q, l, r1, ks1, r2, ks2, h1le, h2le
c
c declare local variables
      real    a1, a2, k1, k2, f1root, f2root, hulp, n1, n2, rmax,
     &        w2

c
c declare function
      real    FLF3IO

      w2     = w * w

c---  set minimal value on r1:  ----------------------------------------

      rmax   = max(0.1, 10 * ks1)
      if (r1 .lt. rmax) then
          r1 = rmax
          a1 = w2 * r1 / ( w - 2 * (npier+1)* r1)
      else
          a1 = w * h1le
      endif
      hulp   = 12 * r1 / ks1
      f1root = 1. / (2 * log10(hulp))
      n1     = r1**(1./6) * f1root / 8.86
      k1     = a1 * r1**(2./3) / n1

c---  set minimal value on r2:  ----------------------------------------

      rmax = max(0.1, 10 * ks2)
      if (r2 .lt. rmax) then
          r2 = rmax
          a2 = r2 * w2 / (w - 2 *(npier+1)* r2)
      else
          a2 = w * h2le
      endif
      hulp   = 12 * r2 / ks2
      f2root = 1. / (2 * log10(hulp))
      n2     = r2**(1./6) * f2root / 8.86
      k2     = a2 * r2**(2./3) / n2

      FLF3IO  = l * q * q / k1 / k2

      return
      end
