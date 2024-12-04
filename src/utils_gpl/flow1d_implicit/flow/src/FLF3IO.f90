function FLF3IO (w, q, l, r1, ks1, r2, ks2, npier,&
&h1le, h2le)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLF3IO, FLow Friction head type 3IO
!
! Module description: Calculates the friction head 3IO in structures.
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flf3io.pf,v $
! Revision 1.2  1998/06/08  12:35:15  kuipe_j
! log added
!
!
!
!***********************************************************************
!  Parameters  (Input / Output) :
!  ------------------------------
!
!   Number    Name     I/O    Description
!   ------    ----     ---    ------------
!      1      w         I     total width of sluice
!      2      q         I     discharge
!      3      l         I     total of sluice length up- and downstream
!      4      r1       I/O    hydraulic radius inlet
!      5      ks1       I     roughness value sluice section upstream
!  (   6      alfa1     I     ratio width of approach section vs.
!                             total width of sluice)
!      7      r2       I/O    hydraulic radius outlet
!      8      ks2       I     roughness value sluice section downstream
!      9      n         I     number of piers
!     10      h1le      I     waterdepth upstream
!     11      h2le      I     waterdepth downstream
!
!  The function is used for structure type III
!
!***********************************************************************

!
! declare arguments
   integer npier
   real    w, q, l, r1, ks1, r2, ks2, h1le, h2le
!
! declare local variables
   real    a1, a2, k1, k2, f1root, f2root, hulp, n1, n2, rmax,&
   &w2

!
! declare function
   real    FLF3IO

   w2     = w * w

!---  set minimal value on r1:  ----------------------------------------

   rmax   = max(0.1, 10 * ks1)
   if (r1 .lt. rmax) then
      r1 = rmax
      a1 = w2 * r1 / ( w - 2 * (npier+1)* r1)
   else
      a1 = w * h1le
   endif
   hulp   = 12 * r1 / ks1
   f1root = 1. / (2 * alog10(hulp))
   n1     = r1**(1./6) * f1root / 8.86
   k1     = a1 * r1**(2./3) / n1

!---  set minimal value on r2:  ----------------------------------------

   rmax = max(0.1, 10 * ks2)
   if (r2 .lt. rmax) then
      r2 = rmax
      a2 = r2 * w2 / (w - 2 *(npier+1)* r2)
   else
      a2 = w * h2le
   endif
   hulp   = 12 * r2 / ks2
   f2root = 1. / (2 * alog10(hulp))
   n2     = r2**(1./6) * f2root / 8.86
   k2     = a2 * r2**(2./3) / n2

   FLF3IO  = l * q * q / k1 / k2

   return
end
