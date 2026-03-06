function FLFRIO (w, q, l, r1, ks1,alfa1, r2, ks2,alfa2,ski)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLow FRiction head IO in structure
!
! Module description: Calculates the friction in the structure (used
!                     only for the open flume). Code is taken from
!                     WENDY. History is:
!                       Project:  Construction-Module
!                       Programmer:  G. van Driel
!                       Function:  Calculation of the friction head
!                       Updates: none.
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
! $Log: flfrio.pf,v $
! Revision 1.2  1998/06/08  12:35:20  kuipe_j
! log added
!
!
!
!***********************************************************************
!  Parameters  (Input / Output) :
!  ------------------------------
!
!   Number    Name         I/O    Description
!   ------    ----         ---    ------------
!      1      w             I     width of flume
!      2      q             I     discharge
!      3      l             I     length of section
!      4      r1           I/O    hydraulic radius
!      5      ks1           I     roughness of section
!      6      alfa1         I     parameter
!      7      r2           I/O    hydraulic radius
!      8      ks2           I     roughness of section
!      9      alfa2         I     parameter
!     10      ski           O     Ki = Ai * Ri**(2./3.) / ni
!
!***********************************************************************

   real q, l, w, a1, r1, ks1, a2, r2, ks2, k1, k2
   real f1root, f2root, hulp, n1, n2, rmax
   real alfa1, alfa2, w2, ski

!
! declare function
   real FLFRIO

   w2     = w * w

!
! set minimum value for r1
   rmax   = MAX(0.1, 10 * ks1)
   if (r1 .lt. rmax) then
      r1 = rmax
      a1 = w2 * r1 / (w - 2.*r1)
   else
      a1 = alfa1 * w
   endif
   hulp   = 12. * r1 / ks1
   f1root = 1. / (2. * LOG10(hulp))
   n1     = r1**(1./6.) * f1root / 8.86
   k1     = a1 * r1**(2./3.) / n1

!
! set minimum value for r2
   rmax = MAX(0.1, 10 * ks2)
   if (r2 .lt. rmax) then
      r2 = rmax
      a2 = r2 * w2 / (w - 2.*r2)
   else
      a2 = alfa2 * w
   endif
   hulp   = 12 * r2 / ks2
   f2root = 1. / (2. * LOG10(hulp))
   n2     = r2**(1./6.) * f2root / 8.86
   k2     = a2 * r2**(2./3.) / n2
   ski    = k2

   FLFRIO  = l * q * q / k1 / k2

   return
end
