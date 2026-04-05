function FLF31I (w, wpwn, q, l, r1, ks1, alfa, r2, ks2, npier,&
&h1le)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLF31I, Flow Friction head 31I
!
! Module description: Calculate the friction head 31I
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
! $Log: flf31i.pf,v $
! Revision 1.2  1998/06/08  12:35:14  kuipe_j
! log added
!
!
!
!***********************************************************************
!  Parameters  (Input / Output) :
!  ------------------------------
!
!   Number    Name        I/O    Description
!   ------    ----        ---    ------------
!      1      w            I     net discharge width of sluice
!      2      wpwn         I     total width of sluice
!      3      q            I     discharge
!      4      l            I     length of approach section
!      5      r1          I/O    hydraulic radius approach section
!      6      ks1          I     roughness value  approach section
!      7      alfa         I     ratio width of approach section vs.
!                                total width of sluice
!      8      r2          I/O    hydraulic radius inlet
!      9      ks2          I     roughness value sluice section upstream
!     10      npier        I     number of piers
!     11      h1le         I     waterdepth upstream
!
!  The function is used for structure type III
!
!***********************************************************************
!  Aanroepen  (Funkties  en / of  Subroutines) : none
!  ---------------------------------------------
!***********************************************************************

!
! declare arguments
   integer npier
   real    w, wpwn, q, l, r1, ks1, alfa, r2, ks2, h1le

!
! declare local variables
   real    a1,   a2,  k1, k2, f1root, f2root, hulp, n1, n2, rmax,&
   &w2

!
! declare function
   real FLF31I

   w2     = w * w

!
! set minimum value for r1
   rmax   = MAX(0.1, 10 * ks1)

   if (r1 .lt. rmax) then
      r1 = rmax
      a1 = alfa * wpwn * r1
   else
      a1 = r1 * alfa * wpwn
   endif

   hulp   = 12 * r1 / ks1
   f1root = 1. / (2 * LOG10(hulp))
   n1     = r1**(1./6) * f1root / 8.86
   k1     = a1 * r1**(2./3) / n1

!
! set minimum value for r2
   rmax = MAX(0.1, 10 * ks2)

   if (r2 .lt. rmax) then
      r2 = rmax
      a2 = r2 * w2 / (w - 2 * (npier+1) * r2)
   else
      a2 = w * h1le
   endif

   hulp   = 12 * r2 / ks2
   f2root = 1. / (2 * LOG10(hulp))
   n2     = r2**(1./6) * f2root / 8.86
   k2     = a2 * r2**(2./3) / n2

   FLF31I  = l * q * q / k1 / k2

   return
end
