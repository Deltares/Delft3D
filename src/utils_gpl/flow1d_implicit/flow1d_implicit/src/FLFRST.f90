function FLFRST (q, l, r1, a1, ks1, r2, a2, ks2)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLow calculate FRiction in STructures
!
! Module description: Function is taken from WENDY as part of the new
!                     structure functionality. WENDY history is:
!                        Project: Construction-Module
!                        Programmer: G. van Driel
!                        Function: Calculation of the friction head
!                        Updates: None
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
! $Log: flfrst.pf,v $
! Revision 1.2  1998/06/08  12:35:22  kuipe_j
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
!      1      q         I     discharge
!      2      l         I     length
!      3      r1        I     hydraulic radius
!      4      a1        I     wet area
!      5      ks1       I     roughness coefficient
!      6      r2        I     hydraulic radius
!      7      a2        I     wet area
!      8      ks2       I     roughness coefficient
!
!  The function is used for structure type VI,VII,VIII and IX
!
!***********************************************************************

!
! declare arguments
   real q, l, a1, r1, ks1, a2, r2, ks2, k1, k2

!
! declare variables
   real f1root, f2root, hulp, n1, n2

!
! declare functions
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


