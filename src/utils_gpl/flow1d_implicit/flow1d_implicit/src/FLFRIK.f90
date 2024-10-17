function FLFRIK (r,sk,a)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLow FRIction head K1ioc
!
! Module description: Calculate the friction head k1ioc for use in the
!                     structure routines. This routine is taken from
!                     WENDY. WENDY history is:
!                       Projekt: Construction-Module
!                       Programmeur: H. van Zanten
!                       Funktie: Calculation of the factor K in the
!                       friction head
!                       No updates
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
! $Log: flfrik.pf,v $
! Revision 1.2  1998/06/08  12:35:19  kuipe_j
! log added
!
!
!
!***********************************************************************
!
!  Parameters  (Input / Output) :
!  ------------------------------
!
!   Number    Name         I/O    Description
!   ------    ----         ---    ------------
!      1      r             I     hydralic radius
!      2      sk            I     roughness coefficient
!      3      a             I     wet area
!
!  The function is used for structure type IV
!
!***********************************************************************
!  Aanroepen  (Funkties  en / of  Subroutines) : none
!  ---------------------------------------------
!***********************************************************************

!
! declare arguments
   real   r, sk, a

!
! declare local variables
   real   hulp, f1root, rn1

!
! declare function
   real   FLFRIK

   hulp   = 12. * r / sk
   f1root = 1. / (2. * LOG10(hulp))
   rn1    = r**(1./6.) * f1root / 8.86
   FLFRIK = a * r**(2./3.) / rn1

   return
end
