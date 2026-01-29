subroutine gstfwb (initra ,nfrac  ,pacfac ,dmed   ,velo  ,wacofb ,&
&sedtra ,sedexp )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSTFWB (Grad Sed Transp WaalBocht )
!
! Module description: Calculate the sediment transport according to
!                     Waalbocht formulae.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 chezy             I  Chezy value
!  5 dmed              I  Dmedium
!  2 g                 I  Acceleration of gravity.
!  3 pacfac            I  packing factor (porosity)
! 11 sedtra            O  calculated sediment transport
!  8 velo              I  velocity (without sign)
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gstfwb.F,v $
! Revision 1.3  1996/06/07  11:56:39  kuipe_j
! multi  +  fixed layer
!
! Revision 1.2  1995/09/27  10:12:59  kuipe_j
! Maintenance
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nfrac
   real       pacfac ,dmed   ,velo  ,sedexp
   real       wacofb (3)     ,&
   &sedtra (nfrac)
!
   logical    initra
!
!     Declaration of local parameters
!
   integer    i
   real       m      ,n      ,l     ,s
!
!     Calculation of transport.
!     Start with the computation of fraction independent constants
!
   m = wacofb(1)
   n = wacofb(2)
   l = wacofb(3)
!
   sedexp = n / 2.
   if (initra) return

   s = m * velo**n / (dmed**l * (1. - pacfac))

   do 10 i=1,nfrac
      sedtra(i) = s
10 continue

end
