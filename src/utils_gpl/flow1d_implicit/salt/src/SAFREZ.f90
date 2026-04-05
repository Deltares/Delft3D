function safrez (nqfloc ,ngrid ,qfloc ,qq )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SAFREZ (SAlt FREsh water discharge Zwendl)
!
! Module description: Summation of fresh water discharges at all locati-
!                     ons that belong to a mouth (ZWENDL dispersion
!                     formulation).
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 ngrid             I  Number of grid points in network.
!  1 nqfloc            I  Number of Q fresh locations (empirical formu-
!                         lation of dispersion).
!  3 qfloc(2,nqfloc)   I  Locations and signs of fresh water discharges
!                         (Empirical formulation of dispersion):
!                         (1,i) = Fresh water discharge location i (grid
!                                 point).
!                         (2,i) = Sign of fresh water discharge location
!                                 i (-1 or 1).
!  4 qq(ngrid)         I  Discharge in every grid point.
!  0 safrez            O  Fresh water discharge at mouth.
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: safrez.pf,v $
! Revision 1.2  1995/05/30  07:06:04  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:46  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:13  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer  nqfloc    ,ngrid
   real     safrez
   real     qfloc(2,*)
   double precision qq(ngrid)
!
!     Declaration of local parameters
!
   integer  ifl   ,igr
   real     q
!
   q = 0.
   do 10 ifl = 1,nqfloc
      igr = int(qfloc(1,ifl))
      q   = q + qq(igr) * qfloc(2,ifl)
10 continue
   safrez = q
!
end
