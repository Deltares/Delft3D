subroutine setfvr (initra ,g      ,pacfac ,relden ,kinvis ,d50   ,&
&d90    ,velo   ,depth  ,hrad   ,forcn1 ,forcn2,&
&forcn3 ,forcn4 ,sedtra )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SETFVR (SEdiment Transport Formula Van Rijn)
!
! Module description: Calculate the sediment transport according to Van
!                     Rijn.
!                     [ Doc. S-FO-002.2KV / Eq. 2.20 - 2.26 ]
!
! Precondition:       Depth > 0.
!                     100*e-6 < D50 < 2000*e-6
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 d50               I  D50
!  7 d90               I  D90
!  9 depth             I  avarage depth
! 11 forcn1            IO Constant 1 in transport formula.
! 12 forcn2            IO Constant 2 in transport formula.
! 13 forcn3            IO Constant 3 in transport formula.
! 14 forcn4            IO Constant 4 in transport formula.
!  2 g                 I  Acceleration of gravity.
! 10 hrad              I  hydraulic radius
!  1 initra            I  True for initialization of transport formulas
!                         (calculation of constants) else False.
!  5 kinvis            I  kinematic viscosity
!  3 pacfac            I  packing factor (porosity)
!  4 relden            I  relative density
! 15 sedtra            O  calculated sediment transport
!  8 velo              I  velocity (without sign)
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: setfvr.pf,v $
! Revision 1.3  1998/02/25  12:49:03  kuipe_j
! Check on grain size added
!
! Revision 1.2  1995/05/30  07:07:38  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:34  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:24  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   real       g      ,pacfac ,relden ,kinvis ,d50    ,d90    ,velo  ,&
   &depth  ,hrad   ,forcn1 ,forcn2 ,forcn3 ,forcn4 ,sedtra
   logical    initra
!
!     Declaration of local parameters
!
   real       uc     ,velouc ,dg     ,part   ,d50dep
!
   if (initra) then
!
!        Calculation of constants in time.
!        First calculate dimensionless grain size Dg.
!
      if (d50 .lt. 1.e-6 .or. d90 .lt. 1.e-6) then
!          A negative value is used to check on grain size
         forcn1 = -1001.
      else
         dg     = d50 * (g * relden / kinvis**2)**(1./3.)
         forcn1 = .005 / (1.-pacfac) / (g * relden * d50)**1.2
         forcn2 = .012 / (1.-pacfac) / (g * relden * d50)**1.2 /&
         &dg**.6
         forcn3 = .19 * d50**.1
         forcn4 = 8.5 * d50**.6
      endif
   else
!
!        Calculation of transport.
!        First determine range of D50 and compute critical
!        flow velocity uc.
!
      if ( d50 .le. 500.e-6) then
         uc = forcn3 * log10(4.*hrad / d90)
      else
         uc = forcn4 * log10(4.*hrad / d90)
      endif
      velouc = velo - uc
!
!        Test if threshold will be exceeded.
!
      if (velouc .gt. 0.) then
!
!           Compute same part for bed and suspended load
!
         part   = velouc**2.4 * velo * depth
         d50dep = d50 / depth
!
!           Compute bed load (bload) and suspended load (sload)
!
         sedtra = part * (forcn1 * d50dep**1.2 + forcn2 * d50dep)
      else
         sedtra = 0.
      endif
   endif
!
end
