subroutine setfud (initra ,g      ,pacfac ,relden ,d50   ,d90    ,&
&chezy  ,uscofb ,velo   ,hrad  ,forcn1 ,forcn2 ,&
&sedtra )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SETFUD (SEdiment Transport Formula User Defined)
!
! Module description: Calculate the sediment transport according user
!                     specified coefficients.
!                     [S-FO-002.2KV / Eq. 2.33, 2.6, 2.7, 2.10 and 2.11]
!
! Precondition:       Depth > 0.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 chezy             I  Chezy value
!  5 d50               I  D50
!  6 d90               I  D90
! 11 forcn1            IO Constant 1 in transport formula.
! 12 forcn2            IO Constant 2 in transport formula.
!  2 g                 I  Acceleration of gravity.
! 10 hrad              I  hydraulic radius
!  1 initra            I  True for initialization of transport formulas
!                         (calculation of constants) else False.
!  3 pacfac            I  packing factor (porosity)
!  4 relden            I  relative density
! 13 sedtra            O  calculated sediment transport
!  8 uscofb(6)         I  Defines user coefficients for a branch with a
!                         user defined transport formula:
!                         (1) =   ALPHA
!                         (2) =   BETA
!                         (3) =   GAMMA
!                         (4) =   Shields-factor-critical
!                         (5) =   Flag indicates calculation of ripple
!                                 factor MU:
!                                 crfcon (0) : Use ripple constant
!                                 crfcal (1) : Calculate ripple
!                         (6) =   Ripple constant in case uscoef(5) = 0.
!  9 velo              I  velocity (without sign)
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: setfud.pf,v $
! Revision 1.4  1998/02/25  12:49:02  kuipe_j
! Check on grain size added
!
! Revision 1.3  1995/05/30  09:56:33  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:07:37  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:34  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:08  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:24  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   real       g      ,pacfac ,relden ,d50    ,d90    ,chezy  ,&
   &velo   ,hrad   ,forcn1 ,forcn2 ,sedtra
   real       uscofb (6)
   logical    initra
!
!     Declaration of local parameters
!
   real       c90    ,mu1    ,mushil ,arg
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   if (initra) then
!
!        Calculation of constants in time.
!
      if (d50 .lt. 1.e-6 .or. d90 .lt. 1.e-6) then
!         A negative value is used to check on grain size
         forcn1 = -1001.
      else
         forcn1 = uscofb(2) / (1.-pacfac) * sqrt(g*relden*d50) * d50
         if ( int(uscofb(5)) .eq. crfcal ) then
!
!           Ripple factor will be calculated
!
            forcn2 = 1. / (relden * d50)
         else
!
!           Ripple factor is constant
!
            forcn2 = uscofb(6) / (relden * d50)
         endif
      endif
   else
!
!        Calculation of transport.
!
      if ( int(uscofb(5)) .eq. crfcal ) then
!
!           Calculate ripple factor.
!
         c90 = 18. * log10(4.*hrad / d90)
         mu1 = forcn2 * (chezy/c90)**1.5
      else
         mu1 = forcn2
      endif
      mushil = (velo / chezy)**2 * mu1
      arg    = mushil - uscofb(4)
!
!        Test if threshold will be exceeded.
!
      if (arg .gt. 0.) then
         sedtra = forcn1 * mushil**uscofb(3) * arg**uscofb(1)
      else
         sedtra = 0.
      endif
   endif
!
end
