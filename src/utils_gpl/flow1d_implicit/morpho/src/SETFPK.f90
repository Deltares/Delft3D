subroutine setfpk (initra ,g      ,pacfac ,relden ,d50    ,chezy ,&
&velo   ,forcn1 ,forcn2 ,sedtra )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SETFPK (SEdiment Transp. Form. Parker & Klingeman)
!
! Module description: Calculate the sediment transport according to
!                     Parker & Klingeman.
!                     [ Doc. S-FO-002.2KV / Eq. 2.27 - 2.32 and 2.7 ]
!
! Precondition:       Depth > 0.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 chezy             I  Chezy value
!  5 d50               I  D50
!  8 forcn1            IO Constant 1 in transport formula.
!  9 forcn2            IO Constant 2 in transport formula.
!  2 g                 I  Acceleration of gravity.
!  1 initra            I  True for initialization of transport formulas
!                         (calculation of constants) else False.
!  3 pacfac            I  packing factor (porosity)
!  4 relden            I  relative density
! 10 sedtra            O  calculated sediment transport
!  7 velo              I  velocity (without sign)
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: setfpk.pf,v $
! Revision 1.3  1998/02/25  12:49:01  kuipe_j
! Check on grain size added
!
! Revision 1.2  1995/05/30  07:07:36  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:33  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:23  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   real       g      ,pacfac ,relden ,d50    ,chezy  ,&
   &velo   ,forcn1 ,forcn2 ,sedtra
   logical    initra
!
!     Declaration of local parameters
!
   real       omega  ,omega1 ,dlbedl
!
!     Constants
!
   real       shielr
   parameter (shielr=0.0876)
!
   if (initra) then
!
!        Calculation of constants in time.
!
      if (d50 .lt. 1.e-6 ) then
!           A negative value is used to check on grain size
         forcn1 = -1001.
      else
         forcn1 = sqrt(g) / ((1.-pacfac) * relden)
         forcn2 = 1. / (relden * d50 * shielr)
      endif
   else
!
!        Calculation of transport.
!        First determine ratio (omega) of shields parameter Theta
!        and Theta-r.
!
      omega = forcn2 * (velo / chezy)**2
!
!        Compute dimensionless bed load (dlbedl).
!        The computation is different for 3 regions of the ratio omega.
!        The transport is calculated thereafter.
!
      if ( omega .le. 0.95 ) then
         sedtra = 0.0
      else
         if ( omega .le. 1.65 ) then
            omega1 = omega - 1.
            dlbedl = 0.0025 * exp(14.2*omega1 - 9.28*omega1**2)
         else
            dlbedl = 11.2 * (1. - 0.822/omega)**4.5
         endif
         sedtra = forcn1 * dlbedl * (velo / chezy)**3
      endif
   endif
!
   return
end
