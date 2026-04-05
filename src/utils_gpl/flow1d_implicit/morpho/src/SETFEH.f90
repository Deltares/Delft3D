subroutine setfeh (initra,g     ,pacfac ,relden ,d50    ,chezy ,&
&velo  ,forcn1,sedtra )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SETFEH (SEdiment Transp. Form. Engelund & Hansen)
!
! Module description: Calculate the sediment transport according to
!                     Engelund & Hansen
!                     [ Doc. S-FO-002.2KV / Eq. 2.8 ]
!
! Precondition:       Depth > 0.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 chezy             I  Chezy value
!  5 d50               I  D50
!  8 forcn1            IO Constant 1 in transport formula.
!  2 g                 I  Acceleration of gravity.
!  1 initra            I  True for initialization of transport formulas
!                         (calculation of constants) else False.
!  3 pacfac            I  packing factor (porosity)
!  4 relden            I  relative density
!  9 sedtra            O  calculated sediment transport
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
! $Log: setfeh.pf,v $
! Revision 1.4  1999/03/15  15:53:41  kuipe_j
! tabs removed
!
! Revision 1.3  1998/02/25  12:48:59  kuipe_j
! Check on grain size added
!
! Revision 1.2  1995/05/30  07:07:33  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:31  hoeks_a
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
   real       g      ,pacfac ,relden ,d50    ,chezy  ,velo   ,&
   &forcn1 ,sedtra
   logical    initra
!
   if (initra) then
!
!        Calculation of constants in time.
!
      if (d50 .lt. 1.e-6) then
!           A negative value is used to check on grain size
         forcn1 = -1001.
      else
         forcn1 = .05 / (d50 * (1.-pacfac) * sqrt(g) * relden**2)
      endif
   else
!
!        Calculation of transport.
!
      sedtra = forcn1 * velo**5 / chezy**3
   endif
!
end
