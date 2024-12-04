subroutine setfmm (initra ,g      ,pacfac ,relden ,dmed  ,d90    ,&
&chezy  ,velo   ,hrad   ,forcn1 ,sedtra)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SETFMM (SEdiment Transp. Form. Meyer-Peter & Muller)
!
! Module description: Calculate the sediment transport according to
!                     Meyer-Peter & Muller.
!                     [ Doc. S-FO-002.2KV / Eq. 2.10 - 2.12 ]
!
! Precondition:       Depth > 0.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 chezy             I  Chezy value
!  6 d90               I  D90
!  5 dmed              I  Dmedium
! 10 forcn1            IO Constant 1 in transport formula.
!  2 g                 I  Acceleration of gravity.
!  9 hrad              I  hydraulic radius
!  1 initra            I  True for initialization of transport formulas
!                         (calculation of constants) else False.
!  3 pacfac            I  packing factor (porosity)
!  4 relden            I  relative density
! 11 sedtra            O  calculated sediment transport
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
! $Log: setfmm.pf,v $
! Revision 1.3  1998/02/25  12:49:00  kuipe_j
! Check on grain size added
!
! Revision 1.2  1995/05/30  07:07:34  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:32  hoeks_a
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
   real       g      ,pacfac ,relden ,dmed   ,d90    ,chezy ,velo   ,&
   &hrad   ,forcn1 ,sedtra
   logical    initra
!
!     Declaration of local parameters
!
   real       c90    ,mushil ,arg
!
   if (initra) then
!
!        Calculation of constants in time.
!
      if (dmed .lt. 1.e-6 .or. d90 .lt. 1.e-6) then
!           A negative value is used to check on grain size
         forcn1 = -1001.
      else
         forcn1 = (8. * sqrt(g * relden * dmed**3)) / (1.-pacfac)
      endif
   else
!
!        Calculation of transport.
!
      c90    = 18. * log10(4.*hrad / d90)
      mushil = velo**2 / (c90 * sqrt(c90 * chezy) * relden * dmed)
      arg    = mushil - 0.047
!
!        Test if threshold will be exceeded.
!
      if (arg .gt. 0.) then
         sedtra = forcn1 * arg**1.5
      else
         sedtra = 0.
      endif
   endif
!
end
