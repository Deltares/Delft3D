subroutine gstfeh (initra ,nfrac ,g      ,pacfac ,relden ,chezy ,&
&velo   ,dfrac ,frcnf1 ,sedtra )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSTFEH (Graded Sediment Transp. Form. Engelund & Hansen)
!
! Module description: Calculate the sediment transport according to
!                     Engelund & Hansen
!
! Precondition:       Depth > 0.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 chezy             I  Chezy value
!  8 dfrac             I  Grain sizes per fraction
!  9 frcnf1            IO Constant 1 in transport formula.
!  3 g                 I  Acceleration of gravity.
!  1 initra            I  True for initialization of transport formulas
!                         (calculation of constants) else False.
!  2 nfrac             I  number of fractions
!  4 pacfac            I  packing factor (porosity)
!  5 relden            I  relative density
! 10 sedtra            O  calculated sediment transport
!  7 velo              I  velocity (without sign)
!=======================================================================
!
!     Declaration of parameters
!
   integer    nfrac
   real       g      ,pacfac ,relden ,chezy  ,velo
   real       dfrac( nfrac)  ,frcnf1(nfrac)  ,sedtra(nfrac)
   logical    initra
!
!
!     Declaration of local parameters
!
   integer    i
   real       uc

   if (initra) then
!
!        Calculation of constants in time per fraction.
!
      do i=1,nfrac
         frcnf1(i) = 0.05 /&
         &(dfrac(i) * (1.-pacfac) * sqrt(g) * relden**2)
      enddo
   else
!
!        Calculation of transport.
!
      uc = velo**5 / chezy**3
      do i=1,nfrac
         sedtra(i) = frcnf1(i) * uc
      enddo
   endif
!
end
