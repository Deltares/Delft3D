subroutine gstfm1 (initra ,nfrac  ,g     ,pacfac ,relden ,&
&chezy  ,velo   ,dfrac ,frcnf1 ,sedtra ,&
&mpmcof ,hidexpf)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSTFMM (Grad Sed Transp Form Meyer-Peter & Muller 1)
!
! Module description: Calculate the sediment transport according to
!                     Meyer-Peter & Muller for graded material.
!                     Hiding effect according to Egiazaroff, Ashida
!                     Michiue. Input of Ripple factor and critical
!                     Shields parameter
!
! Precondition:       Depth > 0.
!                     Hiding and exposure effect already calculated and
!                     stored in array Hidexpf.
!
!-----------------------------------------------------------------------
! Parameters:
!  NAME              IO DESCRIPTION
!  chezy             I  Chezy value
!  dfrac             I  Grain sizes per fraction
!  g                 I  Acceleration of gravity.
!  hrad              I  hydraulic radius
!  hidexpf           I  Hiding and exposure factor per fraction
!  initra            I  True for initialization of transport formulas
!                       (calculation of constants) else False.
!  mpmcof(2)         I  (1) = Ripple factor
!                       (2) = Critical Shields parameter
!  nfrac             I  number of fractions
!  pacfac            I  packing factor (porosity)
!  relden            I  relative density
!  sedtra            O  calculated sediment transport
!  velo              I  velocity (without sign)
!=======================================================================
!
!     Declaration of parameters
!
   integer    nfrac
   real       g      ,pacfac ,relden ,chezy ,velo
   real       dfrac  (nfrac) ,frcnf1 (nfrac) ,&
   &sedtra (nfrac) ,hidexpf(nfrac) ,mpmcof (2)
   logical    initra
!
!     Declaration of local parameters
!
   integer    i
   real       mu     ,mushil ,arg  ,ksi
!
   if (initra) then
!
!        Calculation of constants in time.
!
      do 10 i=1,nfrac
         frcnf1(i) = 8. * sqrt(g * relden * dfrac(i)**3)&
         &/ (1.-pacfac)
10    continue
   else
!
!        Calculation of transport.
!        Start with the computation of fraction independent constants
!
      mu     = mpmcof(1)
      mushil = velo**2 * mu / (chezy**2 * relden )
!
      do 20 i=1,nfrac
!
!           Get hiding effect.
!
         ksi = hidexpf(i)

         arg = mushil / dfrac(i) - ksi * mpmcof(2)
!
!           Test if threshold will be exceeded.
!
         if (arg .gt. 0.) then
            sedtra(i) = frcnf1(i) * arg**1.5
         else
            sedtra(i) = 0.
         endif
20    continue
!
   endif
!
end
