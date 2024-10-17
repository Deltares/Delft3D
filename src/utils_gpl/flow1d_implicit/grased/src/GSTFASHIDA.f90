subroutine gstfashida (initra ,nfrac  ,g      ,pacfac ,relden ,&
&dmed   ,chezy  ,velo   ,hrad   ,dfrac  ,&
&frcnf1 ,frcnf2 ,sedtra ,ashcof ,hidexpf)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSTFASHIDA (Grad Sed Transp Form ASHIDA & Michiue)
!
! Module description: Calculate the sediment transport according to
!                     Ashida & Michiue for graded material.
!                     [ memo Sloff ,Jan 2001 ]
!
! Precondition:       velo > 0.
!                     Hiding and exposure effect already calculated and
!                     stored in array Hidexpf.
!
!-----------------------------------------------------------------------
! Parameters:
! NAME              IO DESCRIPTION
! chezy             I  Chezy value
! dfrac             I  Grain sizes per fraction
! dmed              I  Dmedium grain size
! frcnf1            IO Constant 1 in transport formula per fraction.
! frcnf2            IO Constant 2 in transport formula per fraction.
! g                 I  Acceleration of gravity.
! hidexpf           I  Hiding and exposure factor per fraction
! hrad              I  hydraulic radius
! initra            I  True for initialization of transport formulas
!                      (calculation of constants) else False.
! ashcof(1)         I  (1) = shear stress for Dm
! nfrac             I  number of fractions
! pacfac            I  packing factor (porosity)
! relden            I  relative density
! sedtra            O  calculated sediment transport
! velo              I  velocity (without sign)
!=======================================================================
!
!     Declaration of parameters
!
   integer    nfrac
   real       g      ,pacfac ,relden ,dmed   ,chezy ,velo   ,&
   &hrad
   real       dfrac  (nfrac) ,frcnf1 (nfrac) ,frcnf2 (nfrac),&
   &sedtra (nfrac) ,hidexpf(nfrac) ,ashcof(1)
   logical    initra
!
!     Declaration of local parameters
!
   integer    i
   real       ksi    ,ksitot ,usterkw ,tauascm,&
   &tauasm ,tauasi ,tauasei,uase
!
   if (initra) then
!
!        Calculation of constants in time.
!
      do i=1,nfrac
         frcnf1(i) = 1. / (g * relden * dfrac(i))
         frcnf2(i) = 17. * sqrt(g * relden * dfrac(i)**3)&
         &/ (1.-pacfac)
      enddo
   else
!
!        Calculation of transport.
!        Start with the computation of fraction independent constants
!
      usterkw  = (velo/chezy)**2 * g
      tauasm   = usterkw / (g * relden * dmed)
      tauascm  = ashcof(1)
      do i=1,nfrac
!
!           Get hiding effect.
!
         ksi     = hidexpf(i)
!
!           Use constant user specified shear stress for Dm
!
         tauasi  = usterkw * frcnf1(i)
         uase    = velo / (6.0 + 2.5 *&
         &log(hrad/(dmed*(1.0 + 2.0*tauasm))))
         tauasei = uase**2 * frcnf1(i)
         ksitot  = ksi * tauascm / tauasi
         if (ksitot .ge. 1.0) then
!
!              Test if threshold will be exceeded.
!
            sedtra (i) = 0.
         else
            sedtra(i) = frcnf2(i) * tauasei**1.5 *&
            &(1.0 - ksitot) * (1.0 - sqrt(ksitot))
         endif
      enddo

   endif
!
end
