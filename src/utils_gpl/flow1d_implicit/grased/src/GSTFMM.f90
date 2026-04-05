subroutine gstfmm (initra ,nfrac  ,g     ,pacfac ,relden ,&
&d90    ,chezy  ,velo  ,hrad   ,dfrac  ,&
&frcnf1 ,sedtra ,mpmcof,hidexpf)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSTFMM (Grad Sed Transp Form Meyer-Peter & Muller)
!
! Module description: Calculate the sediment transport according to
!                     Meyer-Peter & Muller for graded material.
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
!  d90               I  D90
!  frcnf1            IO Constant 1 in transport formula per fraction.
!  g                 I  Acceleration of gravity.
!  hidexpf           I  Hiding and exposure factor per fraction
!  hrad              I  hydraulic radius
!  initra            I  True for initialization of transport formulas
!                       (calculation of constants) else False.
!  mpmcof(1)         I  (1) = alpha90
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
   real       g      ,pacfac ,relden ,d90    ,chezy ,velo   ,&
   &hrad
   real       dfrac  (nfrac) ,frcnf1 (nfrac),&
   &sedtra (nfrac) ,hidexpf(nfrac), mpmcof(1)
   logical    initra
!
!     Declaration of local parameters
!
   integer    i
   real       c90    ,mushil ,arg  ,ksi, alf90
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
      alf90  = mpmcof(1)
      if (alf90 .gt. 0.) then
         arg = 12.0*hrad / (d90*alf90)
      else
         arg = 4.0*hrad / d90
      endif
      c90    = 18. * log10(arg)
      mushil = velo**2 / (c90 * sqrt(c90 * chezy) * relden )
      do 20 i=1,nfrac
!
!           Get hiding effect.
!
         ksi = hidexpf(i)
!
         arg = mushil / dfrac(i) - ksi * 0.047
!
!           Test if threshold will be exceeded.
!
         if (arg .gt. 0.) then
            sedtra(i) = frcnf1(i) * arg**1.5
         else
            sedtra(i) = 0.
         endif
20    continue

   endif
!
end
