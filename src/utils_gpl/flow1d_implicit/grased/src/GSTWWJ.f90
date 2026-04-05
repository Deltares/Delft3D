subroutine gstwwj (initra ,nfrac  ,g     ,pacfac ,relden ,kinvis ,&
&d50    ,chezy  ,velo  ,hrad   ,dfrac  ,frcnf1 ,&
&frcnf2 ,frcnf3 ,sedtra,wwjcof ,hidexpf)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSTFWWJ (Grad Sed Transp Form Wu, Wang and Jia)
!
! Module description: Calculate the sediment transport according to
!                     Ashida & Michiue for graded material.
!
!                     [ memo Flokstar ,May 2001 ]
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
! frcnf3            IO Constant 2 in transport formula per fraction.
! g                 I  Acceleration of gravity.
! hidexpf           I  Hiding and exposure factor per fraction
! hrad              I  hydraulic radius
! initra            I  True for initialization of transport formulas
!                      (calculation of constants) else False.
! kinvis            I  kinematic viscosity
! nfrac             I  number of fractions
! pacfac            I  packing factor (porosity)
! relden            I  relative density
! sedtra            O  calculated sediment transport
! velo              I  velocity (without sign)
! wwjcof (1)        I  Critical theta
!=======================================================================
!
!     Declaration of parameters
!
   integer    nfrac
   real       g      ,pacfac ,relden ,d50   ,kinvis ,chezy  ,velo  ,&
   &hrad
   real       dfrac  (nfrac) ,frcnf1 (nfrac) ,frcnf2 (nfrac),&
   &frcnf3 (nfrac) ,sedtra (nfrac) ,hidexpf(nfrac),&
   &wwjcof (1)
   logical    initra
!
!     Declaration of local parameters
!
   integer    i
   real       ksi    ,usterkw ,taubci, term ,sbiacc, ssiacc, mu ,&
   &thetac
!
   if (initra) then
!
!        Calculation of constants in time.
!
      if (wwjcof(1) .eq. 0.) then
         thetac = 0.03
      else
         thetac = wwjcof(1)
      endif
      do i=1,nfrac
         frcnf1(i) = 1.0 / (g * relden * dfrac(i) * thetac)
         frcnf2(i) = sqrt(g * relden * dfrac(i)**3)&
!                       It is assumed that the formula is derived for
!                       a packing factor of 0.4 .
         &* (1.0 - 0.4) / (1.-pacfac)
!           Calculation of Omega(i)
         term      = 13.95 * kinvis / dfrac(i)
         frcnf3(i) = sqrt(term**2 + 1.09 * g * relden * dfrac(i))&
         &- term
      enddo
   else
!
!        Calculation of transport.
!        Start with the computation of fraction independent constants
!
      usterkw  = (velo/chezy)**2 * g
      mu       = ((d50/hrad)**(1./6.) * chezy / 20.)**1.5

      do i=1,nfrac
!
!           Get hiding effect.
!
         ksi  = hidexpf(i)
!
         taubci = usterkw * frcnf1(i) / ksi
         term   = mu*taubci
         if (term .le. 1.0) then
!
!              Test if threshold will be exceeded for bottom transport.
!
            sbiacc = 0.
         else
            sbiacc = 0.0053 * ( term - 1.0)**2.2
         endif
         if (taubci .le. 1.0) then
!
!              Test if threshold will be exceeded for suspended transport.
!
            ssiacc = 0.
         else
            ssiacc = 0.0000262 *&
            &( velo / frcnf3(i) * (taubci - 1.0))**1.74
         endif
         sedtra(i) = frcnf2(i) * (sbiacc + ssiacc)
      enddo

   endif
!
end
