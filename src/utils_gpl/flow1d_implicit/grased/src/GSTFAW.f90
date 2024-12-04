subroutine gstfaw (initra ,nfrac  ,g      ,pacfac ,relden ,&
&kinvis ,chezy  ,velo   ,depth  ,dfrac  ,&
&frcnf1 ,frcnf2 ,frcnf3 ,frcnf4 ,sedtra ,&
&hidexpf)
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSTFAW (Graded Sediment Transport Formula Ackers & White)
!
! Module description: Calculate the sediment transport according to
!                     Ackers & White.
!
! Precondition:       Depth > 0.
!                     Hiding and exposure effect already calculated and
!                     stored in array Hidexpf.
!
!-----------------------------------------------------------------------
! Parameters:
! NAME              IO DESCRIPTION
! chezy             I  Chezy value
! dfrac             I  Grain sizes per fraction
! depth             I  avarage depth
! frcnf1            IO Constant 1 in transport formula (Dg).
! frcnf2            IO Constant 2 in transport formula (n).
! frcnf3            IO Constant 3 in transport formula.
! frcnf4            IO Constant 4 in transport formula.
! hidexpf           I  Hiding and exposure factor per fraction
!                   I  Acceleration of gravity.
! initra            I  True for initialization of transport formulas
!                      (calculation of constants) else False.
! kinvis            I  kinematic viscosity
! nfrac             I  number of fractions
! pacfac            I  packing factor (porosity)
! relden            I  relative density
! sedtra            O  calculated sediment transport
! velo              I  velocity (without sign)
!=======================================================================
!
!
!     Declaration of parameters
!
   integer    nfrac
   real       g      ,pacfac ,relden ,kinvis ,chezy  ,&
   &velo   ,depth
   real       dfrac( nfrac)  ,frcnf1(nfrac)  ,frcnf2(nfrac) ,&
   &frcnf3(nfrac)  ,frcnf4(nfrac)  ,sedtra(nfrac) ,&
   &hidexpf(nfrac)
   logical    initra
!
!     Declaration of local parameters
!
   integer    i
   real       dg     ,fgr    ,fgrea  ,n      ,k     ,m      ,&
   &a      ,eps    ,cpown
!
   if (initra) then
!
!        Calculation of constants in time per fraction.
!
      do i=1,nfrac
         dg = dfrac(i) * (g * relden / kinvis**2)**(1.0/3.0)
!           If (dg.LT.60.0) Then
         If (dg.LT.61.05402) Then
!           If (dg.LT.1.e10) Then
            n  = 1.0 - 0.56*log10(dg)
            k  = exp(2.86*log(dg) - 0.434*log(dg)**2 - 8.13)
         Else
!              dg = 60.0
!              n  =  0.0
!              k  =  0.0248325
            dg = 61.05402
            n  =  0.0
            k  =  0.0245310
         Endif
         frcnf1(i) = dg
         frcnf2(i) = n
         frcnf3(i) = dfrac(i) * k  / (1.0-pacfac) * g**(-0.5*n)
         frcnf4(i) = (g * 32.)**((n-1.0)*0.5) /&
         &sqrt( relden * dfrac(i))
      enddo
   else
!
!        Calculation of transport per fraction.
!
      do i=1,nfrac
!
!           First calculate dimensionless grain size Dg.
!
         dg = frcnf1(i)
         n  = frcnf2(i)
         a  = 0.23/sqrt(dg) + 0.14
         m  = 9.66/dg +1.34
!
!           Get hiding and exposure factor Eps
!
         eps = 1./sqrt(hidexpf(i))
!
         cpown = chezy**n
         fgr   = frcnf4(i) * velo *&
         &log10(10.0 * depth / dfrac(i))**(n-1.0) / cpown
         fgrea = eps * fgr / a
         if (fgrea .gt. 1.) then
            sedtra(i) = frcnf3(i) * (fgrea -1.0)**m * velo * cpown
         else
            sedtra(i) = 0.
         endif
      enddo
   endif
!
end
