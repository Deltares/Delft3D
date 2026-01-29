subroutine setfaw (initra ,g      ,pacfac ,relden ,kinvis ,d35   ,&
&chezy  ,velo   ,depth  ,forcn1 ,forcn2,&
&forcn3 ,forcn4 ,sedtra )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SETFAW (SEdiment Transport Formula Ackers & White)
!
! Module description: Calculate the sediment transport according to
!                     Ackers & White.
!                     [ Doc. S-FO-002.2KV / Eq. 2.13 - 2.19 and 2.7 ]
!
! Precondition:       Depth > 0.
!                     Dimensionless grain size Dg > 1.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 chezy             I  Chezy value
!  6 d35               I  D35
!  7 d50               I  D50
! 10 depth             I  avarage depth
! 11 forcn1            IO Constant 1 in transport formula.
! 12 forcn2            IO Constant 2 in transport formula.
! 13 forcn3            IO Constant 3 in transport formula.
! 14 forcn4            IO Constant 4 in transport formula.
!  2 g                 I  Acceleration of gravity.
!  1 initra            I  True for initialization of transport formulas
!                         (calculation of constants) else False.
!  5 kinvis            I  kinematic viscosity
!  3 pacfac            I  packing factor (porosity)
!  4 relden            I  relative density
! 15 sedtra            O  calculated sediment transport
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
! $Log: setfaw.pf,v $
! Revision 1.3  1998/02/25  12:48:58  kuipe_j
! Check on grain size added
!
! Revision 1.2  1995/05/30  07:07:32  hoeks_a
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
   real       g      ,pacfac ,relden ,kinvis ,d35    ,chezy ,&
   &velo   ,depth  ,forcn1 ,forcn2 ,forcn3 ,forcn4 ,sedtra
   logical    initra
!
!     Declaration of local parameters
!
   real       alpha  ,beta   ,gamma  ,delta  ,dg     ,term   ,fgdel
!
   if (initra) then
!
!        Calculation of constants in time.
!        First calculate dimensionless grain size Dg.
!
      if (d35 .lt. 1.e-6) then
!          A negative value is used to check on grain size
         forcn1 = -1001.
      else
         dg = d35 * (g * relden / kinvis**2)**(1./3.)
!
!          Then calculate alpha, beta, gamma and delta.
!
         if (dg .le. 60.) then
            alpha = 1.00 - 0.56 * log10(dg)
            beta  = 6.83 / dg + 1.67
            gamma = 10.**(2.79*log10(dg) - 0.98*log10(dg)**2 - 3.46)
            delta = 0.23 / sqrt(dg) + 0.14
         else
            alpha = 0.00
            beta  = 1.78
            gamma = 0.025
            delta = 0.17
         endif
!
!          Finaly calculate constants.
!
         forcn1 = alpha
         forcn2 = beta
         forcn3 = gamma * d35 / ((1.-pacfac) * g**(alpha/2.))
         forcn4 = sqrt((32.*g)**(alpha-1.) / (relden*d35)) / delta
      endif
   else
!
!        Calculation of transport.
!
      alpha = forcn1
      beta  = forcn2
      term  = log10(10.*depth / d35)
      fgdel = forcn4 * (term / chezy)**alpha / term * velo
!
!        Test if threshold will be exceeded.
!
      if (fgdel .gt. 1.) then
         sedtra = forcn3 * velo * chezy**alpha * (fgdel-1.)**beta
      else
         sedtra = 0.
      endif
   endif
!
end
