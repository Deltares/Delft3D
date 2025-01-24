subroutine gsdhgi (initra ,g      ,relden ,kinvis ,dmed  ,chezy  ,&
&velo   ,depth  ,frou2  ,beta1  ,beta2 ,trforb ,&
&dgdm   ,sedexp ,dunehe )

!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsdhgi.F,v $
! Revision 1.3  1996/06/07  11:56:04  kuipe_j
! multi  +  fixed layer
!
! Revision 1.2  1995/09/27  10:12:06  kuipe_j
! Maintenance
!
!
!***********************************************************************
!     Graded Sediment calculation of Dune Height according to GIll
!
!     Declaration of parameters
!
   real       g      ,relden ,kinvis ,dmed   ,chezy  ,&
   &velo   ,depth  ,frou2  ,beta1  ,beta2  ,dgdm   ,&
   &sedexp ,dunehe
   real       trforb(*)
   logical    initra
!
!     Declaration of local parameters
!
   real       sheas  ,sheasc  ,reduc  ,dgr ,m
!
   if (initra) then
!
!        Calculation of constants in time.
!        Calculate dgdm where
!        dimensionless grain size Dg=dgdm/dm
!
      dgdm = (g * relden / kinvis**2)**(1./3.)
   else
!
!        Calculation of exponent in transport formule
!
      goto (10,20,30,200,50,200,70,80,90,100) int(trforb(1))
!           EH
10    sedexp = 2.5
      goto 200
!           MPM
20    sedexp = 1.5
      goto 200
!           Ackers White
30    continue
!           sediment exponent will be calculated
!           At input sedexp containds D35
!
      dgr    = sedexp * dgdm
      m      = 9.66/dgr +1.34
      sedexp = (m + 1.)*.5
      goto 200
!           Van Rijn
! 40        sedexp = 2.4 ??
!        goto 200
!           PK
50    sedexp = 1.5
      goto 200
!           User Defined
! 60        sedexp = input
!        goto 200
!           Waalbocht
70    continue
!           sedexp = input
      goto 200
!           MPM (special)
80    sedexp = 1.5
      goto 200
!           Ashida
90    sedexp = 1.5
      goto 200
!           WWJ
100   sedexp = 2.2
200   continue
!
!        Calculation of dune height
!
      sheas  = max((velo / chezy)**2 / (relden * dmed),1.e-10)
      sheasc = .013 * (dmed * dgdm)**.29
      reduc  = max(1. - beta2 * sheasc / sheas ,0.)
      dunehe = depth / (2. * sedexp * beta1) *&
      &reduc * abs(1. - frou2)
   endif

end
