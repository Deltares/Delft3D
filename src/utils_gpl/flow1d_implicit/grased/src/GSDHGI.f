      subroutine gsdhgi (initra ,g      ,relden ,kinvis ,dmed  ,chezy  ,
     &                   velo   ,depth  ,frou2  ,beta1  ,beta2 ,trforb ,
     &                   dgdm   ,sedexp ,dunehe )

c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsdhgi.F,v $
c Revision 1.3  1996/06/07  11:56:04  kuipe_j
c multi  +  fixed layer
c
c Revision 1.2  1995/09/27  10:12:06  kuipe_j
c Maintenance
c
c
c***********************************************************************
c     Graded Sediment calculation of Dune Height according to GIll
c
c     Declaration of parameters
c
      real       g      ,relden ,kinvis ,dmed   ,chezy  ,
     &           velo   ,depth  ,frou2  ,beta1  ,beta2  ,dgdm   ,
     &           sedexp ,dunehe
      real       trforb(*)
      logical    initra
c
c     Declaration of local parameters
c
      real       sheas  ,sheasc  ,reduc  ,dgr ,m
c
      if (initra) then
c
c        Calculation of constants in time.
c        Calculate dgdm where
c        dimensionless grain size Dg=dgdm/dm
c
         dgdm = (g * relden / kinvis**2)**(1./3.)
      else
c
c        Calculation of exponent in transport formule
c
         goto (10,20,30,200,50,200,70,80,90,100) int(trforb(1))
c           EH
  10        sedexp = 2.5
         goto 200
c           MPM
  20        sedexp = 1.5
         goto 200
c           Ackers White         
  30        continue
c           sediment exponent will be calculated
c           At input sedexp containds D35
c
            dgr    = sedexp * dgdm
            m      = 9.66/dgr +1.34
            sedexp = (m + 1.)*.5 
         goto 200
c           Van Rijn
c 40        sedexp = 2.4 ??
c        goto 200
c           PK
  50        sedexp = 1.5
         goto 200
c           User Defined
c 60        sedexp = input
c        goto 200
c           Waalbocht    
  70        continue
c           sedexp = input
         goto 200
c           MPM (special)        
  80        sedexp = 1.5
         goto 200
c           Ashida 
  90        sedexp = 1.5
         goto 200  
c           WWJ 
 100        sedexp = 2.2
 200     continue
c
c        Calculation of dune height
c
         sheas  = max((velo / chezy)**2 / (relden * dmed),1.e-10)
         sheasc = .013 * (dmed * dgdm)**.29
         reduc  = max(1. - beta2 * sheasc / sheas ,0.) 
         dunehe = depth / (2. * sedexp * beta1) *
     &            reduc * abs(1. - frou2)
      endif

      end
