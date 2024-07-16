      subroutine gstfmm (initra ,nfrac  ,g     ,pacfac ,relden ,
     &                   d90    ,chezy  ,velo  ,hrad   ,dfrac  ,
     &                   frcnf1 ,sedtra ,mpmcof,hidexpf)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSTFMM (Grad Sed Transp Form Meyer-Peter & Muller)
c
c Module description: Calculate the sediment transport according to
c                     Meyer-Peter & Muller for graded material.
c
c Precondition:       Depth > 0.
c                     Hiding and exposure effect already calculated and
c                     stored in array Hidexpf.
c
c-----------------------------------------------------------------------
c Parameters:
c  NAME              IO DESCRIPTION
c  chezy             I  Chezy value
c  dfrac             I  Grain sizes per fraction
c  d90               I  D90
c  frcnf1            IO Constant 1 in transport formula per fraction.
c  g                 I  Acceleration of gravity.
c  hidexpf           I  Hiding and exposure factor per fraction
c  hrad              I  hydraulic radius
c  initra            I  True for initialization of transport formulas
c                       (calculation of constants) else False.
c  mpmcof(1)         I  (1) = alpha90
c  nfrac             I  number of fractions 
c  pacfac            I  packing factor (porosity)
c  relden            I  relative density
c  sedtra            O  calculated sediment transport
c  velo              I  velocity (without sign)
c=======================================================================
c
c     Declaration of parameters
c
      integer    nfrac
      real       g      ,pacfac ,relden ,d90    ,chezy ,velo   ,
     &           hrad   
      real       dfrac  (nfrac) ,frcnf1 (nfrac),
     &           sedtra (nfrac) ,hidexpf(nfrac), mpmcof(1)
      logical    initra
c 
c     Declaration of local parameters
c
      integer    i
      real       c90    ,mushil ,arg  ,ksi, alf90
c
      if (initra) then
c
c        Calculation of constants in time.
c
         do 10 i=1,nfrac
            frcnf1(i) = 8. * sqrt(g * relden * dfrac(i)**3)
     &                 / (1.-pacfac)
   10    continue
      else
c
c        Calculation of transport.
c        Start with the computation of fraction independent constants
c
         alf90  = mpmcof(1)
         if (alf90 .gt. 0.) then
             arg = 12.0*hrad / (d90*alf90)
         else    
             arg = 4.0*hrad / d90
         endif    
         c90    = 18. * log10(arg)
         mushil = velo**2 / (c90 * sqrt(c90 * chezy) * relden )
         do 20 i=1,nfrac
c
c           Get hiding effect.
c
            ksi = hidexpf(i) 
c            
            arg = mushil / dfrac(i) - ksi * 0.047
c
c           Test if threshold will be exceeded.
c
            if (arg .gt. 0.) then
               sedtra(i) = frcnf1(i) * arg**1.5
            else
               sedtra(i) = 0.
            endif
   20    continue

      endif
c
      end
