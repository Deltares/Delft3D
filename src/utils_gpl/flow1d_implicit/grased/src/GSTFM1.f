      subroutine gstfm1 (initra ,nfrac  ,g     ,pacfac ,relden , 
     &                   chezy  ,velo   ,dfrac ,frcnf1 ,sedtra ,
     &                   mpmcof ,hidexpf)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSTFMM (Grad Sed Transp Form Meyer-Peter & Muller 1)
c
c Module description: Calculate the sediment transport according to
c                     Meyer-Peter & Muller for graded material.
c                     Hiding effect according to Egiazaroff, Ashida
c                     Michiue. Input of Ripple factor and critical
c                     Shields parameter
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
c  g                 I  Acceleration of gravity.
c  hrad              I  hydraulic radius
c  hidexpf           I  Hiding and exposure factor per fraction
c  initra            I  True for initialization of transport formulas
c                       (calculation of constants) else False.
c  mpmcof(2)         I  (1) = Ripple factor
c                       (2) = Critical Shields parameter 
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
      real       g      ,pacfac ,relden ,chezy ,velo   
      real       dfrac  (nfrac) ,frcnf1 (nfrac) ,
     &           sedtra (nfrac) ,hidexpf(nfrac) ,mpmcof (2)
      logical    initra
c
c     Declaration of local parameters
c
      integer    i      
      real       mu     ,mushil ,arg  ,ksi
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
         mu     = mpmcof(1)
         mushil = velo**2 * mu / (chezy**2 * relden )
c 
         do 20 i=1,nfrac
c
c           Get hiding effect.
c
            ksi = hidexpf(i) 

            arg = mushil / dfrac(i) - ksi * mpmcof(2)
c
c           Test if threshold will be exceeded.
c
            if (arg .gt. 0.) then
               sedtra(i) = frcnf1(i) * arg**1.5
            else
               sedtra(i) = 0.
            endif
   20    continue
c
      endif
c
      end
