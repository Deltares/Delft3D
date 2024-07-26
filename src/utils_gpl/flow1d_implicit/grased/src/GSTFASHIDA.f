      subroutine gstfashida (initra ,nfrac  ,g      ,pacfac ,relden ,
     &                       dmed   ,chezy  ,velo   ,hrad   ,dfrac  ,
     &                       frcnf1 ,frcnf2 ,sedtra ,ashcof ,hidexpf)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSTFASHIDA (Grad Sed Transp Form ASHIDA & Michiue)
c
c Module description: Calculate the sediment transport according to
c                     Ashida & Michiue for graded material.
c                     [ memo Sloff ,Jan 2001 ]
c
c Precondition:       velo > 0.
c                     Hiding and exposure effect already calculated and
c                     stored in array Hidexpf.
c
c-----------------------------------------------------------------------
c Parameters:
c NAME              IO DESCRIPTION
c chezy             I  Chezy value
c dfrac             I  Grain sizes per fraction
c dmed              I  Dmedium grain size 
c frcnf1            IO Constant 1 in transport formula per fraction.
c frcnf2            IO Constant 2 in transport formula per fraction.
c g                 I  Acceleration of gravity.
c hidexpf           I  Hiding and exposure factor per fraction
c hrad              I  hydraulic radius
c initra            I  True for initialization of transport formulas
c                      (calculation of constants) else False.
c ashcof(1)         I  (1) = shear stress for Dm
c nfrac             I  number of fractions 
c pacfac            I  packing factor (porosity)
c relden            I  relative density
c sedtra            O  calculated sediment transport
c velo              I  velocity (without sign)
c=======================================================================
c
c     Declaration of parameters
c
      integer    nfrac
      real       g      ,pacfac ,relden ,dmed   ,chezy ,velo   ,
     &           hrad   
      real       dfrac  (nfrac) ,frcnf1 (nfrac) ,frcnf2 (nfrac),
     &           sedtra (nfrac) ,hidexpf(nfrac) ,ashcof(1)
      logical    initra
c
c     Declaration of local parameters
c
      integer    i
      real       ksi    ,ksitot ,usterkw ,tauascm, 
     %           tauasm ,tauasi ,tauasei,uase 
c
      if (initra) then
c
c        Calculation of constants in time.
c
         do i=1,nfrac
            frcnf1(i) = 1. / (g * relden * dfrac(i))
            frcnf2(i) = 17. * sqrt(g * relden * dfrac(i)**3)
     &                  / (1.-pacfac)
         enddo
      else
c
c        Calculation of transport.
c        Start with the computation of fraction independent constants
c
         usterkw  = (velo/chezy)**2 * g
         tauasm   = usterkw / (g * relden * dmed)
         tauascm  = ashcof(1)
         do i=1,nfrac
c
c           Get hiding effect.
c
            ksi     = hidexpf(i) 
c
c           Use constant user specified shear stress for Dm
c
            tauasi  = usterkw * frcnf1(i)
            uase    = velo / (6.0 + 2.5 * 
     &                log(hrad/(dmed*(1.0 + 2.0*tauasm))))
            tauasei = uase**2 * frcnf1(i)
            ksitot  = ksi * tauascm / tauasi
            if (ksitot .ge. 1.0) then
c
c              Test if threshold will be exceeded. 
c
               sedtra (i) = 0.
            else
               sedtra(i) = frcnf2(i) * tauasei**1.5 * 
     &                     (1.0 - ksitot) * (1.0 - sqrt(ksitot))
            endif
        enddo

      endif
c
      end
