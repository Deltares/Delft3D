      subroutine gstwwj (initra ,nfrac  ,g     ,pacfac ,relden ,kinvis ,
     &                   d50    ,chezy  ,velo  ,hrad   ,dfrac  ,frcnf1 ,
     &                   frcnf2 ,frcnf3 ,sedtra,wwjcof ,hidexpf)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSTFWWJ (Grad Sed Transp Form Wu, Wang and Jia)
c
c Module description: Calculate the sediment transport according to
c                     Ashida & Michiue for graded material.
c
c                     [ memo Flokstar ,May 2001 ]
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
c frcnf3            IO Constant 2 in transport formula per fraction.
c g                 I  Acceleration of gravity.
c hidexpf           I  Hiding and exposure factor per fraction
c hrad              I  hydraulic radius
c initra            I  True for initialization of transport formulas
c                      (calculation of constants) else False.
c kinvis            I  kinematic viscosity  
c nfrac             I  number of fractions 
c pacfac            I  packing factor (porosity)
c relden            I  relative density
c sedtra            O  calculated sediment transport
c velo              I  velocity (without sign)
c wwjcof (1)        I  Critical theta
c=======================================================================
c
c     Declaration of parameters
c
      integer    nfrac
      real       g      ,pacfac ,relden ,d50   ,kinvis ,chezy  ,velo  ,
     &           hrad   
      real       dfrac  (nfrac) ,frcnf1 (nfrac) ,frcnf2 (nfrac),
     &           frcnf3 (nfrac) ,sedtra (nfrac) ,hidexpf(nfrac),
     &           wwjcof (1)
      logical    initra
c
c     Declaration of local parameters
c
      integer    i
      real       ksi    ,usterkw ,taubci, term ,sbiacc, ssiacc, mu ,
     &           thetac          
c
      if (initra) then
c
c        Calculation of constants in time.
c
         if (wwjcof(1) .eq. 0.) then
            thetac = 0.03
         else
            thetac = wwjcof(1)
         endif 
         do i=1,nfrac
            frcnf1(i) = 1.0 / (g * relden * dfrac(i) * thetac)
            frcnf2(i) = sqrt(g * relden * dfrac(i)**3)
c                       It is assumed that the formula is derived for
c                       a packing factor of 0.4 .
     &                  * (1.0 - 0.4) / (1.-pacfac)
c           Calculation of Omega(i)
            term      = 13.95 * kinvis / dfrac(i)
            frcnf3(i) = sqrt(term**2 + 1.09 * g * relden * dfrac(i))
     &                  - term
          enddo
      else
c
c        Calculation of transport.
c        Start with the computation of fraction independent constants
c
         usterkw  = (velo/chezy)**2 * g
         mu       = ((d50/hrad)**(1./6.) * chezy / 20.)**1.5 

         do i=1,nfrac
c
c           Get hiding effect.
c
            ksi  = hidexpf(i) 
c
            taubci = usterkw * frcnf1(i) / ksi
            term   = mu*taubci
            if (term .le. 1.0) then
c
c              Test if threshold will be exceeded for bottom transport.
c
               sbiacc = 0.
            else            
               sbiacc = 0.0053 * ( term - 1.0)**2.2
            endif
            if (taubci .le. 1.0) then
c
c              Test if threshold will be exceeded for suspended transport.
c
               ssiacc = 0.
            else            
               ssiacc = 0.0000262 * 
     &                  ( velo / frcnf3(i) * (taubci - 1.0))**1.74
            endif 
            sedtra(i) = frcnf2(i) * (sbiacc + ssiacc)
        enddo

      endif
c
      end
