      subroutine gstfaw (initra ,nfrac  ,g      ,pacfac ,relden , 
     &                   kinvis ,chezy  ,velo   ,depth  ,dfrac  , 
     &                   frcnf1 ,frcnf2 ,frcnf3 ,frcnf4 ,sedtra ,
     &                   hidexpf)  
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSTFAW (Graded Sediment Transport Formula Ackers & White)
c
c Module description: Calculate the sediment transport according to
c                     Ackers & White. 
c
c Precondition:       Depth > 0.
c                     Hiding and exposure effect already calculated and
c                     stored in array Hidexpf.
c
c-----------------------------------------------------------------------
c Parameters:
c NAME              IO DESCRIPTION
c chezy             I  Chezy value
c dfrac             I  Grain sizes per fraction
c depth             I  avarage depth
c frcnf1            IO Constant 1 in transport formula (Dg).
c frcnf2            IO Constant 2 in transport formula (n).
c frcnf3            IO Constant 3 in transport formula.
c frcnf4            IO Constant 4 in transport formula.
c hidexpf           I  Hiding and exposure factor per fraction
c                   I  Acceleration of gravity.
c initra            I  True for initialization of transport formulas
c                      (calculation of constants) else False.
c kinvis            I  kinematic viscosity
c nfrac             I  number of fractions 
c pacfac            I  packing factor (porosity)
c relden            I  relative density
c sedtra            O  calculated sediment transport
c velo              I  velocity (without sign)
c=======================================================================
c
c
c     Declaration of parameters
c
      integer    nfrac
      real       g      ,pacfac ,relden ,kinvis ,chezy  ,
     &           velo   ,depth  
      real       dfrac( nfrac)  ,frcnf1(nfrac)  ,frcnf2(nfrac) ,
     &           frcnf3(nfrac)  ,frcnf4(nfrac)  ,sedtra(nfrac) ,
     &           hidexpf(nfrac)
      logical    initra
c
c     Declaration of local parameters
c
      integer    i 
      real       dg     ,fgr    ,fgrea  ,n      ,k     ,m      ,
     &           a      ,eps    ,cpown  
c
      if (initra) then
c
c        Calculation of constants in time per fraction.
c
         do i=1,nfrac
            dg = dfrac(i) * (g * relden / kinvis**2)**(1.0/3.0)
c           If (dg.LT.60.0) Then
            If (dg.LT.61.05402) Then
c           If (dg.LT.1.e10) Then
               n  = 1.0 - 0.56*log10(dg)
               k  = exp(2.86*log(dg) - 0.434*log(dg)**2 - 8.13)
            Else
c              dg = 60.0
c              n  =  0.0
c              k  =  0.0248325
               dg = 61.05402
               n  =  0.0
               k  =  0.0245310
            Endif
            frcnf1(i) = dg
            frcnf2(i) = n
            frcnf3(i) = dfrac(i) * k  / (1.0-pacfac) * g**(-0.5*n) 
            frcnf4(i) = (g * 32.)**((n-1.0)*0.5) /
     &                  sqrt( relden * dfrac(i))
         enddo
      else
c
c        Calculation of transport per fraction. 
c
         do i=1,nfrac
c
c           First calculate dimensionless grain size Dg.
c
            dg = frcnf1(i) 
            n  = frcnf2(i)
            a  = 0.23/sqrt(dg) + 0.14
            m  = 9.66/dg +1.34
c
c           Get hiding and exposure factor Eps    
c
            eps = 1./sqrt(hidexpf(i))
c            
            cpown = chezy**n
            fgr   = frcnf4(i) * velo * 
     &              log10(10.0 * depth / dfrac(i))**(n-1.0) / cpown 
            fgrea = eps * fgr / a
            if (fgrea .gt. 1.) then
               sedtra(i) = frcnf3(i) * (fgrea -1.0)**m * velo * cpown
            else   
               sedtra(i) = 0.
            endif
         enddo
      endif
c
      end
