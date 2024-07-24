      subroutine gstfeh (initra ,nfrac ,g      ,pacfac ,relden ,chezy ,
     &                   velo   ,dfrac ,frcnf1 ,sedtra )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSTFEH (Graded Sediment Transp. Form. Engelund & Hansen)
c
c Module description: Calculate the sediment transport according to
c                     Engelund & Hansen
c
c Precondition:       Depth > 0.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 chezy             I  Chezy value
c  8 dfrac             I  Grain sizes per fraction
c  9 frcnf1            IO Constant 1 in transport formula.
c  3 g                 I  Acceleration of gravity.
c  1 initra            I  True for initialization of transport formulas
c                         (calculation of constants) else False.
c  2 nfrac             I  number of fractions 
c  4 pacfac            I  packing factor (porosity)
c  5 relden            I  relative density
c 10 sedtra            O  calculated sediment transport
c  7 velo              I  velocity (without sign)
c=======================================================================
c
c     Declaration of parameters
c
      integer    nfrac
      real       g      ,pacfac ,relden ,chezy  ,velo
      real       dfrac( nfrac)  ,frcnf1(nfrac)  ,sedtra(nfrac)
      logical    initra
c
c
c     Declaration of local parameters
c
      integer    i 
      real       uc
      
      if (initra) then
c
c        Calculation of constants in time per fraction.
c
         do i=1,nfrac
            frcnf1(i) = 0.05 / 
     &                  (dfrac(i) * (1.-pacfac) * sqrt(g) * relden**2)
         enddo
      else
c
c        Calculation of transport.
c
         uc = velo**5 / chezy**3
         do i=1,nfrac
            sedtra(i) = frcnf1(i) * uc
         enddo
      endif
c
      end
