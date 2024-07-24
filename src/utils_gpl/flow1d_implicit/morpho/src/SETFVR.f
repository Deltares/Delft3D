      subroutine setfvr (initra ,g      ,pacfac ,relden ,kinvis ,d50   ,
     &                   d90    ,velo   ,depth  ,hrad   ,forcn1 ,forcn2,
     &                   forcn3 ,forcn4 ,sedtra )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SETFVR (SEdiment Transport Formula Van Rijn)
c
c Module description: Calculate the sediment transport according to Van
c                     Rijn.
c                     [ Doc. S-FO-002.2KV / Eq. 2.20 - 2.26 ]
c
c Precondition:       Depth > 0.
c                     100*e-6 < D50 < 2000*e-6
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 d50               I  D50
c  7 d90               I  D90
c  9 depth             I  avarage depth
c 11 forcn1            IO Constant 1 in transport formula.
c 12 forcn2            IO Constant 2 in transport formula.
c 13 forcn3            IO Constant 3 in transport formula.
c 14 forcn4            IO Constant 4 in transport formula.
c  2 g                 I  Acceleration of gravity.
c 10 hrad              I  hydraulic radius
c  1 initra            I  True for initialization of transport formulas
c                         (calculation of constants) else False.
c  5 kinvis            I  kinematic viscosity
c  3 pacfac            I  packing factor (porosity)
c  4 relden            I  relative density
c 15 sedtra            O  calculated sediment transport
c  8 velo              I  velocity (without sign)
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: setfvr.pf,v $
c Revision 1.3  1998/02/25  12:49:03  kuipe_j
c Check on grain size added
c
c Revision 1.2  1995/05/30  07:07:38  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:34  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:24  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      real       g      ,pacfac ,relden ,kinvis ,d50    ,d90    ,velo  ,
     &           depth  ,hrad   ,forcn1 ,forcn2 ,forcn3 ,forcn4 ,sedtra
      logical    initra
c
c     Declaration of local parameters
c
      real       uc     ,velouc ,dg     ,part   ,d50dep
c
      if (initra) then
c
c        Calculation of constants in time.
c        First calculate dimensionless grain size Dg.
c
         if (d50 .lt. 1.e-6 .or. d90 .lt. 1.e-6) then
c          A negative value is used to check on grain size
           forcn1 = -1001.
         else
           dg     = d50 * (g * relden / kinvis**2)**(1./3.)
           forcn1 = .005 / (1.-pacfac) / (g * relden * d50)**1.2
           forcn2 = .012 / (1.-pacfac) / (g * relden * d50)**1.2 /
     &              dg**.6
           forcn3 = .19 * d50**.1
           forcn4 = 8.5 * d50**.6
         endif
      else
c
c        Calculation of transport.
c        First determine range of D50 and compute critical
c        flow velocity uc.
c
         if ( d50 .le. 500.e-6) then
            uc = forcn3 * log10(4.*hrad / d90)
         else
            uc = forcn4 * log10(4.*hrad / d90)
         endif
         velouc = velo - uc
c
c        Test if threshold will be exceeded.
c
         if (velouc .gt. 0.) then
c
c           Compute same part for bed and suspended load
c
            part   = velouc**2.4 * velo * depth
            d50dep = d50 / depth
c
c           Compute bed load (bload) and suspended load (sload)
c
            sedtra = part * (forcn1 * d50dep**1.2 + forcn2 * d50dep)
         else
            sedtra = 0.
         endif
      endif
c
      end
