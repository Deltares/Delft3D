      subroutine setfmm (initra ,g      ,pacfac ,relden ,dmed  ,d90    ,
     &                   chezy  ,velo   ,hrad   ,forcn1 ,sedtra)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SETFMM (SEdiment Transp. Form. Meyer-Peter & Muller)
c
c Module description: Calculate the sediment transport according to
c                     Meyer-Peter & Muller.
c                     [ Doc. S-FO-002.2KV / Eq. 2.10 - 2.12 ]
c
c Precondition:       Depth > 0.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 chezy             I  Chezy value
c  6 d90               I  D90
c  5 dmed              I  Dmedium
c 10 forcn1            IO Constant 1 in transport formula.
c  2 g                 I  Acceleration of gravity.
c  9 hrad              I  hydraulic radius
c  1 initra            I  True for initialization of transport formulas
c                         (calculation of constants) else False.
c  3 pacfac            I  packing factor (porosity)
c  4 relden            I  relative density
c 11 sedtra            O  calculated sediment transport
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
c $Log: setfmm.pf,v $
c Revision 1.3  1998/02/25  12:49:00  kuipe_j
c Check on grain size added
c
c Revision 1.2  1995/05/30  07:07:34  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:32  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:23  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      real       g      ,pacfac ,relden ,dmed   ,d90    ,chezy ,velo   ,
     &           hrad   ,forcn1 ,sedtra
      logical    initra
c
c     Declaration of local parameters
c
      real       c90    ,mushil ,arg
c
      if (initra) then
c
c        Calculation of constants in time.
c
         if (dmed .lt. 1.e-6 .or. d90 .lt. 1.e-6) then
c           A negative value is used to check on grain size
            forcn1 = -1001.
         else
            forcn1 = (8. * sqrt(g * relden * dmed**3)) / (1.-pacfac)
         endif
      else
c
c        Calculation of transport.
c
         c90    = 18. * log10(4.*hrad / d90)
         mushil = velo**2 / (c90 * sqrt(c90 * chezy) * relden * dmed)
         arg    = mushil - 0.047
c
c        Test if threshold will be exceeded.
c
         if (arg .gt. 0.) then
            sedtra = forcn1 * arg**1.5
         else
            sedtra = 0.
         endif
      endif
c
      end
