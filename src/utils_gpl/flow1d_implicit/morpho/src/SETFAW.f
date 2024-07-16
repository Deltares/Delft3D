      subroutine setfaw (initra ,g      ,pacfac ,relden ,kinvis ,d35   ,
     &                   chezy  ,velo   ,depth  ,forcn1 ,forcn2,
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
c Module:             SETFAW (SEdiment Transport Formula Ackers & White)
c
c Module description: Calculate the sediment transport according to
c                     Ackers & White.
c                     [ Doc. S-FO-002.2KV / Eq. 2.13 - 2.19 and 2.7 ]
c
c Precondition:       Depth > 0.
c                     Dimensionless grain size Dg > 1.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 chezy             I  Chezy value
c  6 d35               I  D35
c  7 d50               I  D50
c 10 depth             I  avarage depth
c 11 forcn1            IO Constant 1 in transport formula.
c 12 forcn2            IO Constant 2 in transport formula.
c 13 forcn3            IO Constant 3 in transport formula.
c 14 forcn4            IO Constant 4 in transport formula.
c  2 g                 I  Acceleration of gravity.
c  1 initra            I  True for initialization of transport formulas
c                         (calculation of constants) else False.
c  5 kinvis            I  kinematic viscosity
c  3 pacfac            I  packing factor (porosity)
c  4 relden            I  relative density
c 15 sedtra            O  calculated sediment transport
c  9 velo              I  velocity (without sign)
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: setfaw.pf,v $
c Revision 1.3  1998/02/25  12:48:58  kuipe_j
c Check on grain size added
c
c Revision 1.2  1995/05/30  07:07:32  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:31  hoeks_a
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
      real       g      ,pacfac ,relden ,kinvis ,d35    ,chezy ,
     &           velo   ,depth  ,forcn1 ,forcn2 ,forcn3 ,forcn4 ,sedtra
      logical    initra
c
c     Declaration of local parameters
c
      real       alpha  ,beta   ,gamma  ,delta  ,dg     ,term   ,fgdel
c
      if (initra) then
c
c        Calculation of constants in time.
c        First calculate dimensionless grain size Dg.
c
         if (d35 .lt. 1.e-6) then
c          A negative value is used to check on grain size
           forcn1 = -1001.
         else
           dg = d35 * (g * relden / kinvis**2)**(1./3.)
c
c          Then calculate alpha, beta, gamma and delta.
c
           if (dg .le. 60.) then
             alpha = 1.00 - 0.56 * log10(dg)
             beta  = 6.83 / dg + 1.67
             gamma = 10.**(2.79*log10(dg) - 0.98*log10(dg)**2 - 3.46)
             delta = 0.23 / sqrt(dg) + 0.14
           else
             alpha = 0.00
             beta  = 1.78
             gamma = 0.025
             delta = 0.17
           endif
c
c          Finaly calculate constants.
c
           forcn1 = alpha
           forcn2 = beta
           forcn3 = gamma * d35 / ((1.-pacfac) * g**(alpha/2.))
           forcn4 = sqrt((32.*g)**(alpha-1.) / (relden*d35)) / delta
         endif
      else
c
c        Calculation of transport.
c
         alpha = forcn1
         beta  = forcn2
         term  = log10(10.*depth / d35)
         fgdel = forcn4 * (term / chezy)**alpha / term * velo
c
c        Test if threshold will be exceeded.
c
         if (fgdel .gt. 1.) then
            sedtra = forcn3 * velo * chezy**alpha * (fgdel-1.)**beta
         else
            sedtra = 0.
         endif
      endif
c
      end
