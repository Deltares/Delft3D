      subroutine setfud (initra ,g      ,pacfac ,relden ,d50   ,d90    ,
     &                   chezy  ,uscofb ,velo   ,hrad  ,forcn1 ,forcn2 ,
     &                   sedtra )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SETFUD (SEdiment Transport Formula User Defined)
c
c Module description: Calculate the sediment transport according user
c                     specified coefficients.
c                     [S-FO-002.2KV / Eq. 2.33, 2.6, 2.7, 2.10 and 2.11]
c
c Precondition:       Depth > 0.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 chezy             I  Chezy value
c  5 d50               I  D50
c  6 d90               I  D90
c 11 forcn1            IO Constant 1 in transport formula.
c 12 forcn2            IO Constant 2 in transport formula.
c  2 g                 I  Acceleration of gravity.
c 10 hrad              I  hydraulic radius
c  1 initra            I  True for initialization of transport formulas
c                         (calculation of constants) else False.
c  3 pacfac            I  packing factor (porosity)
c  4 relden            I  relative density
c 13 sedtra            O  calculated sediment transport
c  8 uscofb(6)         I  Defines user coefficients for a branch with a
c                         user defined transport formula:
c                         (1) =   ALPHA
c                         (2) =   BETA
c                         (3) =   GAMMA
c                         (4) =   Shields-factor-critical
c                         (5) =   Flag indicates calculation of ripple
c                                 factor MU:
c                                 crfcon (0) : Use ripple constant
c                                 crfcal (1) : Calculate ripple
c                         (6) =   Ripple constant in case uscoef(5) = 0.
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
c $Log: setfud.pf,v $
c Revision 1.4  1998/02/25  12:49:02  kuipe_j
c Check on grain size added
c
c Revision 1.3  1995/05/30  09:56:33  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:37  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:34  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:08  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:24  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      real       g      ,pacfac ,relden ,d50    ,d90    ,chezy  ,
     &           velo   ,hrad   ,forcn1 ,forcn2 ,sedtra
      real       uscofb (6)
      logical    initra
c
c     Declaration of local parameters
c
      real       c90    ,mu1    ,mushil ,arg
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      if (initra) then
c
c        Calculation of constants in time.
c
         if (d50 .lt. 1.e-6 .or. d90 .lt. 1.e-6) then
c         A negative value is used to check on grain size
          forcn1 = -1001.
         else
          forcn1 = uscofb(2) / (1.-pacfac) * sqrt(g*relden*d50) * d50
          if ( int(uscofb(5)) .eq. crfcal ) then
c
c           Ripple factor will be calculated
c
            forcn2 = 1. / (relden * d50)
          else
c
c           Ripple factor is constant
c
            forcn2 = uscofb(6) / (relden * d50)
          endif
         endif
      else
c
c        Calculation of transport.
c
         if ( int(uscofb(5)) .eq. crfcal ) then
c
c           Calculate ripple factor.
c
            c90 = 18. * log10(4.*hrad / d90)
            mu1 = forcn2 * (chezy/c90)**1.5
         else
            mu1 = forcn2
         endif
         mushil = (velo / chezy)**2 * mu1
         arg    = mushil - uscofb(4)
c
c        Test if threshold will be exceeded.
c
         if (arg .gt. 0.) then
            sedtra = forcn1 * mushil**uscofb(3) * arg**uscofb(1)
         else
            sedtra = 0.
         endif
      endif
c
      end
