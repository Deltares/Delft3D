      subroutine setfpk (initra ,g      ,pacfac ,relden ,d50    ,chezy ,
     &                   velo   ,forcn1 ,forcn2 ,sedtra )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SETFPK (SEdiment Transp. Form. Parker & Klingeman)
c
c Module description: Calculate the sediment transport according to
c                     Parker & Klingeman.
c                     [ Doc. S-FO-002.2KV / Eq. 2.27 - 2.32 and 2.7 ]
c
c Precondition:       Depth > 0.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 chezy             I  Chezy value
c  5 d50               I  D50
c  8 forcn1            IO Constant 1 in transport formula.
c  9 forcn2            IO Constant 2 in transport formula.
c  2 g                 I  Acceleration of gravity.
c  1 initra            I  True for initialization of transport formulas
c                         (calculation of constants) else False.
c  3 pacfac            I  packing factor (porosity)
c  4 relden            I  relative density
c 10 sedtra            O  calculated sediment transport
c  7 velo              I  velocity (without sign)
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: setfpk.pf,v $
c Revision 1.3  1998/02/25  12:49:01  kuipe_j
c Check on grain size added
c
c Revision 1.2  1995/05/30  07:07:36  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:33  hoeks_a
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
      real       g      ,pacfac ,relden ,d50    ,chezy  ,
     &           velo   ,forcn1 ,forcn2 ,sedtra
      logical    initra
c
c     Declaration of local parameters
c
      real       omega  ,omega1 ,dlbedl
c
c     Constants
c
      real       shielr
      parameter (shielr=0.0876)
c
      if (initra) then
c
c        Calculation of constants in time.
c
         if (d50 .lt. 1.e-6 ) then
c           A negative value is used to check on grain size
            forcn1 = -1001.
         else
            forcn1 = sqrt(g) / ((1.-pacfac) * relden)
            forcn2 = 1. / (relden * d50 * shielr)
         endif
      else
c
c        Calculation of transport.
c        First determine ratio (omega) of shields parameter Theta
c        and Theta-r.
c
         omega = forcn2 * (velo / chezy)**2
c
c        Compute dimensionless bed load (dlbedl).
c        The computation is different for 3 regions of the ratio omega.
c        The transport is calculated thereafter.
c
         if ( omega .le. 0.95 ) then
            sedtra = 0.0
         else
            if ( omega .le. 1.65 ) then
               omega1 = omega - 1.
               dlbedl = 0.0025 * exp(14.2*omega1 - 9.28*omega1**2)
            else
               dlbedl = 11.2 * (1. - 0.822/omega)**4.5
            endif
            sedtra = forcn1 * dlbedl * (velo / chezy)**3
         endif
      endif
c
      return
      end
