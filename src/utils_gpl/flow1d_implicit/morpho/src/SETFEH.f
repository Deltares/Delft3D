      subroutine setfeh (initra,g     ,pacfac ,relden ,d50    ,chezy ,
     &                   velo  ,forcn1,sedtra )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SETFEH (SEdiment Transp. Form. Engelund & Hansen)
c
c Module description: Calculate the sediment transport according to
c                     Engelund & Hansen
c                     [ Doc. S-FO-002.2KV / Eq. 2.8 ]
c
c Precondition:       Depth > 0.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 chezy             I  Chezy value
c  5 d50               I  D50
c  8 forcn1            IO Constant 1 in transport formula.
c  2 g                 I  Acceleration of gravity.
c  1 initra            I  True for initialization of transport formulas
c                         (calculation of constants) else False.
c  3 pacfac            I  packing factor (porosity)
c  4 relden            I  relative density
c  9 sedtra            O  calculated sediment transport
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
c $Log: setfeh.pf,v $
c Revision 1.4  1999/03/15  15:53:41  kuipe_j
c tabs removed
c
c Revision 1.3  1998/02/25  12:48:59  kuipe_j
c Check on grain size added
c
c Revision 1.2  1995/05/30  07:07:33  hoeks_a
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
      real       g      ,pacfac ,relden ,d50    ,chezy  ,velo   ,
     &           forcn1 ,sedtra
      logical    initra
c
      if (initra) then
c
c        Calculation of constants in time.
c
         if (d50 .lt. 1.e-6) then
c           A negative value is used to check on grain size
            forcn1 = -1001.
         else     
            forcn1 = .05 / (d50 * (1.-pacfac) * sqrt(g) * relden**2)
         endif
      else
c
c        Calculation of transport.
c
         sedtra = forcn1 * velo**5 / chezy**3
      endif
c
      end
