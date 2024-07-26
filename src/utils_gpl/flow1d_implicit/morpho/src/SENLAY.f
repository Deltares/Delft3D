      subroutine senlay (redfun ,distnc ,laythn ,depth ,velo  ,pi2   ,
     &                   g      ,sedtra ,celeri )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SENLAY (SEdiment Non-alluvial LAYer)
c
c Module description: Correct sediment transport and celerity in a grid
c                     point for a non-alluvial layer. Transport and
c                     celerity are reduced by one of the two possible
c                     reduction functions.
c
c
c Precondition:       The sediment motion "feels" the non-erodible lay-
c                     er.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 celeri            IO celerity
c  4 depth             I  avarage depth
c  2 distnc            I  Distance between bed level and non-erodible
c                         layer (DELTA).
c  7 g                 I  Acceleration of gravity.
c  3 laythn            I  Layer thickness (DELTA-a)
c  6 pi2               I  Pi / 2.
c  1 redfun            I  Type of reduction function:
c                         crdstr (1) : Straight reduction
c                         crdsin (2) : Sinus reduction
c  8 sedtra            IO calculated sediment transport
c  5 velo              I  velocity (without sign)
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: senlay.pf,v $
c Revision 1.5  1999/03/15  15:53:40  kuipe_j
c tabs removed
c
c Revision 1.4  1996/04/12  13:05:47  kuipe_j
c headers, minor changes
c
c Revision 1.3  1995/05/30  09:56:29  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:27  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:27  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:58  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:22  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      real       distnc ,laythn ,depth  ,velo   ,pi2   ,g     ,
     &           sedtra ,celeri
      integer    redfun
c
c     Declaration of local parameters
c
      real       disrat ,term  ,factor  ,arg
c
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      disrat = distnc / laythn
      term   = sedtra / laythn * (1. + distnc / (depth - velo**2/g))
c
      if (disrat .le. 0.) then
c     
c        Transport is zero on or below fixed layer
c
         sedtra = 0.
         celeri = 0.
      else if ( redfun .eq. crdstr ) then
c
c        Linear reduction function.
c
         sedtra = disrat * sedtra
         celeri = disrat * celeri + term
      else
c
c        Sinusoidal reduction function.
c
         arg    = pi2 * disrat
         factor = sin(arg)
         sedtra = factor * sedtra
         celeri = factor * celeri + pi2 * cos(arg) * term
      endif
c
      end
