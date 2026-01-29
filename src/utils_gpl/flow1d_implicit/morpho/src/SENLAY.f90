subroutine senlay (redfun ,distnc ,laythn ,depth ,velo  ,pi2   ,&
&g      ,sedtra ,celeri )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SENLAY (SEdiment Non-alluvial LAYer)
!
! Module description: Correct sediment transport and celerity in a grid
!                     point for a non-alluvial layer. Transport and
!                     celerity are reduced by one of the two possible
!                     reduction functions.
!
!
! Precondition:       The sediment motion "feels" the non-erodible lay-
!                     er.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 celeri            IO celerity
!  4 depth             I  avarage depth
!  2 distnc            I  Distance between bed level and non-erodible
!                         layer (DELTA).
!  7 g                 I  Acceleration of gravity.
!  3 laythn            I  Layer thickness (DELTA-a)
!  6 pi2               I  Pi / 2.
!  1 redfun            I  Type of reduction function:
!                         crdstr (1) : Straight reduction
!                         crdsin (2) : Sinus reduction
!  8 sedtra            IO calculated sediment transport
!  5 velo              I  velocity (without sign)
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: senlay.pf,v $
! Revision 1.5  1999/03/15  15:53:40  kuipe_j
! tabs removed
!
! Revision 1.4  1996/04/12  13:05:47  kuipe_j
! headers, minor changes
!
! Revision 1.3  1995/05/30  09:56:29  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:07:27  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:27  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:58  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:22  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   real       distnc ,laythn ,depth  ,velo   ,pi2   ,g     ,&
   &sedtra ,celeri
   integer    redfun
!
!     Declaration of local parameters
!
   real       disrat ,term  ,factor  ,arg
!
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   disrat = distnc / laythn
   term   = sedtra / laythn * (1. + distnc / (depth - velo**2/g))
!
   if (disrat .le. 0.) then
!
!        Transport is zero on or below fixed layer
!
      sedtra = 0.
      celeri = 0.
   else if ( redfun .eq. crdstr ) then
!
!        Linear reduction function.
!
      sedtra = disrat * sedtra
      celeri = disrat * celeri + term
   else
!
!        Sinusoidal reduction function.
!
      arg    = pi2 * disrat
      factor = sin(arg)
      sedtra = factor * sedtra
      celeri = factor * celeri + pi2 * cos(arg) * term
   endif
!
end
