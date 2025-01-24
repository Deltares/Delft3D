function putcel (fd_nefis ,grpnam ,elmnam ,&
&uindex  ,usrord ,buffer )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Fileio module (interface to nefis)
!
! Programmer:         J.Kuipers
!
! Module:             PUTCEL (PUT Character ELement to nefis file)
!
! Module description: PUTCEL is a call to PUTELT. All parameters are
!                     passed. PUTCEL will be used for writing characters
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 buffer            P  -
!  2 datfds            P  -
!  1 deffds            P  -
!  4 elmnam            P  -
!  3 grpnam            P  -
!  0 putcel            O  Return code of Putelt.
!  5 uindex            P  -
!  6 usrord            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! putelt
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: putcel.pf,v $
! Revision 1.3  1995/10/18  08:59:03  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  06:57:11  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:11  hoeks_a
! Initial check-in
!
! Revision 1.1  1993/11/26  15:30:04  kuipe_j
! Update after finishing Sobeksel.
!
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       putcel
   integer       fd_nefis, uindex(*) ,usrord(*)
   character*(*) buffer(*)
   character*(*) grpnam    ,elmnam
!
!     Declaration of external functions
!
   integer       putels
   external      putels

!
   putcel = putels (fd_nefis, grpnam ,elmnam ,&
   &uindex  ,usrord ,buffer )
!
end
