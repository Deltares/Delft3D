function putrel (fd_nefis ,grpnam ,elmnam ,&
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
! Module:             PUTREL (PUT Real ELement to a nefis file)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 buffer            P  -
!  2 datfds            P  -
!  1 deffds            P  -
!  4 elmnam            P  -
!  3 grpnam            P  -
!  0 putrel            O  Return code of Putelt.
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
! $Log: putrel.pf,v $
! Revision 1.3  1995/10/18  08:59:05  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  06:57:13  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:13  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:43  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       putrel
   integer       fd_nefis, uindex(*) ,usrord(*)
   real          buffer(*)
   character(len=*) grpnam    ,elmnam
!
!     Declaration of external functions
!
   integer       putelt
   external      putelt
!
   putrel = putelt (fd_nefis ,grpnam ,elmnam ,&
   &uindex  ,usrord ,buffer )
!
!
end
