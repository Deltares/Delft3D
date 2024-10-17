function getlel (fd_nefis ,grpnam ,elmnam ,&
&uindex  ,usrord ,buflen ,buffer )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Fileio module (interface to nefis)
!
! Programmer:         J.Kuipers
!
! Module:             GETLEL (GET Logical ELement from nefis file)
!
! Module description: GETLEL is a call to GETELT. All parameters are
!                     passed. GETLEL will be used for reading logicals
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 buffer            P  -
!  7 buflen            P  -
!  2 datfds            P  -
!  1 deffds            P  -
!  4 elmnam            P  -
!  0 getlel            O  Return code of Getelt.
!  3 grpnam            P  -
!  5 uindex            P  -
!  6 usrord            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! getelt  GETs ELemenT(s) from a group on data file
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
! $Log: getlel.pf,v $
! Revision 1.3  1995/10/18  08:58:59  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  06:57:07  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:09  hoeks_a
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
   integer       getlel
   integer       fd_nefis, uindex(*) ,usrord(*) ,buflen
   logical       buffer(*)
   character(len=*) grpnam    ,elmnam
!
!     Declaration of external functions
!
   integer       getelt
   external      getelt
!
   getlel = getelt (fd_nefis ,grpnam ,elmnam ,&
   &uindex  ,usrord ,buflen ,buffer )
!
end
