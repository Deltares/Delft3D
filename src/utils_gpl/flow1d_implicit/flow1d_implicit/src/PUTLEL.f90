function putlel (fd_nefis ,grpnam ,elmnam ,&
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
! Module:             PUTLEL (PUT Logical ELement to a nefis file)
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
!     Declaration of parameters
!
   integer       putlel
   integer       fd_nefis, uindex(*) ,usrord(*)
   logical       buffer(*)
   character(len=*) grpnam    ,elmnam
!
!     Declaration of external functions
!
   integer       putelt
   external      putelt
!
   putlel = putelt (fd_nefis, grpnam ,elmnam ,&
   &uindex  ,usrord ,buffer )
!
!
end
