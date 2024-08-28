      function putlel (fd_nefis ,grpnam ,elmnam ,
     &                 uindex  ,usrord ,buffer )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Fileio module (interface to nefis)
c
c Programmer:         J.Kuipers
c
c Module:             PUTLEL (PUT Logical ELement to a nefis file)
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 buffer            P  -
c  2 datfds            P  -
c  1 deffds            P  -
c  4 elmnam            P  -
c  3 grpnam            P  -
c  0 putrel            O  Return code of Putelt.
c  5 uindex            P  -
c  6 usrord            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c putelt
c=======================================================================
c
c     Declaration of parameters
c
      integer       putlel
      integer       fd_nefis, uindex(*) ,usrord(*)
      logical       buffer(*)
      character(len=*) grpnam    ,elmnam
c
c     Declaration of external functions
c
      integer       putelt
      external      putelt
c
      putlel = putelt (fd_nefis, grpnam ,elmnam ,
     &                 uindex  ,usrord ,buffer )
c
c
      end
