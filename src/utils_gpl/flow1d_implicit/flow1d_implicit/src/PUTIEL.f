      function putiel (fd_nefis ,grpnam ,elmnam ,
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
c Module:             PUTIEL (PUT Integer ELement to nefis file)
c
c Module description: PUTIEL is a call to PUTELT. All parameters are
c                     passed. PUTIEL will be used for writing of
c                     integers.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 buffer            P  -
c  2 datfds            P  -
c  1 deffds            P  -
c  4 elmnam            P  -
c  3 grpnam            P  -
c  0 putiel            O  Return code of Putelt.
c  5 uindex            P  -
c  6 usrord            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c putelt
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: putiel.pf,v $
c Revision 1.3  1995/10/18  08:59:04  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  06:57:12  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:12  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:43  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       putiel
      integer       fd_nefis, uindex(*) ,usrord(*) ,buffer(*)
      character*(*) grpnam    ,elmnam
c
c     Declaration of external functions
c
      integer       putelt
      external      putelt
c
c
      putiel = putelt (fd_nefis ,grpnam ,elmnam ,
     &                 uindex  ,usrord ,buffer )
c
      end
