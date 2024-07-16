      function getrel (fd_nefis ,grpnam ,elmnam ,
     &                 uindex  ,usrord ,buflen ,buffer )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Fileio module (interface to nefis)
c
c Programmer:         J.Kuipers
c
c Module:             GETREL (GET Real ELement from a nefis file)
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 buffer            P  -
c  7 buflen            P  -
c  2 datfds            P  -
c  1 deffds            P  -
c  4 elmnam            P  -
c  0 getrel            O  Return code of Getelt.
c  3 grpnam            P  -
c  5 uindex            P  -
c  6 usrord            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getelt  GETs ELemenT(s) from a group on data file
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
c $Log: getrel.pf,v $
c Revision 1.3  1995/10/18  08:59:00  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  06:57:08  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:09  hoeks_a
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
      integer       getrel
      integer       fd_nefis, uindex(*) ,usrord(*) ,buflen
      real          buffer(*)
      character*(*) grpnam    ,elmnam
c
c     Declaration of external functions
c
      integer       getelt
      external      getelt
c
      getrel = getelt (fd_nefis ,grpnam ,elmnam ,
     &                 uindex  ,usrord ,buflen ,buffer )
c
      end
