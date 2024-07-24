      function getcel (fd_nefis ,grpnam ,elmnam ,
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
c Module:             GETCEL (GET Character ELement from nefis file)
c
c Module description: GETCEL is a call to GETELT. All parameters are
c                     passed. GETCEL will be used for reading characters
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 buffer            P  -
c  7 buflen            P  -
c  2 datfds            P  -
c  1 deffds            P  -
c  4 elmnam            P  -
c  0 getcel            O  Return code of Getelt.
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
c $Log: getcel.pf,v $
c Revision 1.4  1995/10/18  08:58:57  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:54:36  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:57:05  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:07  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:42  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       getcel
      integer       fd_nefis, uindex(*) ,usrord(*) ,buflen
      character*(*) buffer(*)
      character*(*) grpnam    ,elmnam
c
c     Declaration of external functions
c
      integer       getels
      external      getels
c
      getcel = getels (fd_nefis, grpnam ,elmnam ,
     &                 uindex  ,usrord ,buflen ,buffer )
c
      end
