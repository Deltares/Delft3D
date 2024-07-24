      function putcel (fd_nefis ,grpnam ,elmnam ,
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
c Module:             PUTCEL (PUT Character ELement to nefis file)
c
c Module description: PUTCEL is a call to PUTELT. All parameters are
c                     passed. PUTCEL will be used for writing characters
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 buffer            P  -
c  2 datfds            P  -
c  1 deffds            P  -
c  4 elmnam            P  -
c  3 grpnam            P  -
c  0 putcel            O  Return code of Putelt.
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
c $Log: putcel.pf,v $
c Revision 1.3  1995/10/18  08:59:03  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  06:57:11  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:11  hoeks_a
c Initial check-in
c
c Revision 1.1  1993/11/26  15:30:04  kuipe_j
c Update after finishing Sobeksel.
c
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       putcel
      integer       fd_nefis, uindex(*) ,usrord(*)
      character*(*) buffer(*)
      character*(*) grpnam    ,elmnam
c
c     Declaration of external functions
c
      integer       putels
      external      putels

c
      putcel = putels (fd_nefis, grpnam ,elmnam ,
     &                 uindex  ,usrord ,buffer )
c
      end
