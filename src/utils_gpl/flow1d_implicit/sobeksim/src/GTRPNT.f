      FUNCTION GTRPNT (PNTNAM)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             GTRPNT (GeT Real variable PoiNTer)
c
c Module description: Report address of variable in RPool
c
c                     This function reports the address of a real vari-
c                     able allocated in the RPool
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  0 gtrpnt            O  Address of real variable
c  1 pntnam            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c zzgtpt  ZZ GeT PoinTer
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gtrpnt.pf,v $
c Revision 1.2  1995/05/30  07:03:33  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:45  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:02  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      INTEGER  GTRPNT

      CHARACTER PNTNAM*(*)

      include '..\include\pointrs.i'

      INTEGER   ZZGTPT
      EXTERNAL  ZZGTPT

      GTRPNT = ZZGTPT (PNTNAM, RPNTRS, RADDRS, NRPNTR, ROLDPT)

      END
