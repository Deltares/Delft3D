c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling system
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory management routines
c
c Programmer:         A. Hoekstra
c
c Module:             pointrs.i
c
c Module description: Include file for memory management routines
c
c Pre condition:
c
c Post condition:
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id: pointrs.i,v 1.2 1995/05/30 07:03:41 hoeks_a Exp $
c
c History:
c $Log: pointrs.i,v $
c Revision 1.2  1995/05/30  07:03:41  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:53  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:15  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:04  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      INTEGER   MXCPNT, 
     +          MXDPNT,
     +          MXIPNT, 
     +          MXRPNT, 
     +          MXLPNT

      PARAMETER (MXCPNT=20, 
     +           MXDPNT=20,
     +           MXIPNT=200, 
     +           MXRPNT=200, 
     +           MXLPNT=200)

      CHARACTER CPNTRS(MXCPNT)*16
     +         ,DPNTRS(MXDPNT)*16
     +         ,IPNTRS(MXIPNT)*16
     +         ,RPNTRS(MXRPNT)*16
     +         ,LPNTRS(MXLPNT)*16

      INTEGER   CADDRS(MXCPNT+1)
     +         ,DADDRS(MXDPNT+1)
     +         ,IADDRS(MXIPNT+1)
     +         ,RADDRS(MXRPNT+1)
     +         ,LADDRS(MXLPNT+1)
      
      INTEGER   CLENGT(MXCPNT)
     +         ,DLENGT(MXDPNT)
     +         ,ILENGT(MXIPNT)
     +         ,RLENGT(MXRPNT)
     +         ,LLENGT(MXLPNT)
      
      INTEGER   NCPNTR, 
     +          NDPNTR,
     +          NIPNTR, 
     +          NRPNTR, 
     +          NLPNTR

      INTEGER   COLDPT, 
     +          DOLDPT,
     +          IOLDPT, 
     +          ROLDPT, 
     +          LOLDPT

      COMMON /QQCPNT/ CPNTRS, DPNTRS, IPNTRS, RPNTRS, LPNTRS
      COMMON /QQIPNT/ NCPNTR, NDPNTR, NIPNTR, NRPNTR, NLPNTR
     +               ,CADDRS, DADDRS, IADDRS, RADDRS, LADDRS
     +               ,CLENGT, DLENGT, ILENGT, RLENGT, LLENGT
     +               ,COLDPT, DOLDPT, IOLDPT, ROLDPT, LOLDPT
