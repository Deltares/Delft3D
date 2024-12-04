!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling system
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory management routines
!
! Programmer:         A. Hoekstra
!
! Module:             pointrs.i
!
! Module description: Include file for memory management routines
!
! Pre condition:
!
! Post condition:
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id: pointrs.i,v 1.2 1995/05/30 07:03:41 hoeks_a Exp $
!
! History:
! $Log: pointrs.i,v $
! Revision 1.2  1995/05/30  07:03:41  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:53  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:15  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:04  kuipe_j
! Initial version
!
!
!***********************************************************************
!
INTEGER   MXCPNT,&
&MXDPNT,&
&MXIPNT,&
&MXRPNT,&
&MXLPNT

PARAMETER (MXCPNT=20,&
&MXDPNT=20,&
&MXIPNT=200,&
&MXRPNT=200,&
&MXLPNT=200)

CHARACTER CPNTRS(MXCPNT)*16&
&,DPNTRS(MXDPNT)*16&
&,IPNTRS(MXIPNT)*16&
&,RPNTRS(MXRPNT)*16&
&,LPNTRS(MXLPNT)*16

INTEGER   CADDRS(MXCPNT+1)&
&,DADDRS(MXDPNT+1)&
&,IADDRS(MXIPNT+1)&
&,RADDRS(MXRPNT+1)&
&,LADDRS(MXLPNT+1)

INTEGER   CLENGT(MXCPNT)&
&,DLENGT(MXDPNT)&
&,ILENGT(MXIPNT)&
&,RLENGT(MXRPNT)&
&,LLENGT(MXLPNT)

INTEGER   NCPNTR,&
&NDPNTR,&
&NIPNTR,&
&NRPNTR,&
&NLPNTR

INTEGER   COLDPT,&
&DOLDPT,&
&IOLDPT,&
&ROLDPT,&
&LOLDPT

COMMON /QQCPNT/ CPNTRS, DPNTRS, IPNTRS, RPNTRS, LPNTRS
COMMON /QQIPNT/ NCPNTR, NDPNTR, NIPNTR, NRPNTR, NLPNTR&
&,CADDRS, DADDRS, IADDRS, RADDRS, LADDRS&
&,CLENGT, DLENGT, ILENGT, RLENGT, LLENGT&
&,COLDPT, DOLDPT, IOLDPT, ROLDPT, LOLDPT
