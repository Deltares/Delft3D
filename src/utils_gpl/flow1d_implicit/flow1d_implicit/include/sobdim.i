!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling system
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          SOBEK
!
! Programmer:         S.L. van der Woude
!
! Module:             sobdim.i (include file)
!
! Module description: dimensions for Sobek packed arrays
!
!
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
!
!***********************************************************************
! CVS log information:
!
! $Id: sobdim.i,v 1.6 1998/06/18 13:28:42 kuipe_j Exp $
!
! History:
! $Log: sobdim.i,v $
! Revision 1.6  1998/06/18  13:28:42  kuipe_j
! Dimension Waoft changed
!
! Revision 1.5  1997/01/23  08:29:26  kuipe_j
! Make flow module robust
!
! Revision 1.4  1996/10/31  10:31:51  kuipe_j
! Extra resistance finished
!
! Revision 1.3  1996/09/03  14:54:19  kuipe_j
! frequency time hist,etc
!
! Revision 1.2  1995/11/21  11:08:56  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.1  1995/09/22  10:13:04  kuipe_j
! variable dimensions
!
!
!***********************************************************************
!
!     Codes for array dimensions in FLOW module
!     ------------------------------------------------------
!                          waoft  (ngrid  ,dmwaof)
integer    dmwaof
!                          strhis (dmstrh ,nstru )
integer    dmstrh
!                          work   (nnode  ,dmwork)
integer    dmwork
!                          cnpflg (dmcopr ,nstru )
integer    dmcopr
!                          resbuf (dmbuf1 , 6)
!                          strbuf (dmbuf1 , 2 , nstru)
integer    dmbuf1
!                          solbuf (dmbuf2 , 6 , ngrid)
integer    dmbuf2
!                          sectv  (ngrid  ,dmsecv )
integer    dmsecv
!
integer    dmgrnd
!                          buffer (dmbuffer,ngrid)
integer    dmbuffer
!                          uscoef (nrcoefs,nucoef)
integer    nrcoefs
!                          strpar (dmstrpar,nstru)
integer    dmstrpar
!
parameter (dmwaof =  18,&
&dmstrh =  13,&
&dmwork =   7,&
&dmcopr =   3,&
&dmbuf1 =  20,&
&dmbuf2 =   4,&
&dmsecv =   8,&
&dmbuffer = 33,&
&dmgrnd = 1200&
&)
!
common /dimens/nrcoefs,dmstrpar
!     ------------------------------------------------------------
!     Codes for array dimensions in salt module
!     ------------------------------------------------------------
!     ------------------------------------------------------------
!     Codes for array dimensions in sediment module
!     ------------------------------------------------------------
!     ------------------------------------------------------------
!     Codes for array dimensions in morphology module
!     ------------------------------------------------------------
!     ------------------------------------------------------------
!     Codes for array dimensions in water quality interface module
!     ------------------------------------------------------------
!     ------------------------------------------------------------
!     Codes for array dimensions in cross sectional table module
!     ------------------------------------------------------------
!     ------------------------------------------------------------
!     Codes for array dimensions in main module
!     ------------------------------------------------------------
