c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling system
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          SOBEK
c
c Programmer:         S.L. van der Woude
c
c Module:             sobdim.i (include file)
c
c Module description: dimensions for Sobek packed arrays
c                     
c
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
c
c***********************************************************************
c CVS log information:
c
c $Id: sobdim.i,v 1.6 1998/06/18 13:28:42 kuipe_j Exp $
c
c History:
c $Log: sobdim.i,v $
c Revision 1.6  1998/06/18  13:28:42  kuipe_j
c Dimension Waoft changed
c
c Revision 1.5  1997/01/23  08:29:26  kuipe_j
c Make flow module robust
c
c Revision 1.4  1996/10/31  10:31:51  kuipe_j
c Extra resistance finished
c
c Revision 1.3  1996/09/03  14:54:19  kuipe_j
c frequency time hist,etc
c
c Revision 1.2  1995/11/21  11:08:56  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.1  1995/09/22  10:13:04  kuipe_j
c variable dimensions
c
c
c***********************************************************************
c
c     Codes for array dimensions in FLOW module
c     ------------------------------------------------------
c                          waoft  (ngrid  ,dmwaof)
      integer    dmwaof
c                          strhis (dmstrh ,nstru )       
      integer    dmstrh
c                          work   (nnode  ,dmwork)
      integer    dmwork
c                          cnpflg (dmcopr ,nstru )
      integer    dmcopr
c                          resbuf (dmbuf1 , 6)
c                          strbuf (dmbuf1 , 2 , nstru)  
      integer    dmbuf1
c                          solbuf (dmbuf2 , 6 , ngrid)
      integer    dmbuf2
c                          sectv  (ngrid  ,dmsecv )
      integer    dmsecv
c
      integer    dmgrnd
c                          buffer (dmbuffer,ngrid)   
      integer    dmbuffer       
c                          uscoef (nrcoefs,nucoef) 
      integer    nrcoefs 
c                          strpar (dmstrpar,nstru)
      integer    dmstrpar
c                              
      parameter (dmwaof =  18,
     +           dmstrh =  13,
     +           dmwork =   7,
     +           dmcopr =   3,
     +           dmbuf1 =  20,
     +           dmbuf2 =   4,
     +           dmsecv =   8,
     +           dmbuffer = 33,
     +           dmgrnd = 1200
     +          )
c     
      common /dimens/nrcoefs,dmstrpar
c     ------------------------------------------------------------
c     Codes for array dimensions in salt module
c     ------------------------------------------------------------
c     ------------------------------------------------------------
c     Codes for array dimensions in sediment module
c     ------------------------------------------------------------
c     ------------------------------------------------------------
c     Codes for array dimensions in morphology module
c     ------------------------------------------------------------
c     ------------------------------------------------------------
c     Codes for array dimensions in water quality interface module
c     ------------------------------------------------------------
c     ------------------------------------------------------------
c     Codes for array dimensions in cross sectional table module
c     ------------------------------------------------------------
c     ------------------------------------------------------------
c     Codes for array dimensions in main module
c     ------------------------------------------------------------
