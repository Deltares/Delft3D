      subroutine FLSTRU(i1     ,i2     ,g      ,iter   ,nstru  ,strtyp ,
     +                  strpar ,ngrid  ,lambda ,relstr ,dhstru ,h      ,
     +                  h1     ,q      ,q1     ,q2     ,af     ,wf     ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,ngridm ,lsalt  ,
     +                  rho    ,strclo ,strhis ,a2     ,b2     ,c2     ,
     +                  d2     ,e2     ,hlev   ,maxlev ,stdbq  ,nstdb  ,
     +                  juer   ,ker    )


c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLSTRU (FLow abcde coefficients for STRUctures)
c
c Module description: Subroutine FLSTRU controls the activation of the
c                     correct subroutine in order to compute the ABCDE
c                     coefficients for a specific structure.
c
c                     The set of ABCDE coefficients can be split up in:
c
c                     -   coefficients A1-E1 for the continuity equa-
c                         tion;
c                     -   coefficients A2-E2 for the stage-discharge
c                         equation.
c
c                     The ABCDE coefficients for the continuity equation
c                     are already computed in subroutine FLNORM. The
c                     coefficients for the stage discharge equation are
c                     computed differently for each structure. In this
c                     routine the corresponding structure routine is
c                     selected to compute the A2-E2 coefficients.
c
c                     In order to account for compound structures it is
c                     needed to accumulate the computed coefficients for
c                     the structure rather than to assign them.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 27 a2(ngridm)        IO A2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 16 af                P  -
c 28 b2(ngridm)        IO B2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 29 c2(ngridm)        IO C2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 30 d2(ngridm)        IO D2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 11 dhstru            P  -
c 31 e2(ngridm)        IO E2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c  3 g                 P  -
c 13 h1                P  -
c 12 h                 P  -
c  1 i1                I  Index of first grid point in actual branch.
c  2 i2                I  Index of last grid point in actual branch.
c  4 iter              P  -
c  9 lambda            P  -
c 23 lsalt             P  -
c 18 maxtab            I  Maximum number of defined tables.
c  8 ngrid             I  Number of grid points in network.
c 22 ngridm            I  Maximum number of gridpoints in a branch.
c  5 nstru             I  Number of structures.
c 20 ntab              P  -
c 19 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 15 q1                P  -
c 14 q                 P  -
c 10 relstr            P  -
c 24 rho               P  -
c 25 strclo            P  -
c 26 strhis            P  -
c  7 strpar            P  -
c  6 strtyp(10,nstru)  I  Structure definitions:
c                         (1,i) = Type of structure:
c                                 csweir (1) : Simple weir
c                                 caweir (2) : Advanced weir
c                                 csgate (3) : - not used, reserved for
c                                                simple gate -
c                                 cpump  (4) : Pump
c                                 cgenst (5) : General Structure
c                         (2,i) = Position of structure:
c                                 cstbra (1) : In branch
c                                 cstlat (2) : Lateral structure
c                         (3,i) = Left gridpoint.
c                         (4,i) = Right gridpoint.
c                         (5,i) = dummy
c                         (6,i) = dummy
c                         (7,i) = dummy
c                         (8,i) = dummy
c                         (9,i) = dummy
c                         (10,i)= dummy
c 21 table             P  -
c 17 wf                P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flaw    FLow structure Advanced Weir
c flgs    FLow General Structure
c flpp    FLow structure PumP
c flsw    FLow structure Simple Weir
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flstru.pf,v $
c Revision 1.13  1999/03/15  15:50:48  kuipe_j
c tabs removed
c
c Revision 1.12  1998/05/25  19:12:26  kuipe_j
c Wendy structures
c
c Revision 1.11  1997/10/03  06:39:36  kuipe_j
c criterium for flow drection changed
c
c Revision 1.10  1997/02/17  10:20:53  kuipe_j
c Lateral Q in m3/s in cont equation now
c
c Revision 1.9  1996/01/17  14:38:51  kuipe_j
c header update
c
c Revision 1.8  1995/11/21  11:08:03  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.7  1995/09/29  10:36:18  kuipe_j
c Improvement of autostart and simple weir
c
c Revision 1.6  1995/09/22  10:02:16  kuipe_j
c variable dimensions, new headers
c
c Revision 1.5  1995/08/30  12:36:55  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:30  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:55:29  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:30  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:09  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:31:37  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:55  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters:
c
      integer i1, i2, iter , ngrid, ngridm, nstru, maxlev
      integer maxtab, ntabm, nstdb, juer   ,ker  
      integer ntab(4,maxtab)
      integer strtyp(10,*)
      logical lsalt, strclo(*)
      real    g    ,lambda ,relstr ,dhstru
      real    table(ntabm) ,stdbq(nstdb)
      real    strpar(dmstrpar,*), strhis(dmstrh,*),
     +        af(ngrid), wf(ngrid), rho(ngrid)
c
      double precision h(ngrid), h1(ngrid), 
     +                 q(ngrid), q1(ngrid),  q2(ngrid)
      double precision a2(ngridm),   b2(ngridm),
     +                 c2(ngridm),   d2(ngridm),
     +                 e2(ngridm),   hlev(ngrid,maxlev)
c
c     Declaration of local variables:
c
      integer il, ir, i, istru, k, ind,  nstdb1, ndim
      real    asde, bsde, csde, dsde, esde
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Do for each structure in branch
c
      do 100 istru = 1, nstru
c
c        - left /right gridpoint -
c
         il = strtyp(3,istru)
         ir = strtyp(4,istru)
c
         if ( il .ge. i1 .and. il .le. i2 ) then
c
c           - structure is in this branch -
c
            if ( strtyp(2,istru) .eq. cstbra ) then
c
c              Normal structure in branch
c
               bsde = 0.
               dsde = 0.

               if      ( strtyp(1,istru) .eq. csweir ) then
c
c                 - simple weir -
c
                  call FLSW (g      ,il     ,ir     ,ngrid  ,istru  ,
     +                       strclo ,strpar ,h      ,h1     ,q      ,
     +                       q2     ,af     ,maxtab ,ntabm  ,ntab   ,
     +                       table  ,rho    ,strhis ,iter   ,relstr ,
     +                       asde   ,csde   ,esde   )
c
               else if ( strtyp(1,istru) .eq. caweir ) then
c
c                 - advanced weir -
c
                  call FLAW (g      ,il     ,ir     ,ngrid  ,istru  ,
     +                       strclo ,strpar ,h      ,h1     ,q      ,
     +                       q2     ,af     ,rho    ,strhis ,asde   ,
     +                       csde   ,esde   )
c
               else if ( strtyp(1,istru) .eq. cdtbst ) then
c  
c                 - data base structure -
c                   Locate and determine dimension of the data
c                   base of this structure
c
                  ind    = nint(strpar(9,istru))
                  ndim   = nint(strpar(3,istru))
                  
                  nstdb1 = 1
                  k      = 1
                  do  i=1,ndim
                     if (i.ge.3) k = 0
                     nstdb1 = nstdb1 * (nint(strpar(i+3,istru)) + k)
                  enddo
 
                  call FLTS(il     ,ir     ,iter   ,ngrid  ,istru    ,
     +                      nstru  ,relstr ,strpar ,stdbq(ind),nstdb1,
     +                      h      ,h1     ,q      ,q1     ,strhis   , 
     +                      asde   ,bsde   ,csde   ,dsde   ,esde     ,
     +                      juer   ,ker    )
c                  
               else if ( strtyp(1,istru) .eq. cpump ) then
c
c                 - pump -
c
                  call FLPP (il     ,ir     ,ngrid  ,istru  ,strclo ,
     +                       strpar ,h      ,h1     ,maxtab ,ntabm  ,
     +                       ntab   ,table  ,strhis ,asde   ,csde   ,
     +                       esde   )
c
               else if ( strtyp(1,istru) .eq. cgenst ) then
c
c                 - general structure -
c
                  call FLGS (g      ,il     ,ir     ,iter   ,ngrid  ,
     +                       istru  ,lambda ,relstr ,dhstru ,
     +                       strclo ,strpar ,h      ,h1     ,q      ,
     +                       q1     ,q2     ,af     ,wf     ,lsalt  ,
     +                       rho    ,strhis ,asde   ,bsde   ,csde   ,
     +                       dsde   ,esde   ,juer   ,ker    )
c
               else if (strtyp(1,istru) .eq. cflume) then
c
c open flume
                 call FLFLUM (g     , il    , ir    , ngrid , istru ,
     &                        nstru , strpar, strclo, h     , h1    ,
     &                        q     , q2    , af    , rho   ,
     &                        strhis, asde  , csde  , esde  , hlev  ,
     &                        maxlev)
c
               else if (strtyp(1,istru) .eq. cslubo) then
c
c sluice with bottom hinged gate
                 call FLSLBO (g     , il    , ir    , ngrid , istru ,
     &                        nstru , strpar, strclo, h     , h1    ,
     &                        q     , q2    , af    , wf    , rho   ,
     &                        strhis, asde  , csde  , esde  )
c
               else if (strtyp(1,istru) .eq. cclvrt) then
c
c culvert
                 call FLCLVT (g     , il    , ir    , ngrid , istru ,
     &                        nstru , strpar, strclo, h     , h1    ,
     &                        q     , q2    , af    , wf    , rho   ,
     &                        strhis, asde  , csde  , esde  )
c
               else if (strtyp(1,istru) .eq. cculpr) then
c
c culvert with pressure flow
                 call FLCLPR (g     , il    , ir    , ngrid , istru ,
     &                   nstru , strpar, strclo, h     , h1    ,
     &                   q     , q2    , af    , wf    , rho   ,
     &                   strhis, asde  , csde  , esde  ) 
c
               else if (strtyp(1,istru) .eq. cslund) then
c
c sluice with underflow gate
                 call FLSLUN (g     , il    , ir    , ngrid , istru ,
     &                        nstru , strpar, strclo, h     , h1    ,
     &                        q     , q2    , af    , rho   ,
     &                        strhis, asde  , csde  , esde  , hlev  ,
     &                        maxlev)
c
               else if (strtyp(1,istru) .eq. csovun) then
c
c sluice with underflow gate
                 call FLSLOU (g     , il    , ir    , ngrid , istru ,
     &                        nstru , strpar, strclo, h     , h1    ,
     &                        q     , q2    , af    , wf    , rho   ,
     &                        strhis, asde  , csde  , esde  ) 
c
               else if (strtyp(1,istru) .eq. cbridg) then
c
c bridge piers  
                 call FLBRDG (g     , il    , ir    , ngrid , istru ,
     &                        nstru , strpar, strclo, h     , h1    ,
     &                        q     , q2    , af    , rho   ,
     &                        strhis, asde  , csde  , esde  ) 
c
               else if (strtyp(1,istru) .eq. cabutm) then
c
c abutments     
                 call FLABUT (g     , il    , ir    , ngrid , istru ,
     &                        nstru , strpar, h     , h1    ,
     &                        q     , q2    , af    , rho   ,
     &                        strhis, asde  , csde  , esde  )
c
               endif
c
c              Compute index for a2....e2
c
               k = il - i1 + 1
c
c              Add matrix coefficients
c              NOTE: b2 and d2 are calculated in FLNORM !
c
               a2(k) = a2(k) + dble(asde)
               b2(k) = b2(k) + dble(bsde)
               c2(k) = c2(k) + dble(csde)
               d2(k) = d2(k) + dble(dsde)
               e2(k) = e2(k) + dble(esde)
c
            endif
         endif
  100 continue
c
      end
