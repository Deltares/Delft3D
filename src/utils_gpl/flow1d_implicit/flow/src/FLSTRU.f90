subroutine FLSTRU(i1     ,i2     ,g      ,iter   ,nstru  ,strtyp ,&
&strpar ,ngrid  ,lambda ,relstr ,dhstru ,h      ,&
&h1     ,q      ,q1     ,q2     ,af     ,wf     ,&
&maxtab ,ntabm  ,ntab   ,table  ,ngridm ,lsalt  ,&
&rho    ,strclo ,strhis ,a2     ,b2     ,c2     ,&
&d2     ,e2     ,hlev   ,maxlev ,stdbq  ,nstdb  ,&
&juer   ,ker    )


!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLSTRU (FLow abcde coefficients for STRUctures)
!
! Module description: Subroutine FLSTRU controls the activation of the
!                     correct subroutine in order to compute the ABCDE
!                     coefficients for a specific structure.
!
!                     The set of ABCDE coefficients can be split up in:
!
!                     -   coefficients A1-E1 for the continuity equa-
!                         tion;
!                     -   coefficients A2-E2 for the stage-discharge
!                         equation.
!
!                     The ABCDE coefficients for the continuity equation
!                     are already computed in subroutine FLNORM. The
!                     coefficients for the stage discharge equation are
!                     computed differently for each structure. In this
!                     routine the corresponding structure routine is
!                     selected to compute the A2-E2 coefficients.
!
!                     In order to account for compound structures it is
!                     needed to accumulate the computed coefficients for
!                     the structure rather than to assign them.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 27 a2(ngridm)        IO A2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 16 af                P  -
! 28 b2(ngridm)        IO B2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 29 c2(ngridm)        IO C2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 30 d2(ngridm)        IO D2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 11 dhstru            P  -
! 31 e2(ngridm)        IO E2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
!  3 g                 P  -
! 13 h1                P  -
! 12 h                 P  -
!  1 i1                I  Index of first grid point in actual branch.
!  2 i2                I  Index of last grid point in actual branch.
!  4 iter              P  -
!  9 lambda            P  -
! 23 lsalt             P  -
! 18 maxtab            I  Maximum number of defined tables.
!  8 ngrid             I  Number of grid points in network.
! 22 ngridm            I  Maximum number of gridpoints in a branch.
!  5 nstru             I  Number of structures.
! 20 ntab              P  -
! 19 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 15 q1                P  -
! 14 q                 P  -
! 10 relstr            P  -
! 24 rho               P  -
! 25 strclo            P  -
! 26 strhis            P  -
!  7 strpar            P  -
!  6 strtyp(10,nstru)  I  Structure definitions:
!                         (1,i) = Type of structure:
!                                 csweir (1) : Simple weir
!                                 caweir (2) : Advanced weir
!                                 csgate (3) : - not used, reserved for
!                                                simple gate -
!                                 cpump  (4) : Pump
!                                 cgenst (5) : General Structure
!                         (2,i) = Position of structure:
!                                 cstbra (1) : In branch
!                                 cstlat (2) : Lateral structure
!                         (3,i) = Left gridpoint.
!                         (4,i) = Right gridpoint.
!                         (5,i) = dummy
!                         (6,i) = dummy
!                         (7,i) = dummy
!                         (8,i) = dummy
!                         (9,i) = dummy
!                         (10,i)= dummy
! 21 table             P  -
! 17 wf                P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flaw    FLow structure Advanced Weir
! flgs    FLow General Structure
! flpp    FLow structure PumP
! flsw    FLow structure Simple Weir
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flstru.pf,v $
! Revision 1.13  1999/03/15  15:50:48  kuipe_j
! tabs removed
!
! Revision 1.12  1998/05/25  19:12:26  kuipe_j
! Wendy structures
!
! Revision 1.11  1997/10/03  06:39:36  kuipe_j
! criterium for flow drection changed
!
! Revision 1.10  1997/02/17  10:20:53  kuipe_j
! Lateral Q in m3/s in cont equation now
!
! Revision 1.9  1996/01/17  14:38:51  kuipe_j
! header update
!
! Revision 1.8  1995/11/21  11:08:03  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.7  1995/09/29  10:36:18  kuipe_j
! Improvement of autostart and simple weir
!
! Revision 1.6  1995/09/22  10:02:16  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/08/30  12:36:55  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:30  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:55:29  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:30  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:09  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:31:37  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:55  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   integer i1, i2, iter , ngrid, ngridm, nstru, maxlev
   integer maxtab, ntabm, nstdb, juer   ,ker
   integer ntab(4,maxtab)
   integer strtyp(10,*)
   logical lsalt, strclo(*)
   real    g    ,lambda ,relstr ,dhstru
   real    table(ntabm) ,stdbq(nstdb)
   real    strpar(dmstrpar,*), strhis(dmstrh,*),&
   &af(ngrid), wf(ngrid), rho(ngrid)
!
   double precision h(ngrid), h1(ngrid),&
   &q(ngrid), q1(ngrid),  q2(ngrid)
   double precision a2(ngridm),   b2(ngridm),&
   &c2(ngridm),   d2(ngridm),&
   &e2(ngridm),   hlev(ngrid,maxlev)
!
!     Declaration of local variables:
!
   integer il, ir, i, istru, k, ind,  nstdb1, ndim
   real    asde, bsde, csde, dsde, esde
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Do for each structure in branch
!
   do 100 istru = 1, nstru
!
!        - left /right gridpoint -
!
      il = strtyp(3,istru)
      ir = strtyp(4,istru)
!
      if ( il .ge. i1 .and. il .le. i2 ) then
!
!           - structure is in this branch -
!
         if ( strtyp(2,istru) .eq. cstbra ) then
!
!              Normal structure in branch
!
            bsde = 0.
            dsde = 0.

            if      ( strtyp(1,istru) .eq. csweir ) then
!
!                 - simple weir -
!
               call FLSW (g      ,il     ,ir     ,ngrid  ,istru  ,&
               &strclo ,strpar ,h      ,h1     ,q      ,&
               &q2     ,af     ,maxtab ,ntabm  ,ntab   ,&
               &table  ,rho    ,strhis ,iter   ,relstr ,&
               &asde   ,csde   ,esde   )
!
            else if ( strtyp(1,istru) .eq. caweir ) then
!
!                 - advanced weir -
!
               call FLAW (g      ,il     ,ir     ,ngrid  ,istru  ,&
               &strclo ,strpar ,h      ,h1     ,q      ,&
               &q2     ,af     ,rho    ,strhis ,asde   ,&
               &csde   ,esde   )
!
            else if ( strtyp(1,istru) .eq. cdtbst ) then
!
!                 - data base structure -
!                   Locate and determine dimension of the data
!                   base of this structure
!
               ind    = nint(strpar(9,istru))
               ndim   = nint(strpar(3,istru))

               nstdb1 = 1
               k      = 1
               do  i=1,ndim
                  if (i.ge.3) k = 0
                  nstdb1 = nstdb1 * (nint(strpar(i+3,istru)) + k)
               enddo

               call FLTS(il     ,ir     ,iter   ,ngrid  ,istru    ,&
               &nstru  ,relstr ,strpar ,stdbq(ind),nstdb1,&
               &h      ,h1     ,q      ,q1     ,strhis   ,&
               &asde   ,bsde   ,csde   ,dsde   ,esde     ,&
               &juer   ,ker    )
!
            else if ( strtyp(1,istru) .eq. cpump ) then
!
!                 - pump -
!
               call FLPP (il     ,ir     ,ngrid  ,istru  ,strclo ,&
               &strpar ,h      ,h1     ,maxtab ,ntabm  ,&
               &ntab   ,table  ,strhis ,asde   ,csde   ,&
               &esde   )
!
            else if ( strtyp(1,istru) .eq. cgenst ) then
!
!                 - general structure -
!
               call FLGS (g      ,il     ,ir     ,iter   ,ngrid  ,&
               &istru  ,lambda ,relstr ,dhstru ,&
               &strclo ,strpar ,h      ,h1     ,q      ,&
               &q1     ,q2     ,af     ,wf     ,lsalt  ,&
               &rho    ,strhis ,asde   ,bsde   ,csde   ,&
               &dsde   ,esde   ,juer   ,ker    )
!
            else if (strtyp(1,istru) .eq. cflume) then
!
! open flume
               call FLFLUM (g     , il    , ir    , ngrid , istru ,&
               &nstru , strpar, strclo, h     , h1    ,&
               &q     , q2    , af    , rho   ,&
               &strhis, asde  , csde  , esde  , hlev  ,&
               &maxlev)
!
            else if (strtyp(1,istru) .eq. cslubo) then
!
! sluice with bottom hinged gate
               call FLSLBO (g     , il    , ir    , ngrid , istru ,&
               &nstru , strpar, strclo, h     , h1    ,&
               &q     , q2    , af    , wf    , rho   ,&
               &strhis, asde  , csde  , esde  )
!
            else if (strtyp(1,istru) .eq. cclvrt) then
!
! culvert
               call FLCLVT (g     , il    , ir    , ngrid , istru ,&
               &nstru , strpar, strclo, h     , h1    ,&
               &q     , q2    , af    , wf    , rho   ,&
               &strhis, asde  , csde  , esde  )
!
            else if (strtyp(1,istru) .eq. cculpr) then
!
! culvert with pressure flow
               call FLCLPR (g     , il    , ir    , ngrid , istru ,&
               &nstru , strpar, strclo, h     , h1    ,&
               &q     , q2    , af    , wf    , rho   ,&
               &strhis, asde  , csde  , esde  )
!
            else if (strtyp(1,istru) .eq. cslund) then
!
! sluice with underflow gate
               call FLSLUN (g     , il    , ir    , ngrid , istru ,&
               &nstru , strpar, strclo, h     , h1    ,&
               &q     , q2    , af    , rho   ,&
               &strhis, asde  , csde  , esde  , hlev  ,&
               &maxlev)
!
            else if (strtyp(1,istru) .eq. csovun) then
!
! sluice with underflow gate
               call FLSLOU (g     , il    , ir    , ngrid , istru ,&
               &nstru , strpar, strclo, h     , h1    ,&
               &q     , q2    , af    , wf    , rho   ,&
               &strhis, asde  , csde  , esde  )
!
            else if (strtyp(1,istru) .eq. cbridg) then
!
! bridge piers
               call FLBRDG (g     , il    , ir    , ngrid , istru ,&
               &nstru , strpar, strclo, h     , h1    ,&
               &q     , q2    , af    , rho   ,&
               &strhis, asde  , csde  , esde  )
!
            else if (strtyp(1,istru) .eq. cabutm) then
!
! abutments
               call FLABUT (g     , il    , ir    , ngrid , istru ,&
               &nstru , strpar, h     , h1    ,&
               &q     , q2    , af    , rho   ,&
               &strhis, asde  , csde  , esde  )
!
            endif
!
!              Compute index for a2....e2
!
            k = il - i1 + 1
!
!              Add matrix coefficients
!              NOTE: b2 and d2 are calculated in FLNORM !
!
            a2(k) = a2(k) + dble(asde)
            b2(k) = b2(k) + dble(bsde)
            c2(k) = c2(k) + dble(csde)
            d2(k) = d2(k) + dble(dsde)
            e2(k) = e2(k) + dble(esde)
!
         endif
      endif
100 continue
!
end
