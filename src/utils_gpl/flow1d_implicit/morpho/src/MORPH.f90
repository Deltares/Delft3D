subroutine MORPH  ( ngrid  ,nbran  ,nboun  ,nnode  ,&
&nbrnod ,ntmpgr ,time   ,dtm    ,&
&morpar ,flwdir ,sumda  ,&
&grid   ,branch ,node   ,typcr  ,&
&brnode ,bgout  ,tmpgr  ,&
&maxtab ,ntabm  ,ntab   ,table  ,&
&mbdpar ,x      ,&
&maxlev ,nlev   ,hlev   ,h2     ,&
&waoft  ,wft    ,ws     ,&
&sectc  ,afwfqs ,&
&celer  ,sedtr  ,dissed ,slat   ,&
&itim   ,juer   ,ker    ,prslot )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MORPH (MORPHology)
!
! Module description: Calculate the change in cross sectional area by
!                     solving a continuity equation and adapt the cross
!                     sectional dimensions for each gridpoint in the
!                     network. If a branch is of type sedredge the con-
!                     tinuity equation is solved for the left and right
!                     side of the channel.
!
!                     For each branch in the network routine MOAREA is
!                     called. If a branch is of type sedredge the routi-
!                     ne is called twice. One time for the left side of
!                     the channel and one time for the right side of the
!                     channel.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 33 afwfqs            P  -
! 16 bgout             P  -
! 12 branch            P  -
! 15 brnode            P  -
! 34 celer             P  -
! 36 dissed            P  -
!  8 dtm               P  -
! 10 flwdir            P  -
! 11 grid              P  -
! 27 h                 P  -
! 26 hlev              P  -
! 38 itim(2)           I  Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
! 39 juer              P  -
! 40 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 24 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 18 maxtab            I  Maximum number of defined tables.
! 22 mbdpar            P  -
!  9 morpar(20)        I  (1) = Reduction parameter for actual tr.width
!                         (2) = Maximum number of iteration steps allowed
!                         (3) = Stability factor (>1)
!                         (4) = Method of adapting cross sections
!                               ceqows (1) : Equally over the actual
!                                            sediment tr. width of the
!                                            cross section
!                               cprodp (2) : Proportional to the local
!                                            water depth across the cross
!                                            section
!                          (5) = Limiter constant
!                          (6) = Switch used in formulas (mopta)
!                          (7) = Switch used in formulas (moptb)
!                          (8) = Switch used in formulas (moptc)
!                          (9) = Switch used in formulas (moptd)
!                          (10)= Switch used in formulas (mopte)
!                          (11)= Switch used in formulas (moptf)
!  3 nboun             I  Number of boundary nodes.
!  2 nbran             I  Number of branches.
!  5 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  1 ngrid             I  Number of grid points in network.
! 25 nlev              P  -
!  4 nnode             I  Number of nodes.
! 13 node              P  -
! 20 ntab              P  -
! 19 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  6 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
! 41 prslot            P  -
! 32 sectc             P  -
! 35 sedtr             P  -
! 37 slat              P  -
! 21 table             P  -
!  7 time              P  -
! 17 tmpgr             P  -
! 14 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 28 waoft             P  -
! 29 wft               P  -
! 31 ws                P  -
! 30 wtt               P  -
! 23 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
! error   write an ERROR to the error file.
! moarea  MOrphology AREA
! moczio  MOrphology Check Z Increasing Order
! mointn  MOrphology INTegrals for Nodes
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: morph.pf,v $
! Revision 1.11  1999/03/15  15:53:03  kuipe_j
! tabs removed
!
! Revision 1.10  1998/06/11  11:47:20  kuipe_j
! Estuary special integrated
!
! Revision 1.9  1997/06/17  11:26:59  kuipe_j
! output in history format
!
! Revision 1.8  1997/02/17  10:23:17  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.7  1997/01/23  08:29:50  kuipe_j
! Make flow module robust
!
! Revision 1.6  1996/03/08  09:39:15  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.5  1996/03/07  10:44:24  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.4  1995/10/18  09:00:04  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:55:56  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:04:59  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:25  hoeks_a
! Initial check-in
!
! Revision 1.4  1994/12/02  13:03:09  kuipe_j
! Wrong water level was used for checking for negative depth.
! Error handling improved.
!
! Revision 1.3  1994/11/28  08:52:41  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:58  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:09  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Parameters
!
   integer   ngrid  ,nbran  ,nboun  ,nnode  ,&
   &nbrnod ,ntmpgr ,maxtab ,&
   &ntabm  ,maxlev ,juer   ,ker

   integer   grid   (ngrid),&
   &branch (4,nbran),&
   &flwdir (ngrid),&
   &node   (4,nnode),&
   &brnode (nbrnod+1,nnode),&
   &bgout  (3,nbrnod),&
   &typcr  (nbran),&
   &ntab   (4,maxtab),&
   &mbdpar (5,nboun),&
   &nlev   (ngrid),&
   &itim   (2)

   real      morpar (20),&
   &table  (ntabm),&
   &x      (ngrid),&
   &waoft  (ngrid,6),&
   &wft    (ngrid,maxlev),&
   &ws     (ngrid),&
   &sectc  (ngrid,3),&
   &afwfqs (ngrid,8),&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*),&
   &dissed (4,nbran),&
   &slat   (ngrid,*),&
   &tmpgr  (ngrid,ntmpgr),&
   &prslot (3,nbran),sumda (ngrid)

   double    precision  time ,dtm, hlev (ngrid,maxlev)
   double    precision  h2(ngrid)

!
!     Local variables
!
   integer ibr, isec,moropt
!
!
   real    alphac,alphad

   logical mopta, moptb, moptc, moptd, mopte, moptf
   logical epsequ

   character*18 txt
   external epsequ
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
   alphac = morpar(3)
   moropt = INT(morpar(4))
   alphad = morpar(5)
   mopta  = (INT(morpar(6)) .eq. 1)
   moptb  = (INT(morpar(7)) .eq. 1)
   moptc  = (INT(morpar(8)) .eq. 1)
   moptd  = (INT(morpar(9)) .eq. 1)
   mopte  = (INT(morpar(10)) .eq. 1)
!
!     Totdat het UI is aangepast op langer array morpar (20 ipv 10)
!     moet moptf hard op false gezet worden.
!
!      moptf  = (INT(morpar(11)) .eq. 1)
   moptf = .FALSE.
!
!     set default alphad =1
!
   if (epsequ(alphad ,0. ,cdchk)) then
      alphad = 1.
   endif
!
!     Calculate integral values for nodes and boundaries
!
   call MOINTN (nnode  ,nbran  ,nbrnod ,ngrid  ,nboun ,maxtab ,&
   &ntabm  ,time   ,dtm    ,alphac ,branch,brnode ,&
   &typcr  ,bgout  ,grid   ,node   ,mbdpar,ntab   ,&
!                         <intgr>
   &dissed ,tmpgr  ,x      ,table  ,celer ,sedtr  ,&
   &mopta  ,moptb  ,moptc  ,moptd  ,alphad,flwdir ,&
   &juer   ,ker    )

!
!     Check for error
!
   if (ker .eq. fatal) goto 1000
!
!     Do for each branch in network
!
   do 200 ibr = 1, nbran

      if (typcr(ibr) .eq. ccrtab) then
!
!           Process continuity equation for whole channel
!
         isec = 1
         call MOAREA ( isec   ,ngrid  ,nbran  ,&
         &nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
         &node   ,typcr(ibr) ,mbdpar ,hlev ,nlev ,&
         &grid   ,maxtab ,ntabm  ,ntab   ,&
!                          <h_n+1> <wf>        <wfh0>
         &table  ,h2     ,waoft(1,1),sectc(1,2)  ,&
!                                  <A-main>
         &ws     ,wft    ,afwfqs(1,1),dissed ,&
         &x      ,time   ,dtm    ,alphac ,&
!                                  <intgr>
         &celer  ,sedtr  ,tmpgr  ,flwdir ,sumda  ,&
         &alphad ,mopta  ,moptb  ,moptc  ,moptd  ,&
         &mopte  ,moptf  ,slat   ,moropt ,&
         &juer   ,ker&
         &)

      elseif (typcr(ibr) .eq. ccrsed) then
!
!           Process continuity equation for left and right channel
!
         do 100 isec = 1, 2
            call MOAREA ( isec   ,ngrid  ,nbran  ,&
            &nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
            &node   ,typcr(ibr) ,mbdpar ,hlev,nlev ,&
            &grid   ,maxtab ,ntabm  ,ntab   ,&
!                              <h_n+1> <wf>        <wfh0>
            &table  ,h2     ,waoft(1,1),sectc(1,2)  ,&
!                                       <A-main>
            &ws     ,wft    ,afwfqs(1,1),dissed ,&
            &x      ,time   ,dtm    ,alphac ,&
!                                      <intgr>
            &celer  ,sedtr  ,tmpgr  ,flwdir ,sumda  ,&
            &alphad ,mopta  ,moptb  ,moptc  ,moptd  ,&
            &mopte  ,moptf  ,slat   ,moropt ,&
            &juer   ,ker&
            &)

100      continue
      endif
200 continue
!
!     Check for z in increasing order
!
   call MOCZIO ( nbran  ,ngrid  ,maxlev ,branch ,nlev   ,&
!                                  <h_n+1>
   &hlev   ,typcr  ,h2     ,&
   &juer   ,ker    ,prslot )
!
!     Error label
!
1000 continue
!
   if (ker .ne. ok) then
      write (txt,'(2(1x,i8))') itim
      call ERROR (juer,'MORP timestep@'//txt//'@',emomes,info)
      if (ker .ne. fatal) ker = ok
   endif
!
end
