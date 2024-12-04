subroutine FLNP1(lkalm  ,nbran  ,ngrid  ,nnf    ,branch ,typcr  ,&
&bfrict ,bfricp ,h2     ,q2     ,maxlev ,nlev   ,&
&hlev   ,wft    ,aft    ,overlp ,arex   ,arexcn ,&
&arexop ,of     ,maxtab ,ntabm  ,ntab   ,table  ,&
&sectc  ,sectv  ,prslot ,psltvr ,waoft  ,grsize ,&
&engpar ,scifri ,pfa    ,juer   ,cp     ,rp     ,&
&afwfqs ,alfab  ,wtt,att ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLNP1 (FLow variables for time level N + 1)
!
! Module description: Calculate the different variables for time level
!                     n+1.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 15 aft               P  -
! 37 afwfqs            P  -
! 38 alfab             P  -
! 17 arex              P  -
! 18 arexcn            P  -
! 19 arexop            P  -
!  8 bfricp            P  -
!  7 bfrict            P  -
!  5 branch            P  -
! 35 cp                P  -
! 31 engpar            P  -
! 30 grsize            P  -
! 13 hlev              P  -
!  9 hp                P  -
! 34 juer              P  -
! 39 ker               P  -
!  1 lkalm             I  -
! 11 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 21 maxtab            I  Maximum number of defined tables.
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
! 12 nlev              P  -
!  4 nnf               I  Number of uncertain bed friction parameters.
! 23 ntab              P  -
! 22 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 20 of                P  -
! 16 overlp            P  -
! 33 pfa               P  -
! 27 prslot            P  -
! 28 psltvr            P  -
! 10 qp                P  -
! 36 rp                P  -
! 32 scifri            P  -
! 25 sectc             P  -
! 26 sectv             P  -
! 24 table             P  -
!  6 typcr             P  -
! 29 waoft             P  -
! 14 wft               P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flawn1  FLow Areas and Widths on time level N+1
! flkac2  FLow Kalman Chezy correction 2
! flqsec  FLow Q in SECtions
! flvnp1  FLow Variables on time level N+1
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flnp1.pf,v $
! Revision 1.10  1998/06/11  11:46:56  kuipe_j
! Estuary special integrated
!
! Revision 1.9  1998/04/10  09:18:24  kuipe_j
! total area recalculated
!
! Revision 1.8  1997/01/23  08:29:11  kuipe_j
! Make flow module robust
!
! Revision 1.7  1996/04/12  13:04:08  kuipe_j
! headers, minor changes
!
! Revision 1.6  1996/04/11  08:23:43  kuipe_j
! Kalman module added
!
! Revision 1.5  1995/09/22  10:01:56  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:10:58  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.2  1993/11/26  15:31:15  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:52  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants
!
   include '..\include\sobdim.i'
!
!     Declaration of Parameters:
!
   integer maxtab, ntabm, juer, ker, nnf, ntab(4,maxtab)
   integer nbran, ngrid, maxlev, nlev(ngrid), branch(4,nbran)
   integer typcr(nbran), bfrict(3,nbran), arexcn(ngrid,2), arexop(2)
   integer scifri(ngrid)
   real    pfa(nnf)
   real    overlp, wft(ngrid,maxlev)
   real    of(ngrid,maxlev), aft(ngrid,maxlev)
   real    wtt(ngrid,maxlev), att(ngrid,maxlev)
   real    table(ntabm), arex(ngrid,4)
   real    cp(ngrid,4), rp(ngrid,4)
   real    grsize(4,ngrid,*), engpar(9)
   real    sectc(ngrid,3), sectv(ngrid,dmsecv)
   real    afwfqs(ngrid,8), waoft(ngrid,*) , alfab(ngrid)
   real    bfricp(6,ngrid), prslot(3,nbran), psltvr(7,ngrid)
   double precision hlev(ngrid,maxlev)
   double precision h2(ngrid), q2(ngrid)
   logical lkalm
!
!     Calculate variables on time level n+1
!
!     Af an Wf
!
   call FLAWN1 (nbran   ,ngrid  ,branch     ,typcr      ,prslot     ,&
   &h2      ,maxlev ,nlev       ,hlev       ,wft        ,&
   &aft     ,overlp ,arex       ,arexcn     ,arexop     ,&
   &of      ,juer   ,waoft(1,1) ,waoft(1,3) ,waoft(1,6) ,&
   &wtt     ,att    ,waoft(1,4) ,&
   &ker     ,psltvr )
!
!     Chezy values, hydraulic radius, area and widths for each gridpoint
!     and section
!
   call FLVNP1 (nbran      ,ngrid      ,branch      ,typcr      ,&
   &bfrict     ,bfricp     ,h2          ,q2         ,&
   &maxlev     ,hlev       ,wft         ,maxtab     ,&
!                                                       <subsec>
   &ntabm      ,ntab       ,table       ,sectc(1,1) ,&
!                  <secth0>    <secth1>    <Wf>         <Wfh0>
   &sectv(1,2) ,sectv(1,3) ,waoft(1,1)  ,sectc(1,2) ,&
!                  <Wfh1>                               <Af>
   &sectc(1,3) ,grsize     ,engpar      ,waoft(1,3) ,&
!                  <O>         <Afh0>      <Afh1>       <Oh0>
   &waoft(1,6) ,sectv(1,4) ,sectv(1,5)  ,sectv(1,6) ,&
!                  <Oh1>       <asubsc>
   &sectv(1,7) ,sectv(1,1) ,prslot      ,psltvr     ,&
   &cp(1,1)    ,rp(1,1)    ,cp(1,2)     ,rp(1,2)    ,&
   &afwfqs(1,1),afwfqs(1,3),alfab       )
!
!     Q distribution for each gridpoint and section
!
   call FLQSEC(ngrid       ,q2          ,waoft(1,3) ,sectv(1,1) ,&
   &cp(1,1)     ,rp(1,1)     ,cp(1,2)    ,rp(1,2)    ,&
   &afwfqs(1,1) ,afwfqs(1,7) )
!
!     Correct Chezy values
!
   if ( lkalm ) then
      call FLKAC2 (ngrid  ,nnf    ,scifri ,pfa    ,sectv(1,1)   ,&
      &cp(1,1)        ,cp(1,2) )
   endif
!
end
