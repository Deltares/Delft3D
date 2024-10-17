subroutine FLDSCO(g      ,iter   ,dt1    ,steady ,psi    ,theta  ,&
&exrstp ,nbran  ,branch ,ngrid  ,lambda ,relstr ,&
&dhstru ,hp     ,qp     ,qlatgr ,grid   ,&
&x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,&
&ksi    ,lsalt  ,rho    ,rhow   ,a1m    ,nstru  ,&
&strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,&
&ngridm ,strclo ,strhis ,abcd1  ,abcd2  ,rfv1   ,&
&rfv2   ,theta2 ,omr    ,cflpse ,omcfl  ,dhtyp  ,&
&ibuf   ,istep  ,lfrou  ,qlat   ,qltpar ,nqlat  ,&
&hlev   ,maxlev ,solbuf ,stdbq  ,nstdb  ,juer   ,&
&ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLDSCO (FLow Double Sweep COefficients)
!
! Module description: Subroutine FLDSCO computes the double sweeped
!                     ABCDE coefficients r1,f1,v1 and r2,f2,v2 for all
!                     grid points in the network.
!
!                     In subroutine FLABCD the matrix coefficients
!                     (-diagonals) A,B,C,D and E will be computed, for
!                     the 'normal' gridpoints and the gridpoints con-
!                     nected to structures.
!
!                     Subroutine DSWEEP will perform a double sweep
!                     operation on the just computed ABCDE matrix,
!                     resulting in coefficients r1,f1,v1 and r2,f2,v2
!                     for each grid point.
!
!                     These double sweeped coefficients are the final
!                     result of subroutine FLDSCO.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 27 a1m               P  -
! 38 abcd1             P  -
! 39 abcd2             P  -
! 21 alfab             P  -
!  8 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 44 cflpse            P  -
! 19 cp                P  -
! 12 dhstru            P  -
! 46 dhtyp             P  -
!  3 dt1               P  -
!  1 g                 P  -
! 16 grid              P  -
! 13 hp                P  -
!  2 iter              P  -
! 48 istep             P  -
! 47 jufrou            P  -
! 23 ksi               P  -
! 10 lambda            P  -
! 51 lfrou             P  -
! 24 lsalt             P  -
! 31 maxtab            I  Maximum number of defined tables.
!  7 nbran             I  Number of branches.
!  9 ngrid             I  Number of grid points in network.
! 35 ngridm            I  Maximum number of gridpoints in a branch.
! 28 nstru             P  -
! 33 ntab              P  -
! 32 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 45 omcfl             P  -
! 43 omr               P  -
!  5 psi               P  -
! 15 qlatgr            P  -
! 14 qp                P  -
! 11 relstr            P  -
! 40 rfv1              P  -
! 41 rfv2              P  -
! 25 rho               P  -
! 26 rhow              P  -
! 20 rp                P  -
!  4 steady            P  -
! 36 strclo            P  -
! 37 strhis            P  -
! 30 strpar            P  -
! 29 strtyp            P  -
! 34 table             P  -
! 22 tauwi             P  -
! 42 theta2            P  -
!  6 theta             P  -
! 18 waoft             P  -
! 17 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! dsweep  Double SWEEP coefficients
! flabcd  FLow ABCDe coefficients
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: fldsco.pf,v $
! Revision 1.15  1999/03/15  14:21:40  kuipe_j
! Improve writing Froude file
!
! Revision 1.14  1998/05/25  19:11:45  kuipe_j
! Wendy structures
!
! Revision 1.13  1997/11/04  14:17:22  kuipe_j
! Retention basin
!
! Revision 1.12  1997/05/26  07:41:28  kuipe_j
! dicretization Q(H), H(Q) boundaries improved
!
! Revision 1.11  1997/01/23  08:29:02  kuipe_j
! Make flow module robust
!
! Revision 1.10  1996/10/31  10:30:17  kuipe_j
! Extra resistance finished
!
! Revision 1.9  1996/01/17  14:38:19  kuipe_j
! header update
!
! Revision 1.8  1995/11/21  11:07:48  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.7  1995/09/22  10:01:14  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:10:51  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:36:32  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:16  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:54:57  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:54  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:39  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/11/28  08:37:25  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.1.1.1  1993/07/21  14:43:48  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters:
!
   integer iter, nbran, ngrid, ngridm, nstru, maxtab, ntabm, exrstp,&
   &juer,  ker
   integer branch(4,nbran), ntab(4,maxtab), grid(ngrid)
   integer strtyp(10,*), ibuf(*),istep, nqlat
   integer maxlev,nstdb
   logical lsalt, steady, strclo(*), lfrou
   real    g, psi, theta, rhow ,omcfl  ,dhtyp
   real    lambda ,relstr ,dhstru
   real    qlatgr(ngrid)
   real    x(ngrid), waoft(ngrid,*)
   real    cp(ngrid,4), rp(ngrid,4), alfab(ngrid)
   real    tauwi(ngrid), ksi(ngrid), rho(ngrid), a1m(ngrid)
   real    strpar(dmstrpar,*), table(ntabm),stdbq(nstdb)
   real    theta2,omr,cflpse
   real    strhis(dmstrh,*)
   real    qltpar(9,nqlat), qlat(nqlat,9)
   real    solbuf(dmbuf2,7,ngrid)
!
   double precision dt1, hlev(ngrid,maxlev)
   double precision abcd1(ngridm,5), abcd2(ngridm,5)
   double precision rfv1(ngrid,3)  , rfv2(ngrid,3)
   double precision hp(ngrid,3), qp(ngrid,3)
!
!     Declaration of local variables:
!
   integer ibr, i1, i2, n, i, ind, il, ir, inda
!
!     Store total discharge through all structures of a compound,
!     except simple weirs, in all members of the compound.
!     Store also the sum of the wetted areas of simple weirs in
!     a compound in all members of the compund.
!     ARS 7170 (18-4-2001)
!
!     Initialize
   do i=1,nstru
      strhis(10,i) = 0.
      strhis(11,i) = 0.
   enddo

!     Add discharge or area
   do i=1,nstru
      ind=nint(strhis(9,i))
      if (ind.gt.0)then
!           No weir, add discharge
         strhis(10,ind) = strhis(10,ind) + abs(strhis(4,i))
      else
!           Weir, calculate and add area
         il = strtyp(3,i)
         ir = strtyp(4,i)
         call flswarea  (g    ,il    ,ir   ,ngrid   ,i   ,strpar    ,&
!                           <h>     <h1>    <q>     <q2>       <af>
         &hp(1,2) ,hp(1,1) ,qp(1,2) ,qp(1,3) ,waoft(1,3),&
         &rho     )
         inda = -ind
         strhis(11,inda) = strhis(11,inda) + strpar(10,i)
      endif
   enddo

!     Copy summed discharge and area to all members
   do i=1,nstru
      ind=abs(nint(strhis(9,i)))
      if (i.ne.ind) then
         strhis(10,i) = strhis(10,ind)
         strhis(11,i) = strhis(11,ind)
      endif
   enddo
!
!     Loop over branches
!
   do 10 ibr = 1, nbran
!
!        i1 = global grid point number at node n1
!        i2 = global grid point number at node n2
!
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!
!        Compute ABCDE coefficients for actual branch
!
      call FLABCD (i1     ,i2     ,g      ,iter   ,dt1    ,steady ,&
      &psi    ,theta  ,exrstp ,ngrid  ,lambda ,relstr ,&
      &dhstru ,hp     ,qp     ,qlatgr ,grid   ,&
      &x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,&
      &ksi    ,lsalt  ,rho    ,rhow   ,a1m    ,nstru  ,&
      &strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,&
      &ngridm ,strclo ,strhis ,abcd1  ,abcd2  ,theta2 ,&
      &omr    ,cflpse ,omcfl  ,dhtyp  ,ibuf   ,istep  ,&
      &lfrou  ,qlat   ,qltpar ,nqlat  ,hlev   ,maxlev ,&
      &solbuf ,stdbq  ,nstdb  ,juer   ,ker    )

!
!        Perform double sweep operation on ABCDE coefficients
!
      n = i2 - i1
      call DSWEEP (n      ,ngridm ,&
      &abcd1(1,1)     ,abcd1(1,2)     ,abcd1(1,3)     ,&
      &abcd1(1,4)     ,abcd1(1,5)     ,&
      &abcd2(1,1)     ,abcd2(1,2)     ,abcd2(1,3)     ,&
      &abcd2(1,4)     ,abcd2(1,5)     ,&
      &rfv1 (i1,1)    ,rfv1 (i1,2)    ,rfv1 (i1,3)    ,&
      &rfv2 (i1,1)    ,rfv2 (i1,2)    ,rfv2 (i1,3)    )
10 continue
end
