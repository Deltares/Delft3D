      subroutine FLDSCO(g      ,iter   ,dt1    ,steady ,psi    ,theta  ,
     +                  exrstp ,nbran  ,branch ,ngrid  ,lambda ,relstr ,
     +                  dhstru ,hp     ,qp     ,qlatgr ,grid   ,
     +                  x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,
     +                  ksi    ,lsalt  ,rho    ,rhow   ,a1m    ,nstru  ,
     +                  strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,
     +                  ngridm ,strclo ,strhis ,abcd1  ,abcd2  ,rfv1   ,
     +                  rfv2   ,theta2 ,omr    ,cflpse ,omcfl  ,dhtyp  ,
     +                  ibuf   ,istep  ,lfrou  ,qlat   ,qltpar ,nqlat  ,
     +                  hlev   ,maxlev ,solbuf ,stdbq  ,nstdb  ,juer   ,
     +                  ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLDSCO (FLow Double Sweep COefficients)
c
c Module description: Subroutine FLDSCO computes the double sweeped
c                     ABCDE coefficients r1,f1,v1 and r2,f2,v2 for all
c                     grid points in the network.
c
c                     In subroutine FLABCD the matrix coefficients
c                     (-diagonals) A,B,C,D and E will be computed, for
c                     the 'normal' gridpoints and the gridpoints con-
c                     nected to structures.
c
c                     Subroutine DSWEEP will perform a double sweep
c                     operation on the just computed ABCDE matrix,
c                     resulting in coefficients r1,f1,v1 and r2,f2,v2
c                     for each grid point.
c
c                     These double sweeped coefficients are the final
c                     result of subroutine FLDSCO.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 27 a1m               P  -
c 38 abcd1             P  -
c 39 abcd2             P  -
c 21 alfab             P  -
c  8 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 44 cflpse            P  -
c 19 cp                P  -
c 12 dhstru            P  -
c 46 dhtyp             P  -
c  3 dt1               P  -
c  1 g                 P  -
c 16 grid              P  -
c 13 hp                P  -
c  2 iter              P  -
c 48 istep             P  -
c 47 jufrou            P  -
c 23 ksi               P  -
c 10 lambda            P  -
c 51 lfrou             P  -
c 24 lsalt             P  -
c 31 maxtab            I  Maximum number of defined tables.
c  7 nbran             I  Number of branches.
c  9 ngrid             I  Number of grid points in network.
c 35 ngridm            I  Maximum number of gridpoints in a branch.
c 28 nstru             P  -
c 33 ntab              P  -
c 32 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 45 omcfl             P  -
c 43 omr               P  -
c  5 psi               P  -
c 15 qlatgr            P  -
c 14 qp                P  -
c 11 relstr            P  -
c 40 rfv1              P  -
c 41 rfv2              P  -
c 25 rho               P  -
c 26 rhow              P  -
c 20 rp                P  -
c  4 steady            P  -
c 36 strclo            P  -
c 37 strhis            P  -
c 30 strpar            P  -
c 29 strtyp            P  -
c 34 table             P  -
c 22 tauwi             P  -
c 42 theta2            P  -
c  6 theta             P  -
c 18 waoft             P  -
c 17 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c dsweep  Double SWEEP coefficients
c flabcd  FLow ABCDe coefficients
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: fldsco.pf,v $
c Revision 1.15  1999/03/15  14:21:40  kuipe_j
c Improve writing Froude file
c
c Revision 1.14  1998/05/25  19:11:45  kuipe_j
c Wendy structures
c
c Revision 1.13  1997/11/04  14:17:22  kuipe_j
c Retention basin
c
c Revision 1.12  1997/05/26  07:41:28  kuipe_j
c dicretization Q(H), H(Q) boundaries improved
c
c Revision 1.11  1997/01/23  08:29:02  kuipe_j
c Make flow module robust
c
c Revision 1.10  1996/10/31  10:30:17  kuipe_j
c Extra resistance finished
c
c Revision 1.9  1996/01/17  14:38:19  kuipe_j
c header update
c
c Revision 1.8  1995/11/21  11:07:48  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.7  1995/09/22  10:01:14  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:10:51  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:36:32  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:16  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:54:57  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:54  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:39  hoeks_a
c Initial check-in
c
c Revision 1.2  1994/11/28  08:37:25  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.1.1.1  1993/07/21  14:43:48  kuipe_j
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
      integer iter, nbran, ngrid, ngridm, nstru, maxtab, ntabm, exrstp,
     +        juer,  ker     
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
c
      double precision dt1, hlev(ngrid,maxlev)
      double precision abcd1(ngridm,5), abcd2(ngridm,5)
      double precision rfv1(ngrid,3)  , rfv2(ngrid,3)
      double precision hp(ngrid,3), qp(ngrid,3)
c
c     Declaration of local variables:
c
      integer ibr, i1, i2, n, i, ind, il, ir, inda
c
c     Store total discharge through all structures of a compound,
c     except simple weirs, in all members of the compound.
c     Store also the sum of the wetted areas of simple weirs in 
c     a compound in all members of the compund.
c     ARS 7170 (18-4-2001)
c
c     Initialize
      do i=1,nstru
         strhis(10,i) = 0.
         strhis(11,i) = 0.
      enddo
      
c     Add discharge or area      
      do i=1,nstru
         ind=nint(strhis(9,i))
         if (ind.gt.0)then
c           No weir, add discharge         
            strhis(10,ind) = strhis(10,ind) + abs(strhis(4,i))
         else 
c           Weir, calculate and add area
            il = strtyp(3,i)
            ir = strtyp(4,i)
            call flswarea  (g    ,il    ,ir   ,ngrid   ,i   ,strpar    ,
c                           <h>     <h1>    <q>     <q2>       <af>
     +                   hp(1,2) ,hp(1,1) ,qp(1,2) ,qp(1,3) ,waoft(1,3),
     +                   rho     )
            inda = -ind
            strhis(11,inda) = strhis(11,inda) + strpar(10,i)
         endif
      enddo

c     Copy summed discharge and area to all members       
      do i=1,nstru
         ind=abs(nint(strhis(9,i))) 
         if (i.ne.ind) then 
            strhis(10,i) = strhis(10,ind)
            strhis(11,i) = strhis(11,ind)
         endif   
      enddo
c     
c     Loop over branches
c
      do 10 ibr = 1, nbran
c
c        i1 = global grid point number at node n1
c        i2 = global grid point number at node n2
c
        i1 = branch (3,ibr)
        i2 = branch (4,ibr)
c
c        Compute ABCDE coefficients for actual branch
c
         call FLABCD (i1     ,i2     ,g      ,iter   ,dt1    ,steady ,
     +                psi    ,theta  ,exrstp ,ngrid  ,lambda ,relstr ,
     +                dhstru ,hp     ,qp     ,qlatgr ,grid   ,
     +                x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,
     +                ksi    ,lsalt  ,rho    ,rhow   ,a1m    ,nstru  ,
     +                strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,
     +                ngridm ,strclo ,strhis ,abcd1  ,abcd2  ,theta2 ,
     +                omr    ,cflpse ,omcfl  ,dhtyp  ,ibuf   ,istep  ,
     +                lfrou  ,qlat   ,qltpar ,nqlat  ,hlev   ,maxlev ,
     +                solbuf ,stdbq  ,nstdb  ,juer   ,ker    )

c
c        Perform double sweep operation on ABCDE coefficients
c
        n = i2 - i1
        call DSWEEP (n      ,ngridm ,
     +                abcd1(1,1)     ,abcd1(1,2)     ,abcd1(1,3)     ,
     +                abcd1(1,4)     ,abcd1(1,5)     ,
     +                abcd2(1,1)     ,abcd2(1,2)     ,abcd2(1,3)     ,
     +                abcd2(1,4)     ,abcd2(1,5)     ,
     +                rfv1 (i1,1)    ,rfv1 (i1,2)    ,rfv1 (i1,3)    ,
     +                rfv2 (i1,1)    ,rfv2 (i1,2)    ,rfv2 (i1,3)    )
   10 continue
      end
