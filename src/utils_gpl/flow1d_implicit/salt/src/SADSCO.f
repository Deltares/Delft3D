      subroutine sadsco (ngrid  ,ngridm ,nbran  ,nstru  ,dt     ,
     &                   psi    ,theta  ,q1     ,q2     ,qltgim ,csa1  ,
     &                   csd1   ,source ,disgr  ,x      ,at1    ,at2   ,
     &                   af     ,branch ,strtyp ,salstr ,strclo ,strhis,
     &                   aa     ,ba     ,da     ,ea     ,fd     ,gd    ,
     &                   md     ,nd     ,ra     ,rd     ,
     &                   r1     ,f1     ,v1     ,r2     ,f2     ,v2    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SADSCO (SAlt Double Sweep COefficients)
c
c Module description: Computation of the double sweeped A,B,D,E-etc.
c                     coefficients R1,F1,V1 and R2,F2,V2 for all grid
c                     points in the network.
c
c                     In subroutine SAABCD the matrix coefficients (on
c                     diagonals) A,B,D,E-etc. will be computed for all
c                     gridpoints of a branch.
c
c                     Routine DSWEEP will perform a double sweep opera-
c                     tion on the just computed A,B,D,E-etc. matrix,
c                     resulting in coefficients R1,F1,V1 and R2,f2,V2
c                     for each grid point of the current branch.
c
c                     These double sweeped coefficients in all grid
c                     points are the final result of this subroutine.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 23 aa                P  -
c 18 af                P  -
c 16 at1               P  -
c 17 at2               P  -
c 24 ba                P  -
c 19 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 11 csa1              P  -
c 12 csd1              P  -
c 25 da                P  -
c 14 disgr             P  -
c  5 dt                P  -
c 26 ea                P  -
c 34 f1                P  -
c 37 f2                P  -
c 27 fd                P  -
c 28 gd                P  -
c 29 md                P  -
c  3 nbran             I  Number of branches.
c 30 nd                P  -
c  1 ngrid             I  Number of grid points in network.
c  2 ngridm            I  Maximum number of gridpoints in a branch.
c  4 nstru             P  -
c  6 psi               P  -
c  8 q1                P  -
c  9 q2                P  -
c 10 qltgim            P  -
c 33 r1                P  -
c 36 r2                P  -
c 31 ra                P  -
c 32 rd                P  -
c 21 salstr            P  -
c 13 source            P  -
c 22 strclo            P  -
c 20 strtyp            P  -
c  7 theta             P  -
c 35 v1                P  -
c 38 v2                P  -
c 15 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c dsweep  Double SWEEP coefficients
c saabcd  SAlt A,B,(C),D,e-etc. coeff. calculation
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sadsco.pf,v $
c Revision 1.5  1997/11/26  14:44:47  kuipe_j
c diffusion zero for free flow
c
c Revision 1.4  1995/10/18  09:00:17  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/08/23  14:29:39  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.2  1995/05/30  07:05:57  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:39  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:17:05  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:33:31  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer    ngrid  ,ngridm ,nbran  ,nstru
      real       psi    ,theta
      integer    branch(4,nbran),strtyp(10,*)
      real       qltgim(ngrid)  ,
     &           csa1  (ngrid)  ,csd1  (ngrid)  ,source(ngrid)  ,
     &           disgr (ngrid)  ,x     (ngrid)  ,at1   (ngrid)  ,
     &           at2   (ngrid)  ,af    (ngrid)  ,
     &           salstr(7,nstru),strhis(dmstrh,nstru)
      double precision  dt
      double precision
     &           q1    (ngrid)  ,q2    (ngrid) ,
     &           aa    (ngridm) ,ba   (ngridm) ,da    (ngridm) ,
     &           ea    (ngridm) ,fd   (ngridm) ,gd    (ngridm) ,
     &           md    (ngridm) ,nd   (ngridm) ,ra    (ngridm) ,
     &           rd    (ngridm) ,
     &           r1    (ngrid)  ,f1   (ngrid)  ,v1    (ngrid)  ,
     &           r2    (ngrid)  ,f2   (ngrid)  ,v2    (ngrid)
      logical    strclo(*)
c
c     Declaration of local parameters
c
      integer    i1    ,i2    ,ngbr  ,ibr
c
c     Loop over branches
c
      do 10 ibr = 1, nbran
c
c        Compute A,B,D,E-etc. coefficients for actual branch.
c
c
c        i1 = global grid point number at begin of branch
c        i2 = global grid point number at end of branch
c
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
CJK      WRITE (99,*) 'TAK=',ibr
CJK      WRITE (99,*) '------------------------------------------'


         call saabcd (ngrid  ,ngridm ,nstru  ,i1     ,i2     ,dt    ,
     &                psi    ,theta  ,q1     ,q2     ,qltgim ,csa1  ,
     &                csd1   ,source ,disgr  ,x      ,at1    ,at2   ,
     &                af     ,strtyp ,salstr ,strclo ,strhis ,
     &                aa     ,ba     ,da     ,ea     ,fd     ,gd    ,
     &                md     ,nd     ,ra     ,rd     )

c
c        Perform double sweep operation on A,B,D,E-etc coefficients.
c
         ngbr = i2 - i1
         call dsweep (ngbr   ,ngridm ,
     &                aa     ,ba     ,da     ,ea     ,ra     ,
     &                fd     ,gd     ,md     ,nd     ,rd     ,
     &                r1(i1) ,f1(i1) ,v1(i1) ,
     &                r2(i1) ,f2(i1) ,v2(i1) )
CJK      do 99 i=i1,i2-1
CJK         WRITE (99,*)  'NA DSWEEP:,r1,f1,v1,r2,f2,v2 ',i
CJK         WRITE (99,*)   r1(i) ,f1(i) ,v1(i) ,
CJK  &                r2(i) ,f2(i) ,v2(i)
CJK9      continue
   10 continue
c
      end
