subroutine sadsco (ngrid  ,ngridm ,nbran  ,nstru  ,dt     ,&
&psi    ,theta  ,q1     ,q2     ,qltgim ,csa1  ,&
&csd1   ,source ,disgr  ,x      ,at1    ,at2   ,&
&af     ,branch ,strtyp ,salstr ,strclo ,strhis,&
&aa     ,ba     ,da     ,ea     ,fd     ,gd    ,&
&md     ,nd     ,ra     ,rd     ,&
&r1     ,f1     ,v1     ,r2     ,f2     ,v2    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SADSCO (SAlt Double Sweep COefficients)
!
! Module description: Computation of the double sweeped A,B,D,E-etc.
!                     coefficients R1,F1,V1 and R2,F2,V2 for all grid
!                     points in the network.
!
!                     In subroutine SAABCD the matrix coefficients (on
!                     diagonals) A,B,D,E-etc. will be computed for all
!                     gridpoints of a branch.
!
!                     Routine DSWEEP will perform a double sweep opera-
!                     tion on the just computed A,B,D,E-etc. matrix,
!                     resulting in coefficients R1,F1,V1 and R2,f2,V2
!                     for each grid point of the current branch.
!
!                     These double sweeped coefficients in all grid
!                     points are the final result of this subroutine.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 23 aa                P  -
! 18 af                P  -
! 16 at1               P  -
! 17 at2               P  -
! 24 ba                P  -
! 19 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 11 csa1              P  -
! 12 csd1              P  -
! 25 da                P  -
! 14 disgr             P  -
!  5 dt                P  -
! 26 ea                P  -
! 34 f1                P  -
! 37 f2                P  -
! 27 fd                P  -
! 28 gd                P  -
! 29 md                P  -
!  3 nbran             I  Number of branches.
! 30 nd                P  -
!  1 ngrid             I  Number of grid points in network.
!  2 ngridm            I  Maximum number of gridpoints in a branch.
!  4 nstru             P  -
!  6 psi               P  -
!  8 q1                P  -
!  9 q2                P  -
! 10 qltgim            P  -
! 33 r1                P  -
! 36 r2                P  -
! 31 ra                P  -
! 32 rd                P  -
! 21 salstr            P  -
! 13 source            P  -
! 22 strclo            P  -
! 20 strtyp            P  -
!  7 theta             P  -
! 35 v1                P  -
! 38 v2                P  -
! 15 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! dsweep  Double SWEEP coefficients
! saabcd  SAlt A,B,(C),D,e-etc. coeff. calculation
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sadsco.pf,v $
! Revision 1.5  1997/11/26  14:44:47  kuipe_j
! diffusion zero for free flow
!
! Revision 1.4  1995/10/18  09:00:17  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/08/23  14:29:39  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.2  1995/05/30  07:05:57  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:39  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:05  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:31  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer    ngrid  ,ngridm ,nbran  ,nstru
   real       psi    ,theta
   integer    branch(4,nbran),strtyp(10,*)
   real       qltgim(ngrid)  ,&
   &csa1  (ngrid)  ,csd1  (ngrid)  ,source(ngrid)  ,&
   &disgr (ngrid)  ,x     (ngrid)  ,at1   (ngrid)  ,&
   &at2   (ngrid)  ,af    (ngrid)  ,&
   &salstr(7,nstru),strhis(dmstrh,nstru)
   double precision  dt
   double precision&
   &q1    (ngrid)  ,q2    (ngrid) ,&
   &aa    (ngridm) ,ba   (ngridm) ,da    (ngridm) ,&
   &ea    (ngridm) ,fd   (ngridm) ,gd    (ngridm) ,&
   &md    (ngridm) ,nd   (ngridm) ,ra    (ngridm) ,&
   &rd    (ngridm) ,&
   &r1    (ngrid)  ,f1   (ngrid)  ,v1    (ngrid)  ,&
   &r2    (ngrid)  ,f2   (ngrid)  ,v2    (ngrid)
   logical    strclo(*)
!
!     Declaration of local parameters
!
   integer    i1    ,i2    ,ngbr  ,ibr
!
!     Loop over branches
!
   do 10 ibr = 1, nbran
!
!        Compute A,B,D,E-etc. coefficients for actual branch.
!
!
!        i1 = global grid point number at begin of branch
!        i2 = global grid point number at end of branch
!
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!JK      WRITE (99,*) 'TAK=',ibr
!JK      WRITE (99,*) '------------------------------------------'


      call saabcd (ngrid  ,ngridm ,nstru  ,i1     ,i2     ,dt    ,&
      &psi    ,theta  ,q1     ,q2     ,qltgim ,csa1  ,&
      &csd1   ,source ,disgr  ,x      ,at1    ,at2   ,&
      &af     ,strtyp ,salstr ,strclo ,strhis ,&
      &aa     ,ba     ,da     ,ea     ,fd     ,gd    ,&
      &md     ,nd     ,ra     ,rd     )

!
!        Perform double sweep operation on A,B,D,E-etc coefficients.
!
      ngbr = i2 - i1
      call dsweep (ngbr   ,ngridm ,&
      &aa     ,ba     ,da     ,ea     ,ra     ,&
      &fd     ,gd     ,md     ,nd     ,rd     ,&
      &r1(i1) ,f1(i1) ,v1(i1) ,&
      &r2(i1) ,f2(i1) ,v2(i1) )
!JK      do 99 i=i1,i2-1
!JK         WRITE (99,*)  'NA DSWEEP:,r1,f1,v1,r2,f2,v2 ',i
!JK         WRITE (99,*)   r1(i) ,f1(i) ,v1(i) ,
!JK  &                r2(i) ,f2(i) ,v2(i)
!JK9      continue
10 continue
!
end
