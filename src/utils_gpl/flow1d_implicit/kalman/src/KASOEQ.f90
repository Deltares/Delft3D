subroutine KASOEQ(nnode  ,nnn    ,nbran  ,nosdim ,scnodl ,&
&branch ,ngridm ,rfv1   ,&
&rfv2   ,calsol ,scnode ,snnode ,sclnod ,nbrnod ,&
&brnode ,mat    ,rhs    ,indx   ,ngrid  ,np     ,&
&kbeta  ,abcd1  ,kabcd1 ,kabcd2 ,rkmat  ,q      )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KASOEQ (KAlman SOlve EQuations)
!
! Module description: The complete system of equations in C.S=R will be
!                     solved column per column. For each column the next
!                     steps are performed:
!                     -    Calculate right hand side of system of node
!                          equations.
!                     -    Solve and make back substitution in large
!                          matrix.
!
!                     do for each column do
!                     -    The current column of the matrix contains
!                          coefficients E1 and E2. Use these a.o. to
!                          calculate right hand side vector v1 and v2.
!                          (KASWPR)
!                          Build right hand side RHS(NNODE) using BETA,
!                          v1 and v2. (KASBNR)
!                     -    Solve node equations. (LUBKSB)
!                     -    H-variances at nodes are known after the
!                          previous step. These variances are back
!                          substituted in the double sweeped equations
!                          (using coefficients r1, r2, f1, f2, v1 and v2).
!                          The obtained covariances are stored in the
!                          same column of the matrix (KABRAN)
!                     enddo
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 20 abcd1(ngridm,5)   O  (i,1) = A1 or a coefficient for grid point i
!                         (i,2) = B1 or b coefficient for grid point i
!                         (i,3) = C1 or d coefficient for grid point i
!                         (i,4) = D1 or e coefficient for grid point i
!                         (i,5) = E1 or r1 coefficient for grid point i
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 13 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
!        ,nnode)          contains the number of connected branches
!                         (index 1) for each node. The second index
!                         contains the first connected branch number
!                         etc.
!  8 calsol            I  -
! 16 indx              P  -
! 21 kabcd1            P  -
! 22 kabcd2            P  -
! 19 kbeta             P  -
! 14 mat               P  -
!  3 nbran             I  Number of branches.
! 12 nbrnod            I  Maximum number of connected branches to one
!                         node.
! 17 ngrid             I  Number of grid points in network.
!  5 ngridm            I  Maximum number of gridpoints in a branch.
!  2 nnn               I  Number of uncorrelated random noise processes
!                         for nodal (= boundary) equations.
!  1 nnode             I  Number of nodes.
! 18 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  6 rfv1              P  -
!  7 rfv2              P  -
! 15 rhs(nnode)        IO (i) contains at:
!                               input:  Right-hand-side of the equation
!                                       at node i of the set of node
!                                       equations.
!                               output: Solution in node i.
! 23 rkmat(np,np)      I  Right hand side matrix.
! 11 sclnod(nnn+1)     I  sclnod(i) points to begin of group i of nodes
!                         (in array scnode) with correlated r.n. process
!                         for node equations.
!  9 scnode(scnodl)    I  Node numbers of all uncorrelated r.n. processes
!                         for node equations. The node numbers are grouped
!                         per process.
! 10 snnode(nosdim,nnn)IO Boundary noise for each group.
!                         (1,i) mean
!                         (2,i) deviation (input) or variance
!                         (3,i) indicator for absolute(0) or relative
!                               (1) noise variance
!                         (4,i) time correlation
!                         (5,i) current variance
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! kabran  KAlman in BRANches
! kasbnr  KALman Substitute BouNdary in Right h.s.
! kaswpr  KAlman double SWeeP R.h.side
! lubksb  LU decomposition; BacKSuBstitution
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kasoeq.pf,v $
! Revision 1.4  1999/03/15  15:52:20  kuipe_j
! tabs removed
!
! Revision 1.3  1996/12/05  10:00:06  kuipe_j
! Smoothing kgain,linearization,limit covariance,etc
!
! Revision 1.2  1996/04/12  13:05:28  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:01  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer nnode, nbrnod, nnn, ngrid, nbran, ngridm, np, calsol,&
   &scnodl,nosdim
   integer sclnod(nnn+1), scnode(scnodl), indx(nnode)
   integer branch(4,nbran), brnode(nbrnod+1,nnode)
   real    snnode(nosdim,nnn)
   real    rkmat(np,np)
!
   double precision q(ngrid), kabcd1(ngrid,8), kabcd2(ngrid,14)
   double precision rfv1(ngrid,3), rfv2(ngrid,3)
   double precision mat(nnode,nnode), rhs(nnode)
   double precision kbeta(2,nbran), abcd1(ngridm,5)
!
!     Declaration of local variables:
!
   integer j, k, ibr, i, i1, i2, ind, m, nr, n, igp
   logical found
!
!     Add time correlation
!
   do m = 1, nnn
      snnode(5,m) = snnode(4,m)**2 * snnode(5,m) + snnode(2,m)
   enddo
!
   do 60 j = 1, np

      do 10  i=1,nnode
         rhs(i) = 0.D0
10    continue
!
      do 30 ibr = 1, nbran
!
!           i1 = global grid point number at node n1
!           i2 = global grid point number at node n2
!
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
!
!           Column j of matrix RKMAT contains coefficients E1 and E2,
!           E1 starts at RKMAT(1,j) and E2 at RKMAT(ngrid+1,j) .
!           E1, E2 are used to calculate right hand side vector
!           v1 (=rfv1(i1,3)) and v2 (=rfv2(i1,3)) .
!
         k = 1
         do 20 i = i1, i2-1
            abcd1(k,1) = dble(rkmat(i,j))
            abcd1(k,2) = dble(rkmat(ngrid+i,j))
            k = k + 1
20       continue
!
         call KASWPR (i2-i1  ,ngridm ,&
         &kabcd1(i1,1)   ,kabcd1(i1,2)   ,kabcd1(i1,3)  ,&
         &kabcd1(i1,4)   ,abcd1(1,1)     ,&
         &kabcd2(i1,1)   ,kabcd2(i1,2)   ,kabcd2(i1,3)  ,&
         &kabcd2(i1,4)   ,abcd1(1,2)     ,&
         &rfv1 (i1,3)    ,&
         &rfv2 (i1,3)    )
30    continue
!
!        Build right hand side RHS(NNODE) using BETA, v1 and v2.
!
      call KASBNR (nnode  ,nbran  ,branch ,ngrid  ,kbeta  ,&
      &rfv1(1,3)      ,rfv2(1,3)      ,rhs    )
!
!        Adaption for 'nodal administration equations' required?
!        System noise is allowed only at external nodes.
!
      if (calsol .eq. 2) then
         do 50 m = 1, nnn
            found = .false.
            do 40 ind = sclnod(m), sclnod(m+1)-1
               n = scnode(ind)
               if (brnode(1,n) .eq. 1) then
!                    external node
                  ibr = brnode(2,n)
                  if (branch(1,ibr) .eq. n) then
!                       begin of branch
!                       Equation will be stored at H-variance part
!                       of matrix
                     igp = branch(3,ibr)
                  else
!                       end of branch
!                       Equation will be stored at Q-variance part
!                       of matrix
                     igp = branch(4,ibr) + ngrid
                  endif
                  if (igp .eq. j) found = .true.
               endif
40          continue
            if (found) then
               do 45 ind = sclnod(m), sclnod(m+1)-1
                  nr      = scnode(ind)
                  if (nosdim.le.2 .or. int(snnode(3,m)).eq.0) then
                     rhs(nr) = rhs(nr) + snnode(5,m)
                  else
                     rhs(nr) = rhs(nr) + q(nr)**2 * snnode(5,m)
                  endif
45             continue
            endif
50       continue


      else if (calsol .eq. 3) then

         do 55 n=1,nnode
            if (brnode(1,n) .eq. 1) then
!                 external node
               ibr = brnode(2,n)
               if (branch(1,ibr) .eq. n) then
!                    begin of branch
                  igp = branch(3,ibr)
               else
!                    end of branch
                  igp = branch(4,ibr) + ngrid
               endif
               rhs(n) = rhs(n) + rkmat(igp,j)
            endif
55       continue

      endif
!
!        Solve node equations
!
      call LUBKSB (mat    ,nnode  ,nnode  ,indx   ,rhs    )
!
!        Calculate new covariances discharges and water levels
!        H-variances at nodes are known after the previous step.
!        These variances are substituted back into the double sweep
!        equations (using coefficients r1, r2, f1, f2, v1 and v2).
!        The obtained covariances (in KABRAN the formal parameters 'ph'
!        and 'pq') are stored in the coefficients matrix RKMAT.
!
      call KABRAN (nbran   ,nnode   ,ngrid   ,&
      &branch  ,rfv1    ,rfv2    ,&
      &rhs     ,rkmat(1,j)       ,rkmat(ngrid+1,j) )

60 continue
!
end
