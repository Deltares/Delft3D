      subroutine KASOEQ(nnode  ,nnn    ,nbran  ,nosdim ,scnodl ,
     +                  branch ,ngridm ,rfv1   ,
     +                  rfv2   ,calsol ,scnode ,snnode ,sclnod ,nbrnod ,
     +                  brnode ,mat    ,rhs    ,indx   ,ngrid  ,np     ,
     +                  kbeta  ,abcd1  ,kabcd1 ,kabcd2 ,rkmat  ,q      )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KASOEQ (KAlman SOlve EQuations)
c
c Module description: The complete system of equations in C.S=R will be
c                     solved column per column. For each column the next
c                     steps are performed:
c                     -    Calculate right hand side of system of node
c                          equations.
c                     -    Solve and make back substitution in large
c                          matrix.
c
c                     do for each column do
c                     -    The current column of the matrix contains
c                          coefficients E1 and E2. Use these a.o. to
c                          calculate right hand side vector v1 and v2.
c                          (KASWPR)
c                          Build right hand side RHS(NNODE) using BETA,
c                          v1 and v2. (KASBNR)
c                     -    Solve node equations. (LUBKSB)
c                     -    H-variances at nodes are known after the
c                          previous step. These variances are back
c                          substituted in the double sweeped equations
c                          (using coefficients r1, r2, f1, f2, v1 and v2).
c                          The obtained covariances are stored in the
c                          same column of the matrix (KABRAN)
c                     enddo
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 20 abcd1(ngridm,5)   O  (i,1) = A1 or a coefficient for grid point i
c                         (i,2) = B1 or b coefficient for grid point i
c                         (i,3) = C1 or d coefficient for grid point i
c                         (i,4) = D1 or e coefficient for grid point i
c                         (i,5) = E1 or r1 coefficient for grid point i
c  4 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 13 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
c        ,nnode)          contains the number of connected branches
c                         (index 1) for each node. The second index
c                         contains the first connected branch number
c                         etc.
c  8 calsol            I  -
c 16 indx              P  -
c 21 kabcd1            P  -
c 22 kabcd2            P  -
c 19 kbeta             P  -
c 14 mat               P  -
c  3 nbran             I  Number of branches.
c 12 nbrnod            I  Maximum number of connected branches to one
c                         node.
c 17 ngrid             I  Number of grid points in network.
c  5 ngridm            I  Maximum number of gridpoints in a branch.
c  2 nnn               I  Number of uncorrelated random noise processes
c                         for nodal (= boundary) equations.
c  1 nnode             I  Number of nodes.
c 18 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  6 rfv1              P  -
c  7 rfv2              P  -
c 15 rhs(nnode)        IO (i) contains at:
c                               input:  Right-hand-side of the equation
c                                       at node i of the set of node
c                                       equations.
c                               output: Solution in node i.
c 23 rkmat(np,np)      I  Right hand side matrix.
c 11 sclnod(nnn+1)     I  sclnod(i) points to begin of group i of nodes
c                         (in array scnode) with correlated r.n. process
c                         for node equations.
c  9 scnode(scnodl)    I  Node numbers of all uncorrelated r.n. processes
c                         for node equations. The node numbers are grouped
c                         per process.
c 10 snnode(nosdim,nnn)IO Boundary noise for each group.
c                         (1,i) mean
c                         (2,i) deviation (input) or variance
c                         (3,i) indicator for absolute(0) or relative
c                               (1) noise variance
c                         (4,i) time correlation
c                         (5,i) current variance
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c kabran  KAlman in BRANches
c kasbnr  KALman Substitute BouNdary in Right h.s.
c kaswpr  KAlman double SWeeP R.h.side
c lubksb  LU decomposition; BacKSuBstitution
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kasoeq.pf,v $
c Revision 1.4  1999/03/15  15:52:20  kuipe_j
c tabs removed
c
c Revision 1.3  1996/12/05  10:00:06  kuipe_j
c Smoothing kgain,linearization,limit covariance,etc
c
c Revision 1.2  1996/04/12  13:05:28  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:25:01  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer nnode, nbrnod, nnn, ngrid, nbran, ngridm, np, calsol,
     +        scnodl,nosdim
      integer sclnod(nnn+1), scnode(scnodl), indx(nnode)
      integer branch(4,nbran), brnode(nbrnod+1,nnode)
      real    snnode(nosdim,nnn)
      real    rkmat(np,np)
c
      double precision q(ngrid), kabcd1(ngrid,8), kabcd2(ngrid,14)
      double precision rfv1(ngrid,3), rfv2(ngrid,3)
      double precision mat(nnode,nnode), rhs(nnode)
      double precision kbeta(2,nbran), abcd1(ngridm,5)
c
c     Declaration of local variables:
c
      integer j, k, ibr, i, i1, i2, ind, m, nr, n, igp
      logical found
c
c     Add time correlation
c
      do m = 1, nnn
         snnode(5,m) = snnode(4,m)**2 * snnode(5,m) + snnode(2,m)
      enddo
c
      do 60 j = 1, np

         do 10  i=1,nnode
            rhs(i) = 0.D0
   10    continue
c
         do 30 ibr = 1, nbran
c
c           i1 = global grid point number at node n1
c           i2 = global grid point number at node n2
c
            i1 = branch (3,ibr)
            i2 = branch (4,ibr)
c
c           Column j of matrix RKMAT contains coefficients E1 and E2,
c           E1 starts at RKMAT(1,j) and E2 at RKMAT(ngrid+1,j) .
c           E1, E2 are used to calculate right hand side vector
c           v1 (=rfv1(i1,3)) and v2 (=rfv2(i1,3)) .
c
            k = 1
            do 20 i = i1, i2-1
               abcd1(k,1) = dble(rkmat(i,j))
               abcd1(k,2) = dble(rkmat(ngrid+i,j))
               k = k + 1
   20       continue
c
            call KASWPR (i2-i1  ,ngridm ,
     +                   kabcd1(i1,1)   ,kabcd1(i1,2)   ,kabcd1(i1,3)  ,
     +                   kabcd1(i1,4)   ,abcd1(1,1)     ,
     +                   kabcd2(i1,1)   ,kabcd2(i1,2)   ,kabcd2(i1,3)  ,
     +                   kabcd2(i1,4)   ,abcd1(1,2)     ,
     +                   rfv1 (i1,3)    ,
     +                   rfv2 (i1,3)    )
   30    continue
c
c        Build right hand side RHS(NNODE) using BETA, v1 and v2.
c
         call KASBNR (nnode  ,nbran  ,branch ,ngrid  ,kbeta  ,
     +                rfv1(1,3)      ,rfv2(1,3)      ,rhs    )
c
c        Adaption for 'nodal administration equations' required?
c        System noise is allowed only at external nodes.
c
         if (calsol .eq. 2) then
            do 50 m = 1, nnn
               found = .false.
               do 40 ind = sclnod(m), sclnod(m+1)-1
                  n = scnode(ind)
                  if (brnode(1,n) .eq. 1) then
c                    external node
                     ibr = brnode(2,n)
                     if (branch(1,ibr) .eq. n) then
c                       begin of branch
c                       Equation will be stored at H-variance part
c                       of matrix
                        igp = branch(3,ibr)
                     else
c                       end of branch
c                       Equation will be stored at Q-variance part
c                       of matrix
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
c                 external node
                  ibr = brnode(2,n)
                  if (branch(1,ibr) .eq. n) then
c                    begin of branch
                     igp = branch(3,ibr)
                  else
c                    end of branch
                     igp = branch(4,ibr) + ngrid
                  endif
                  rhs(n) = rhs(n) + rkmat(igp,j)
               endif
   55       continue

         endif
c
c        Solve node equations
c
         call LUBKSB (mat    ,nnode  ,nnode  ,indx   ,rhs    )
c
c        Calculate new covariances discharges and water levels
c        H-variances at nodes are known after the previous step.
c        These variances are substituted back into the double sweep
c        equations (using coefficients r1, r2, f1, f2, v1 and v2).
c        The obtained covariances (in KABRAN the formal parameters 'ph'
c        and 'pq') are stored in the coefficients matrix RKMAT.
c
         call KABRAN (nbran   ,nnode   ,ngrid   ,
     +                branch  ,rfv1    ,rfv2    ,
     +                rhs     ,rkmat(1,j)       ,rkmat(ngrid+1,j) )

   60 continue
c
      end
