SUBROUTINE BICGST( N, B, X, WORK, LDW, ITER, RESID, MATVC1,&
&MATVC2, PSOLVE, MAT ,INFO ,nodnod ,numnod)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         Kian Tan
!
! Module:             BICGST (Iterative template routine BICGSTAB)
!
! Module description: Iterative template routine , original name
!                     BICGSTAB. Univ. of Tennessee and Oak Ridge
!                     National Laboratory October 1, 1993.
!
!-----------------------------------------------------------------------
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: bicgst.pf,v $
! Revision 1.12  1999/03/15  15:51:11  kuipe_j
! tabs removed
!
! Revision 1.11  1998/06/24  11:10:26  kuipe_j
! Try direct solver if BICGST fails
!
! Revision 1.10  1998/06/08  14:42:03  kuipe_j
! comment leader
!
! Revision 1.8  1997/08/26  15:05:33  kuipe_j
! Restart removed
!
! Revision 1.7  1997/08/08  10:49:38  kuipe_j
! Display residuals after error
!
! Revision 1.6  1997/05/26  07:45:41  kuipe_j
! Small changes
!
! Revision 1.5  1996/05/30  09:57:15  kuipe_j
! messages
!
! Revision 1.4  1996/05/28  13:36:17  kuipe_j
! Linearization =  Solution
!
! Revision 1.3  1995/10/18  08:59:30  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/09/22  10:02:41  kuipe_j
! variable dimensions, new headers
!
!
!
!***********************************************************************
!
!  -- Iterative template routine , original name BICGSTAB --
!     Univ. of Tennessee and Oak Ridge National Laboratory
!     October 1, 1993
!     Details of this algorithm are described in "Templates for the
!     Solution of Linear Systems: Building Blocks for Iterative
!     Methods", Barrett, Berry, Chan, Demmel, Donato, Dongarra,
!     Eijkhout, Pozo, Romine, and van der Vorst, SIAM Publications,
!     1993. (ftp netlib2.cs.utk.edu; cd linalg; get templates.ps).
!
!     .. Scalar Arguments ..
   INTEGER            N, LDW, ITER, INFO
   DOUBLE PRECISION   RESID
!     ..
!     .. Array Arguments ..
   DOUBLE PRECISION   X( * ), B( * ), WORK( LDW,* ), MAT(LDW,LDW)
   integer            nodnod(ldw,*), numnod(ldw)
!     ..
!     .. Function Arguments ..
   EXTERNAL           MATVC1, MATVC2, PSOLVE
!     ..
!
!  Purpose
!  =======
!
!  BICGSTAB solves the linear system A*x = b using the
!  BiConjugate Gradient Stabilized iterative method with
!  preconditioning.
!
!  Convergence test: ( norm( b - A*x ) / norm( b ) ) < TOL.
!  For other measures, see the above reference.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER.
!          On entry, the dimension of the matrix.
!          Unchanged on exit.
!
!  B       (input) DOUBLE PRECISION array, dimension N.
!          On entry, right hand side vector B.
!          Unchanged on exit.
!
!  X       (input/output) DOUBLE PRECISION array, dimension N.
!          On input, the initial guess. This is commonly set to
!          the zero vector.
!          On exit, if INFO = 0, the iterated approximate solution.
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (LDW,7)
!          Workspace for residual, direction vector, etc.
!          Note that vectors R and S shared the same workspace.
!
!  LDW     (input) INTEGER
!          The leading dimension of the array WORK. LDW >= max(1,N).
!
!  ITER    (input/output) INTEGER
!          On input, the maximum iterations to be performed.
!          On output, actual number of iterations performed.
!
!  RESID   (input/output) DOUBLE PRECISION
!          On input, the allowable convergence measure for
!          norm( b - A*x ) / norm( b ).
!          On output, the final value of this measure.
!
!  MAT     (input) DOUBLE PRECISION
!          Coefficient matrix used to calculate delh(i) from
!          MAT * delh = b
!
!  MATVEC  (external subroutine)
!          The user must provide a subroutine to perform the
!          matrix-vector product
!
!               y := alpha*A*x + beta*y,
!
!          where alpha and beta are scalars, x and y are vectors,
!          and A is a matrix. Vector x must remain unchanged.
!          The solution is over-written on vector y.
!
!          The call is:
!
!             CALL MATVEC( ALPHA, X, BETA, Y )
!
!          The matrix is passed into the routine in a common block.
!
!  PSOLVE  (external subroutine)
!          The user must provide a subroutine to perform the
!          preconditioner solve routine for the linear system
!
!               M*x = b,
!
!          where x and b are vectors, and M a matrix. Vector b must
!          remain unchanged.
!          The solution is over-written on vector b.
!
!          The call is:
!
!             CALL PSOLVE( X, B )
!
!          The preconditioner is passed into the routine in a common block.
!
!  INFO    (output) INTEGER
!
!          =  0: Successful exit. Iterated approximate solution returned.
!
!          >  0: Convergence to tolerance not achieved. This will be
!                set to the number of iterations performed.
!
!          <  0: Illegal input parameter, or breakdown occurred
!                during iteration.
!
!                Illegal parameter:
!
!                   -1: matrix dimension N < 0
!                   -2: LDW < N
!                   -3: Maximum number of iterations ITER <= 0.
!
!                BREAKDOWN: If parameters RHO or OMEGA become smaller
!                   than some tolerance, the program will terminate.
!                   Here we check against tolerance BREAKTOL.
!
!                  -10: RHO < BREAKTOL: RHO and RTLD have become
!                                       orthogonal.
!                  -11: OMEGA < BREAKTOL: S and T have become
!                                         orthogonal relative to T'*T.
!
!                  BREAKTOL is set in function GETBRK.
!
!  NODNOD  INTEGER array, dimension (LDW,nbrnod+1)
!          Indicates the connection between nodes in the network
!
!  NUMNOD  INTEGER array, dimension (LDW)
!          Indicates the maximum number of nodes connected with node i
!
!  BLAS CALLS: DAXPY, DCOPY, DDOT, DNRM2, DSCAL
!  ==============================================================
!
!     .. Parameters ..
   DOUBLE PRECISION   ZERO, ONE
   PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )
   PARAMETER          BREAKTOL=1.0D-32
!     ..
!     .. Local Scalars ..
   INTEGER            R, RTLD, P, PHAT, V, S, SHAT, T, MAXIT
   DOUBLE PRECISION   TOL, ALPHA, BETA, RHO, RHO1, BNRM2, OMEGA,&
   &RHOTOL, OMEGATOL, GETBRK, DDOT, DNRM2,&
   &RHOTLE
   integer            ir,ir1,ir2,irmax,rep
   parameter          (irmax=20,rep=10000)
   double precision   resida(irmax)
!     ..
!     .. External Functions ..
   EXTERNAL           DAXPY, DCOPY, DDOT, DNRM2, DSCAL
!     ..
!     .. Intrinsic Functions ..
   INTRINSIC          ABS, MAX
!     ..
!     .. Executable Statements ..
!
   INFO = 0
!
!     Test the input parameters.
!
   IF ( N.LT.0 ) THEN
      INFO = -1
   ELSE IF ( LDW.LT.MAX( 1, N ) ) THEN
      INFO = -2
   ELSE IF ( ITER.LE.0 ) THEN
      INFO = -3
   ENDIF
   IF ( INFO.NE.0 ) RETURN
!
   MAXIT = ITER
   TOL   = RESID
!
!     Alias workspace columns.
!
   R    = 1
   RTLD = 2
   P    = 3
   V    = 4
   T    = 5
   PHAT = 6
   SHAT = 7
   S    = 1
!
!     Set parameter tolerances.
!
!     Both `RHOTOL` and `OMEGATOL` used to be set
!     by GETBRK(). This caused IFX in release to fail.
!     See UNST-8316
   RHOTOL = BREAKTOL
   RHOTLE = SQRT(RHOTOL)*2.D0
   OMEGATOL = BREAKTOL
!jk
!jk   After 15 iterations the iteration will be started again
   ITER = 0
5  continue
!
!     Set initial residual.
!
   CALL DCOPY( N, B, 1, WORK(1,R), 1 )
!     since we know initial guess is always not equal zero we can
!     skip this test
!     Perturbation to avoid breakdown  26-4-96 J.K/ K.T
!
   X(1) = X(1) +  RHOTLE
! KT  IF ( DNRM2( N, X, 1 ).NE.ZERO ) THEN
   CALL MATVC2( -ONE, X, ONE, WORK(1,R),mat,ldw ,nodnod ,numnod)

!        at least one BiCGSTAB iteration!
! KT     IF ( DNRM2( N, WORK(1,R), 1 ).LE.TOL ) GO TO 30
! KT  ENDIF
   CALL DCOPY( N, WORK(1,R), 1, WORK(1,RTLD), 1 )
!
   BNRM2 = DNRM2( N, B, 1 )
!JK   IF ( BNRM2 .EQ. ZERO ) BNRM2 = ONE
   IF ( abs(BNRM2) .lt. RHOTOL ) BNRM2 = ONE
!
!JK   ITER = 0
!
10 CONTINUE
!
!     Perform BiConjugate Gradient Stabilized iteration.
!
   RHO = DDOT( N, WORK(1,RTLD), 1, WORK(1,R), 1 )
   IF ( ABS( RHO ).LT.RHOTOL ) GO TO 25
!
   ITER = ITER + 1
!
!        Compute vector P.
!
!jk      IF ( ITER.GT.1 ) THEN
   IF ( ITER.GT.1 .and. mod(iter,rep).ne.0) THEN
      BETA = ( RHO / RHO1 ) * ( ALPHA / OMEGA )
      CALL DAXPY( N, -OMEGA, WORK(1,V), 1, WORK(1,P), 1 )
      CALL DSCAL( N, BETA, WORK(1,P), 1 )
      CALL DAXPY( N, ONE, WORK(1,R), 1, WORK(1,P), 1 )
   ELSE
      CALL DCOPY( N, WORK(1,R), 1, WORK(1,P), 1  )
   ENDIF
!
!        Compute direction adjusting vector PHAT and scalar ALPHA.
!
!         CALL PSOLVE( WORK(1,PHAT), WORK(1,P),mat,ldw )
   CALL PSOLVE( WORK(1,PHAT), WORK(1,P),mat,ldw,nodnod,numnod )
   CALL MATVC1( ONE, WORK(1,PHAT), ZERO, WORK(1,V),mat,ldw ,&
   &nodnod, numnod )
   ALPHA = RHO / DDOT( N, WORK(1,RTLD), 1, WORK(1,V), 1 )
!
!        Early check for tolerance.
!
   CALL DAXPY( N, -ALPHA, WORK(1,V), 1, WORK(1,R), 1 )
   CALL DCOPY( N, WORK(1,R), 1, WORK(1,S), 1 )
   IF ( DNRM2( N, WORK(1,S), 1 ).LE.TOL ) THEN
      CALL DAXPY( N, ALPHA, WORK(1,PHAT), 1, X, 1 )
      RESID = DNRM2( N, WORK(1,S), 1 ) / BNRM2
      GO TO 30
   ELSE
!
!           Compute stabilizer vector SHAT and scalar OMEGA.
!
!            CALL PSOLVE( WORK(1,SHAT), WORK(1,S),mat,ldw )
      CALL PSOLVE( WORK(1,SHAT), WORK(1,S),mat,ldw,nodnod,numnod )
      CALL MATVC1( ONE, WORK(1,SHAT), ZERO, WORK(1,T),mat,ldw ,&
      &nodnod, numnod )
      OMEGA = DDOT( N, WORK(1,T), 1, WORK(1,S), 1 ) /&
      &DDOT( N, WORK(1,T), 1, WORK(1,T), 1 )
!
!           Compute new solution approximation vector X.
!
      CALL DAXPY( N, ALPHA, WORK(1,PHAT), 1, X, 1 )
      CALL DAXPY( N, OMEGA, WORK(1,SHAT), 1, X, 1 )
!
!           Compute residual R, check for tolerance.
!
      CALL DAXPY( N, -OMEGA, WORK(1,T), 1, WORK(1,R), 1 )
      RESID = DNRM2( N, WORK(1,R), 1 ) / BNRM2
      resida(mod(iter,irmax)+1)=resid
      IF ( RESID.LE.TOL  ) GO TO 30
      IF ( ITER.EQ.MAXIT ) GO TO 20
   ENDIF
!
   IF ( ABS( OMEGA ).LT.OMEGATOL ) THEN
      GO TO 25
   ELSE
      RHO1 = RHO
!JK      write (11,*) 'Ib=',iter,'RESID=',resid,'IP=',rho
!jk      Restart iteration at every xxth step
      if (mod(iter,rep).eq.0) goto 5
      GO TO 10
   ENDIF
!
20 CONTINUE
!
!     Iteration fails.
!
   INFO = 1
   write(11,*) 'BICGST  fout1:  INFO= ',info,' ITER= ',iter
   write(11,*) 'BICGST  is not convergating'
   write(11,*) 'The last residuals are:'
21 continue
   ir  = mod(iter,irmax)
   ir2 = max(0,iter-irmax)
   if (iter.ge.irmax) then
      do 22  ir1 = ir+2,irmax
         ir2 = ir2+1
         write (11,*) ir2,':',resida(ir1)
22    continue
      do 23  ir1 = 1,ir+1
         ir2 = ir2+1
         write (11,*) ir2,':',resida(ir1)
23    continue
   else
      do 24  ir1 = 2,ir+1
         ir2 = ir2+1
         write (11,*) ir2,':',resida(ir1)
24    continue
   endif
   write (11,*) 'Try direct solver now'
   goto 30
!
25 CONTINUE
!
!     Set breakdown flag.
!
   IF ( ABS( RHO ).LT.RHOTOL ) THEN
      INFO = -10
   ELSE IF ( ABS( OMEGA ).LT.OMEGATOL ) THEN
      INFO = -11
   ENDIF
   write(11,*) 'BICGST  fout2:  INFO= ',info,' ITER= ',iter
   write(11,*) 'BICGST  breakdown in GMRES part'
   goto 21
!
30 CONTINUE
!
!     Iteration successful; return.
!
   resid = tol
   iter  = maxit
   RETURN
!
!     End of BICGST
!
END
