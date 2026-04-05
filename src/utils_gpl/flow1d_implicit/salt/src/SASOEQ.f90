subroutine sasoeq (nnode  ,nboun  ,nbran  ,ngrid ,juer  ,node ,&
&branch ,q2     ,sbdpar ,sbdscr ,rfv1 ,rfv2 ,&
&mat    ,rhs    ,indx   ,vv    ,csa2  ,csd2 ,&
&ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SASOEQ (SAlt SOlve EQuations)
!
! Module description: Calculates the new salt concentrations for the
!                     current iteration step.
!
!                     The A,B,D,E- etc coefficients are now known and
!                     are applied to calculate a new concentration value
!                     for each gridpoint. First a nodal administration
!                     matrix is created by calling routine SASBCO. This
!                     matrix is solved using a general LU decomposition
!                     routine. The solution of this routine is used to
!                     calculate new concentrations (Cs and C's) in SA-
!                     BRAN.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 branch            P  -
! 17 csa2              P  -
! 18 csd2              P  -
! 15 indx              P  -
!  5 juer              P  -
! 19 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 13 mat               P  -
!  2 nboun             I  Number of boundary nodes.
!  3 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
!  1 nnode             I  Number of nodes.
!  6 node              P  -
!  8 q2                P  -
! 11 rfv1              P  -
! 12 rfv2              P  -
! 14 rhs               P  -
!  9 sbdpar            P  -
! 10 sbdscr            P  -
! 16 vv                P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! lubksb  LU decomposition; BacKSuBstitution
! ludcmp  LU DeCOMposition
! sabran  SAlt in BRANches
! sasbco  SAlt Substitute Boundary Conditions
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sasoeq.pf,v $
! Revision 1.5  1995/10/18  09:00:29  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/08/23  14:29:44  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:56:15  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:16  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:57  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:05  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:15  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer nnode ,nboun  ,ngrid      ,nbran   ,juer  ,ker
   integer node  (4,nnode)    ,indx  (nnode)  ,branch(4,nbran)
   real    csa2  (ngrid)  ,csd2  (ngrid)  ,&
   &sbdpar(5,nboun)    ,sbdscr(3,nboun)
   double  precision&
   &q2    (ngrid)      ,&
   &mat   (nnode,nnode),rhs   (nnode)  ,vv    (nnode)  ,&
   &rfv1  (ngrid,3)    ,rfv2  (ngrid,3)
!
!     Declaration of local variables
!
   integer kerlu
   real    d
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     Substitute boundary conditions to create a nodal administration
!     matrix MAT and the right hand side vector RHS.
!
   call sasbco (nnode  ,nboun  ,nbran  ,ngrid ,node   ,branch,&
   &q2     ,sbdpar ,sbdscr ,rfv1  ,rfv2   ,mat   ,&
   &rhs    )
!JK   WRITE  (99,*) 'MATRIX'
!JK   do 87  i=1,nnode
!JK       WRITE (99,*) 'R=',i
!JK       WRITE (99,*) (mat(i,j),j=1,nnode)
!JK       WRITE (99,*) 'RL',rhs(i)
!JK87 continue
!
!     Solve the matrix by using LU decomposition. The resulting
!     concentrations are stored in RHS.
!
   call ludcmp (mat    ,nnode  ,nnode  ,indx   ,d      ,vv    ,kerlu)
!
   if (kerlu.ne.0) then
!        Matrix singular
      ker = fatal
      call error (juer ,'SASOEQ' ,esamat ,ker)
      goto 1000
   endif
!
   call lubksb (mat    ,nnode  ,nnode  ,indx   ,rhs    )
!
!     Calculate new salt concentrations Cs and C`s.
!
   call sabran (nbran  ,nnode  ,ngrid  ,branch ,rfv1 ,rfv2   ,&
   &rhs    ,csa2   ,csd2   )
!
1000 continue
!
end
