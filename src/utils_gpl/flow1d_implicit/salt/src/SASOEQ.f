      subroutine sasoeq (nnode  ,nboun  ,nbran  ,ngrid ,juer  ,node ,
     &                   branch ,q2     ,sbdpar ,sbdscr ,rfv1 ,rfv2 ,
     &                   mat    ,rhs    ,indx   ,vv    ,csa2  ,csd2 ,
     &                   ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SASOEQ (SAlt SOlve EQuations)
c
c Module description: Calculates the new salt concentrations for the
c                     current iteration step.
c
c                     The A,B,D,E- etc coefficients are now known and
c                     are applied to calculate a new concentration value
c                     for each gridpoint. First a nodal administration
c                     matrix is created by calling routine SASBCO. This
c                     matrix is solved using a general LU decomposition
c                     routine. The solution of this routine is used to
c                     calculate new concentrations (Cs and C's) in SA-
c                     BRAN.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 branch            P  -
c 17 csa2              P  -
c 18 csd2              P  -
c 15 indx              P  -
c  5 juer              P  -
c 19 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 13 mat               P  -
c  2 nboun             I  Number of boundary nodes.
c  3 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  1 nnode             I  Number of nodes.
c  6 node              P  -
c  8 q2                P  -
c 11 rfv1              P  -
c 12 rfv2              P  -
c 14 rhs               P  -
c  9 sbdpar            P  -
c 10 sbdscr            P  -
c 16 vv                P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c lubksb  LU decomposition; BacKSuBstitution
c ludcmp  LU DeCOMposition
c sabran  SAlt in BRANches
c sasbco  SAlt Substitute Boundary Conditions
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sasoeq.pf,v $
c Revision 1.5  1995/10/18  09:00:29  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/08/23  14:29:44  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:56:15  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:16  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:57  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:05  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:15  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer nnode ,nboun  ,ngrid      ,nbran   ,juer  ,ker
      integer node  (4,nnode)    ,indx  (nnode)  ,branch(4,nbran)
      real    csa2  (ngrid)  ,csd2  (ngrid)  ,
     &        sbdpar(5,nboun)    ,sbdscr(3,nboun)
      double  precision
     &        q2    (ngrid)      ,
     &        mat   (nnode,nnode),rhs   (nnode)  ,vv    (nnode)  ,
     &        rfv1  (ngrid,3)    ,rfv2  (ngrid,3)
c
c     Declaration of local variables
c
      integer kerlu
      real    d
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     Substitute boundary conditions to create a nodal administration
c     matrix MAT and the right hand side vector RHS.
c
      call sasbco (nnode  ,nboun  ,nbran  ,ngrid ,node   ,branch,
     &             q2     ,sbdpar ,sbdscr ,rfv1  ,rfv2   ,mat   ,
     &             rhs    )
CJK   WRITE  (99,*) 'MATRIX'
CJK   do 87  i=1,nnode
CJK       WRITE (99,*) 'R=',i
CJK       WRITE (99,*) (mat(i,j),j=1,nnode)
CJK       WRITE (99,*) 'RL',rhs(i)
CJK87 continue
c
c     Solve the matrix by using LU decomposition. The resulting
c     concentrations are stored in RHS.
c
      call ludcmp (mat    ,nnode  ,nnode  ,indx   ,d      ,vv    ,kerlu)
c
      if (kerlu.ne.0) then
c        Matrix singular
         ker = fatal
         call error (juer ,'SASOEQ' ,esamat ,ker)
         goto 1000
      endif
c
      call lubksb (mat    ,nnode  ,nnode  ,indx   ,rhs    )
c
c     Calculate new salt concentrations Cs and C`s.
c
      call sabran (nbran  ,nnode  ,ngrid  ,branch ,rfv1 ,rfv2   ,
     &             rhs    ,csa2   ,csd2   )
c
 1000 continue
c
      end
