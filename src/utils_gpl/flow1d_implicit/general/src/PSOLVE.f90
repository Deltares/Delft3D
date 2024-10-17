SUBROUTINE PSOLVE (x, b, mat, nnode, nodnod, numnod )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         Kian Tan
!
! Module:             PSOLVE (Precondition SOLVEr)
!
! Module description: Precondition Solve routine.
!
!-----------------------------------------------------------------------
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: psolve.pf,v $
! Revision 1.4  1999/03/15  15:51:23  kuipe_j
! tabs removed
!
! Revision 1.3  1995/10/18  08:59:45  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/09/22  10:03:09  kuipe_j
! variable dimensions, new headers
!
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer nnode
   integer nodnod(nnode,*), numnod(nnode)
   double precision x(*), b(*), mat(nnode,nnode)
!
!     Declaration of local variables:
!
   integer i, j, k
   double precision hulp
!
!     KHT: 08 09 95 this is just a dcopy in case of an
!                   explicitly left preconditioned system
!      do 10 i=1,nnode
!          x(i) = b(i)
!   10 continue


!     KHT: 08 09 95 but we can do better, so we use the
!                   GS-2 preconditioner developed for DELWAQ.
!                   for those interested: write A = L+I+U then
!                   our preconditioner is M = (I+U)*(L+I)

!--   (L+I) x^\ast = b --c
   do 30 i=1,nnode
      hulp = b(i)
      do 20 j = 1,numnod(i)
         k = nodnod(i,j)
         if (k .lt. i) hulp = hulp - mat(i,k)*x(k)
20    continue
      x(i) = hulp
30 continue

!--   (I+U) x^(n+1) = x^\ast --c
   do 50 i=nnode,1,-1
      hulp = x(i)
      do 40 j = 1,numnod(i)
         k = nodnod(i,j)
         if (k .gt. i) hulp = hulp - mat(i,k)*x(k)
40    continue
      x(i) = hulp
50 continue

   RETURN
!
END
