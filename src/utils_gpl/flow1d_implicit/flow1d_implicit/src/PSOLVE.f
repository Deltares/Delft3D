      SUBROUTINE PSOLVE (x, b, mat, nnode, nodnod, numnod )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         Kian Tan 
c
c Module:             PSOLVE (Precondition SOLVEr)
c
c Module description: Precondition Solve routine.
c
c-----------------------------------------------------------------------
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: psolve.pf,v $
c Revision 1.4  1999/03/15  15:51:23  kuipe_j
c tabs removed
c
c Revision 1.3  1995/10/18  08:59:45  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/09/22  10:03:09  kuipe_j
c variable dimensions, new headers
c
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      integer nnode
      integer nodnod(nnode,*), numnod(nnode)
      double precision x(*), b(*), mat(nnode,nnode)
c
c     Declaration of local variables:
c
      integer i, j, k
      double precision hulp
c
c     KHT: 08 09 95 this is just a dcopy in case of an
c                   explicitly left preconditioned system
c      do 10 i=1,nnode
c          x(i) = b(i)
c   10 continue


c     KHT: 08 09 95 but we can do better, so we use the
c                   GS-2 preconditioner developed for DELWAQ.
c                   for those interested: write A = L+I+U then
c                   our preconditioner is M = (I+U)*(L+I)

c--   (L+I) x^\ast = b --c
      do 30 i=1,nnode
         hulp = b(i)
         do 20 j = 1,numnod(i)
            k = nodnod(i,j)
            if (k .lt. i) hulp = hulp - mat(i,k)*x(k)
   20    continue
         x(i) = hulp
   30 continue

c--   (I+U) x^(n+1) = x^\ast --c
      do 50 i=nnode,1,-1
         hulp = x(i)
         do 40 j = 1,numnod(i)
            k = nodnod(i,j)
            if (k .gt. i) hulp = hulp - mat(i,k)*x(k)
   40    continue
         x(i) = hulp
   50 continue

      RETURN
*
      END
