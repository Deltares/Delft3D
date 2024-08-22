      subroutine gsdscp (nfrac  ,nbran  ,dis1d  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSDSCP (Graded Sediment DiStribute COpy)
c
c Module description: Total (distributed) transports are stored 
c                     according to declaration DISSED. They are 
c                     copied in the same array in such a manner
c                     that it satisfies the declaration DISGSE.
c                     The arrays
c                     DISSED(4,nbran),
c                     DISGSE(nfrac,2,nbran) and
c                     DIS1D(*)
c                     share the same space.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 nbran             I  Number of branches.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: $
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nfrac   ,nbran
      real       dis1d(*)
c
c     Declaration of local parameters
c
      integer    ibr  
      real       sbegin,seind
      if (nfrac .gt. 2) then
         do 10 ibr=nbran,1,-1
c                    dissed(1,ibr)
            sbegin = dis1d (1+(ibr-1)*4)
c                    dissed(3,ibr)
            seind  = dis1d (3+(ibr-1)*4)
c           disgse(1,ibr)     
            dis1d (1+      (ibr-1)*nfrac*2) = sbegin
c           disgse(1+nfrac,ibr)
            dis1d (1+nfrac+(ibr-1)*nfrac*2) = seind
   10    continue
      else if (nfrac.eq.1) then
         do 20 ibr=1,nbran
c                    dissed(1,ibr)
            sbegin = dis1d (1+(ibr-1)*4)
c                    dissed(3,ibr)
            seind  = dis1d (3+(ibr-1)*4)
c           disgse(1,ibr)     
            dis1d (ibr*2-1) = sbegin
c           disgse(1+nfrac,ibr)
            dis1d (ibr*2)   = seind
   20    continue
      endif

      end
