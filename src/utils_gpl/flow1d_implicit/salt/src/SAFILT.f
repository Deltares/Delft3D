      subroutine safilt (nbran  ,ngrid ,branch ,  
     &                   grid   ,x      ,af    ,disgr  ,
     &                   filc   ,csd2  ,csa2   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SAFILT (SAlt FILTer)
c
c Module description: Filter concentration field in case negative values
c                     occur.
c
c                     This subroutine tests if negative concentration
c                     values occur in the calculated concentrations
c                     field. If this is the case subroutine SAFORF is
c                     called to ensure a positive
c                     concentration on each gridpoint on the network.
c                     If the concentrations are adapted c's (A*D*dc/dx)
c                     will be recalculated on adapted grid points.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c    af(ngrid)         I  Flow area at every grid point at time t(n+1)
c    branch            P  -
c    csa2              P  -
c    csd2(ngrid)       IO Diffusion (c s) in every grid point at time
c                         t(n+1).
c    disgr(ngrid)      I  Dispersion coefficient in every grid point at
c                         time t(n+1).
c    grid              P  -
c
c    nbran             I  Number of branches.
c    ngrid             I  Number of grid points in network.
c    x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c sadcdx  SAlt DC/DX calculation
c saforf  SAlt FORester Filter
c satneg  SAlt Test NEGative concentrations
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
c $Log: safilt.pf,v $
c Revision 1.7  1999/03/15  15:53:22  kuipe_j
c tabs removed
c
c Revision 1.6  1995/10/18  09:00:21  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.5  1995/09/22  10:03:17  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/08/30  12:37:17  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:56:09  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:02  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:44  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:33:42  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:13  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nbran  ,ngrid   
      integer    branch(4,nbran) 
      integer    grid  (ngrid)
      real       x     (ngrid)   ,af    (ngrid)  ,disgr(ngrid),
     &           filc  (ngrid)   ,csd2  (ngrid)  ,csa2 (ngrid)
c
c     Declaration of local variables
c
      integer    igr   
      real       ceps
      logical    filter
c
      ceps   = -1.e-7
c
      call satneg (ngrid ,ceps ,csa2 ,filter)
c
      if (filter) then
c
c        Eenvoudige beveiliging tegen negatieve concentraties
c
         call saforf (nbran ,ngrid ,ceps ,branch ,csa2  ,filc)

c        Calculate A*D*dc/dx when the concentrations are adapted.
c        Calculate dc/dx first.
c                                                                 dc/dx
         call sadcfi (nbran  ,ngrid  ,branch ,grid   ,csa2   ,x  ,csd2,
     &                filc   )     
c
         do igr = 1,ngrid

            if (filc(igr).gt.0.) then 
c                                             dc/dx
               csd2(igr) = af(igr)*disgr(igr)*csd2(igr)
            endif
         enddo
      endif
c
      end
