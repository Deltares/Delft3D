      subroutine FLKAPA(istep  ,ngrid  ,nnf    ,nstru  ,nnmu   ,nbran  ,
     &                  c      ,scifri ,pfa    ,strpar ,scimu  ,pmua   ,
     &                  branch ,wfrict ,tauwi  ,pw     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLKAPA (FLow KAlman correction PArameter)
c
c Module description: Correction of bottom friction, contraction
c                     coefficient of general structure and wind friction
c                     due to uncertain correction parameters.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 13 branch            P  -
c  7 c                 P  -
c  1 istep             I  Current time step number (t(n+1)).
c  6 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  3 nnf               I  Number of uncertain bed friction parameters.
c  5 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  4 nstru             I  Number of structures.
c  9 pfa               P  -
c 12 pmua              P  -
c 16 pw                P  -
c  8 scifri            P  -
c 11 scimu             P  -
c 10 strpar            P  -
c 15 tauwi             P  -
c 14 wfrict            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flkac1  FLow KAlman Chezy correction 1
c flkasp  FLow KAlman Structure correction Parameter
c flkawp  FLow KAlman Wind correction Parameter
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flkapa.pf,v $
c Revision 1.3  1999/03/15  15:50:18  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:04:01  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:23:31  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer   istep, ngrid, nnf, nstru, nnmu, nbran
      integer   branch(4,nbran), wfrict(3,nbran)
      integer   scifri(ngrid), scimu(nstru)
      real      pfa(nnf), c(ngrid), strpar(dmstrpar,*),
     &          pmua(nnmu)
      real      pw(1), tauwi(ngrid)
c
      call FLKAC1(ngrid  ,nnf    ,c      ,scifri ,pfa    )
c
      if ( istep .eq. 1 ) then
         call FLKASP(nstru  ,nnmu   ,strpar ,scimu  ,pmua   )
      endif
c
      call FLKAWP(nbran  ,ngrid  ,branch ,wfrict ,tauwi  ,pw     )
c
      end
