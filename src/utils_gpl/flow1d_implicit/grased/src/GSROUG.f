      subroutine gsroug (rouopt ,d90   ,hrad  ,dunehe ,dunele ,chezy)
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsroug.F,v $
c Revision 1.2  1995/09/27  10:12:49  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Graded Sediment ROUGhness predictor
c

c
c     Declaration of parameters
c
      integer    rouopt
      real       d90   ,hrad  ,dunehe ,dunele ,chezy
c
c     Declaration of constants
c
c                Roughness option
c                Van Rijn   White, Paris and Bettess
      integer    rouryn    ,rouwpb
      parameter  (rouryn=1 ,rouwpb=2)

      if (rouopt .eq. rouryn) then
         call gsrory (d90   ,hrad  ,dunehe ,dunele ,chezy)
      else if (rouopt .eq. rouwpb) then
c        White, Paris and Bettess
      endif

      end
