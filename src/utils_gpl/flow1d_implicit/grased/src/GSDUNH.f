      subroutine gsdunh (initra ,heiopt ,g      ,relden ,kinvis ,dmed  ,
     &                   chezy  ,velo   ,depth  ,frou2  ,duncof ,trforb,
     &                   duncon ,sedexp ,dunehe )

c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsdunh.F,v $
c Revision 1.2  1995/09/27  10:12:23  kuipe_j
c Maintenance
c
c
c***********************************************************************
c     Graded Sediment Calculation of DUNe Height

c
c     Declaration of parameters
c
      integer    heiopt
      real       g      ,relden ,kinvis ,dmed   ,chezy   ,
     &           velo   ,depth  ,frou2  ,sedexp ,dunehe
      real       trforb(*)      ,duncof(*)      ,duncon(*)
      logical    initra
c
c     Declaration of constants
c
c                Dune height option
c                Gill      Van Rijn
      integer    dhgill   ,dhvryn
      parameter (dhgill=1 ,dhvryn=2)

      if (heiopt .eq. dhgill) then
         call gsdhgi (initra  ,g    ,relden ,kinvis ,dmed  ,
     &                chezy   ,velo ,depth  ,frou2  ,duncof(1)      ,
     &                duncof(2)     ,trforb ,duncon(1)     ,sedexp  ,
     &                dunehe  )
      else if (heiopt .eq. dhvryn) then
c        Van Rijn
      endif

      end
