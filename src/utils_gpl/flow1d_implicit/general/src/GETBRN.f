      subroutine getbrn (ibr ,branam, lbrnam)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Kuipers
c
c Module:             GETBRN (GET BRanch Name)
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 ibr               I  branch number
c  2 branam            O  branch name
c  3 lbrnam            O  number of characters of name
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getbr1  GET Branch name
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: getbrn.pf,v $
c Revision 1.1  1999/06/01  13:42:26  kuipe_j
c names in messages substituted + message template
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    ibr , lbrnam
      character*(*)    branam
c
c     Declaration of local variables
c
      integer    branch ,gridnm
c
c     External functions
c
      integer    gtcpnt, gtipnt
      external   gtcpnt, gtipnt
c
c     Include memory pool
c
      include '..\include\mempool.i'

      branch =    gtipnt('BRANCH')
      gridnm =    gtcpnt('GRIDNM')

      call getbr1 (ibr ,ip(branch) ,cp(gridnm), branam, lbrnam)

      end

      subroutine getbr1 (ibr    ,branch ,gridnm, branam, lbrnam)
c
c     Declaration of parameters
c
      integer       ibr    ,lbrnam
      integer       branch (4,*)
      character*40  gridnm(*)
      character*(*) branam
c
c     Declaration of local variables
c
      integer    igr  ,i2 ,i

      igr = branch(3,ibr)
      i2 = 40
      do i=40,1,-1
         i2 = i2 -1
         if (gridnm (igr)(i:i).eq.'_') exit
      enddo
      if (i2 .le. 0) then
         write (branam(1:5),'(i5)') ibr
         lbrnam = 5
      else
         branam (:i2) = gridnm(igr)(:i2)
         lbrnam       = i2
      endif
      branam (lbrnam+1:) = ' '

      end
