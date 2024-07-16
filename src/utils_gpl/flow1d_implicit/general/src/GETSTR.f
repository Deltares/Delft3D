      subroutine getstr (istr ,strnam, lstnam)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Kuipers
c
c Module:             GETSTR (GET STRucture name)
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 istr              I  structure number
c  2 strnam            O  structure name
c  3 lstnam            O  number of characters of name
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getst1  GET STructure name
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: getstr.pf,v $
c Revision 1.1  1999/06/01  13:42:27  kuipe_j
c names in messages substituted + message template
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    istr , lstnam
      character*(*)     strnam
c
c     Declaration of local variables
c
      integer    strunm
c
c     External functions
c
      integer    gtcpnt
      external   gtcpnt
c
c     Include memory pool
c
      include '..\include\mempool.i'

      strunm =    gtcpnt('STRUNM')

      call getst1 (istr ,cp(strunm), strnam, lstnam)

      end

      subroutine getst1 (istr ,strunm, strnam, lstnam)
c
c     Declaration of parameters
c
      integer       istr   ,lstnam
      character*40  strunm(*)
      character*(*) strnam
c
c     Declaration of local variables
c
      integer    l  ,i2,  i

      l  = len (strunm(istr))
      i2 = l
      do i=l,1,-1
         if (strunm(istr)(i:i).ne.' ') exit
         i2 = i2-1
      enddo

      if (i2 .le. 0) then
         write (strnam(1:2),'(i2)') istr
         lstnam = 2
      else
         strnam (:i2) = strunm(istr)(:i2)
         lstnam       = i2
      endif
      strnam (lstnam+1:) = ' '

      end
