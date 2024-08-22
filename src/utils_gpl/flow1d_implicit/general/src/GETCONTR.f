      subroutine getcontr (icon ,cntrnm, lcntrnm)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Kuipers
c
c Module:             GETCONTR (GET CONTRoller name)
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 icon              I  controller number
c  2 cntrnm            O  controller name
c  3 lcntrnm           O  number of characters of name
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME       DESCRIPTION
c getcontr1  GET CONTRoller name
c=======================================================================
c
 
c
c     Declaration of parameters
c
      integer    icon , lcntrnm
      character*(*)     cntrnm
c
c     Declaration of local variables
c
      integer    contrnam
c
c     External functions
c
      integer    gtcpnt
      external   gtcpnt
c
c     Include memory pool
c
      include '..\include\mempool.i'

      contrnam =    gtcpnt('CONTRNAM')

      call getcontr1 (icon, cp(contrnam), cntrnm, lcntrnm)

      end

      subroutine getcontr1 (icon ,contrnam, cntrnm, lcntrnm)
c
c     Declaration of parameters
c
      integer       icon   ,lcntrnm
      character*40  contrnam(*)
      character*(*) cntrnm
c
c     Declaration of local variables
c
      integer    l  ,i2,  i

      l  = len (contrnam(icon))
      i2 = l
      do i=l,1,-1
         if (contrnam(icon)(i:i).ne.' ') exit
         i2 = i2-1
      enddo

      if (i2 .le. 0) then
         write (cntrnm(1:2),'(i2)') icon
         lcntrnm = 2
      else
         cntrnm (:i2)  = contrnam(icon)(:i2)
         lcntrnm       = i2
      endif
      cntrnm (lcntrnm+1:) = ' '

      end
