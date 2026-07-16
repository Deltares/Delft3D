subroutine wqmoin (luwqi1, luwqi2, nexdef, juer, ker)
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         J.Kuipers
!
! Module:             WQMOIN (Water Quality MOdify delwaq INput file)
!
! Module description: The Delwaq input file (extensie .IN0) will be
!                     copied to file .INP and the number of exchanges
!                     will get the actual value.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1 luwqi1             I  Logical unit of input file .INO
! 2 luwqi2             I  Logical unit of output file .INP
! 3 nexdef             I  nr of entries in exdef
! 4 juer               I  Unit number of error file.
! 5 ker                O  Error code
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqmoin.pf,v $
! Revision 1.2  1999/03/15  15:54:04  kuipe_j
! tabs removed
!
! Revision 1.1  1996/11/01  11:25:41  kuipe_j
! Update of Delwaq input file added
!
!
!***********************************************************************
!
!     Parameters
!
   integer    luwqi1 ,luwqi2 ,nexdef ,juer ,ker
!
!     Local variables
!
   integer io   ,io1    ,io2    ,j    ,width
   integer numbers(3)
   character*256 record
   logical hek3 ,transp ,search ,loop ,trailb

   include '..\include\errcod.i'
!
!     Get line width
!
   read (luwqi1,*,iostat=io) width
   if (io.ne.0) goto 9000
   rewind (luwqi1,iostat=io)
   if (io.ne.0) goto 9000
!
   record = ' '
   hek3   = .false.
   transp = .false.
   search = .true.
   loop   = .true.

!  ---------->
10 continue
   if (loop) then
      if (transp) then
!
!          Write number of exchanges
!
         read (luwqi1,*,iostat=io) numbers
         if (io.ne.0) goto 9000
         numbers(1) = nexdef
         write (luwqi2,*,iostat=io)  numbers
         if (io.ne.0) goto 9000
         search=.false.
         transp=.false.
      else

         read (luwqi1,'(a)',iostat=io)  record(1:width)
         if (io.eq.0) then
!
!             Remove trailing blanks
!
            j = width
            trailb = .true.
! - - - - - - - - >
20          continue
            if (trailb) then
               if (record(j:j).ne.' ') then
                  trailb = .false.
               else
                  if (j.gt.1) then
                     j = j-1
                  else
                     trailb = .false.
                     record(1:1) = ' '
                  endif
               endif
               goto 20
            endif
! < - - - - - - - - -
!
            write (luwqi2,'(a)',iostat=io)  record(1:j)
            if (io.ne.0) goto 9000
!
!             Search for text '#3' followed by 'transport' on
!             next record.

            if (search) then
               if (hek3) then
                  if (index(record(1:width),'transport').ne.0) then
                     transp = .true.
                  else
                     transp = .false.
                  endif
               endif
               if (index(record(1:width),'#3').eq.1) then
                  hek3 = .true.
               else
                  hek3 = .false.
               endif
            endif
         else
!             End of file, clear status flag
            io = 0
            loop = .false.
         endif
      endif
!  <------------
      goto 10
   endif

9000 continue
   close (luwqi1,iostat=io1)
   close (luwqi2,iostat=io2)
   if (search .or. io.ne.0 .or. io1.ne.0 .or. io2.ne.0) then
      ker = fatal
      call error (juer,'WQMOIN Cannot modify input file',&
      &ewqmod,ker )
   endif
end


