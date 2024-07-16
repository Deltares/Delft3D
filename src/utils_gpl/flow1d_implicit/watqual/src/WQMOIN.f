      subroutine wqmoin (luwqi1, luwqi2, nexdef, juer, ker)
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         J.Kuipers      
c
c Module:             WQMOIN (Water Quality MOdify delwaq INput file)
c
c Module description: The Delwaq input file (extensie .IN0) will be
c                     copied to file .INP and the number of exchanges
c                     will get the actual value.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1 luwqi1             I  Logical unit of input file .INO
c 2 luwqi2             I  Logical unit of output file .INP
c 3 nexdef             I  nr of entries in exdef
c 4 juer               I  Unit number of error file.
c 5 ker                O  Error code
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
c $Log: wqmoin.pf,v $
c Revision 1.2  1999/03/15  15:54:04  kuipe_j
c tabs removed
c
c Revision 1.1  1996/11/01  11:25:41  kuipe_j
c Update of Delwaq input file added
c
c
c***********************************************************************
c
c     Parameters
c
      integer    luwqi1 ,luwqi2 ,nexdef ,juer ,ker
c
c     Local variables
c
      integer io   ,io1    ,io2    ,j    ,width
      integer numbers(3)
      character*256 record
      logical hek3 ,transp ,search ,loop ,trailb

      include '..\include\errcod.i'
c
c     Get line width
c
      read (luwqi1,*,iostat=io) width
      if (io.ne.0) goto 9000
      rewind (luwqi1,iostat=io)
      if (io.ne.0) goto 9000
c
      record = ' '
      hek3   = .false.
      transp = .false.
      search = .true.
      loop   = .true.

c  ---------->
 10   continue
      if (loop) then
        if (transp) then
c
c          Write number of exchanges
c
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
c
c             Remove trailing blanks 
c
              j = width
              trailb = .true.
c - - - - - - - - >
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
c < - - - - - - - - -
c
              write (luwqi2,'(a)',iostat=io)  record(1:j)
              if (io.ne.0) goto 9000    
c
c             Search for text '#3' followed by 'transport' on
c             next record.

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
c             End of file, clear status flag
              io = 0
              loop = .false.
           endif
        endif
c  <------------
        goto 10
      endif

 9000 continue
      close (luwqi1,iostat=io1)
      close (luwqi2,iostat=io2)
      if (search .or. io.ne.0 .or. io1.ne.0 .or. io2.ne.0) then      
         ker = fatal
         call error (juer,'WQMOIN Cannot modify input file',
     &               ewqmod,ker )
      endif
      end


