      subroutine resdes (modnam ,fd_nefis_res ,grnamd ,kodes ,writim ,
     &                   rtim   ,ncelm  ,ncelh  ,neferr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Fileio module (interface to nefis)
c
c Programmer:         J.Kuipers
c
c Module:             RESDES (RESults; DEScription group is defined)
c
c Module description: Definition of description group for results of a
c                     module.
c
c                     Initial call in a run: If the description group is
c                     not present on file it will be made. If it is
c                     present the last cell numbers of map and history
c                     block will be read.
c
c                     Next calls: The current cell number(s) will be
c                     updated on file.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 dafdrs            P  -
c  2 defdrs            P  -
c  4 grnamd            I  Name of data group for description block.
c  5 kodes             I  Control code of routine resdes:
c                         0 =     Initial call; define description block
c                         1 =     Next calls; write map cell number
c                         2 =     Next calls; write history cell number
c                         3 =     Next calls; write map and history cell
c                                 numbers
c  1 modnam            I  Module name.
c  9 ncelh             IO Actual cell number of a history block of re-
c                         sult file.
c  8 ncelm             IO Actual cell number of a map block of result
c                         file.
c 10 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  7 rtim(nrtim)       I  Parameter list for a HIST block with results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nrtim)  = Report parameter n sub code
c                         written immediately.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c credat  CREation of a data group in the DATa file
c defcel  DEFinition of a CELl
c defelm  DEFinition of an ELeMent
c defgrp  DEFinition of a GRouP
c getiel  GET Integer ELement from nefis file
c inqdat  INQuire for info of DATa group on data file
c putiel  PUT Integer ELement to nefis file
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
c $Log: resdes.pf,v $
c Revision 1.3  1995/10/18  08:59:08  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  06:57:16  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:15  hoeks_a
c Initial check-in
c
c Revision 1.3  1995/03/08  09:05:40  kuipe_j
c The number of report time steps are set initialy for both
c maps and histories.
c
c Revision 1.2  1993/11/26  15:30:07  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:43  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       kodes     ,ncelm     ,ncelh   ,neferr
      integer       fd_nefis_res ,rtim(*)
      character*(*) grnamd    ,modnam
      logical       writim
c
c     Declaration of local variables
c
      integer       error     ,i
      integer       dimn(1)   ,uindex(3) ,ord(1) ,buf(1)
      character*16  celnam
      character*16  namdes(4) ,names(4)
      character*64  desdes(4)
c
c     Declaration of external functions
c
      integer   defelm ,defcel ,defgrp ,credat ,inqdat ,putiel ,getiel
      external  defelm ,defcel ,defgrp ,credat ,inqdat ,putiel ,getiel
c
      data      uindex ,ord /1,1,1,1/
c
c     Definition of elements.
c
      data (desdes(i) ,namdes(i) ,i=1,4) /
c
     &     'No of time steps in MAP block (N1)' , 'NO_TIMES_MAP'  ,
     &     'No of time steps in HIST block (N2)', 'NO_TIMES_HIST' ,
     &     'Number of places in HIST block (N3)', 'NO_PLACE_HIST' ,
     &     'Grid point numbers in HIST block'   , 'PLACE_GRID'    /
c
      neferr = 0
      if (kodes.eq.0) then
c
c        Definition of Block Descriptor
c
         error  = inqdat (fd_nefis_res ,grnamd ,grnamd)
c
         if (error .eq. 6004) then
c
c           Data group for description doesn't exist, so
c           define elements, cel and group def.
c           Apparently a new output file starts.
c
            dimn(1) = 0
            do 10 i = 1,3
               error = defelm (fd_nefis_res ,namdes(i) ,'INTEGER' ,4  ,
     &                         'n'        ,'-'       ,desdes(i) ,0  ,
     &                         dimn       )
c
c              Remark: The elements may be defined by another module.
c
               if (error .ne. 0 .and. error .ne. 5007) goto 1000
               names(i) = namdes(i)
   10       continue
c
            dimn(1)  = rtim(1)
            names(4) = modnam//'_'//namdes(4)
            error    = defelm (fd_nefis_res,names(4) ,'INTEGER' ,4,
     &                         'n'        ,'-'   ,desdes(4) ,1  ,
     &                         dimn       )
c
c           Derive cell name from group name.
c
            i      = index (grnamd,'GROUP') - 1
            celnam = grnamd(1:i)//'CEL'
c
            error  = defcel (fd_nefis_res, celnam ,4 ,names)
            if (error.ne.0) goto 1000
c
            dimn(1) = 0
            error = defgrp(fd_nefis_res, grnamd, celnam, 0 ,dimn ,ord )
            if (error.ne.0) goto 1000
c
c           Create data group on data file.
c
            error = credat (fd_nefis_res, grnamd, grnamd)
            if (error.ne.0) goto 1000
c
c           Write Block Descriptor
c
            ncelm  = 0
            ncelh  = 0
            buf(1) = ncelm
            error  = putiel(fd_nefis_res ,grnamd ,namdes(1) ,
     &                       uindex  ,ord    ,buf    )
            if (error.ne.0) goto 1000

            buf(1) = ncelh
            error  = putiel(fd_nefis_res ,grnamd ,namdes(2) ,
     &                       uindex  ,ord    ,buf    )
            if (error.ne.0) goto 1000

            buf(1) = rtim(1)
            error  = putiel(fd_nefis_res ,grnamd ,namdes(3) ,
     &                       uindex  ,ord    ,buf    )
            if (error.ne.0) goto 1000
c
            if (rtim(1).gt.0) then
               error  = putiel(fd_nefis_res ,grnamd ,names(4) ,
     &                          uindex  ,ord    ,rtim(2))
               if (error.ne.0) goto 1000
            endif
c
            neferr = 2
c
         else
c
c           Data group already exists, so read last cel numbers.
c           Apparently an existing output file will be extended.
c
            error = getiel(fd_nefis_res ,grnamd ,namdes(1) ,
     &                      uindex  ,ord    ,4      ,buf       )
            ncelm = buf(1)
            if (error.ne.0) goto 1000
c
            error = getiel(fd_nefis_res ,grnamd ,namdes(2) ,
     &                      uindex  ,ord    ,4      ,buf       )
            if (error.ne.0) goto 1000
            ncelh = buf(1)
c
         endif
      endif
c
c     If writim = .false. the time step counter must be updated at the
c     last time step.
c
c     Check has been switched off because of the following reason:
c
c     - If an error occurs the counters are never written to file.
c
c     if (writim) then
         if (kodes.ne.2 .and. ncelm .ge. 0) then
c
c           Update Map time step counter on file.
c
            buf(1) = ncelm
            error  = putiel(fd_nefis_res ,grnamd ,namdes(1) ,
     &                       uindex  ,ord    ,buf    )
            if (error.ne.0) goto 1000
         endif
         if (kodes.ne.1 .and. ncelh .ge. 0) then
c
c           Update History time step counter on file.
c
            buf(1) = ncelh
            error  = putiel(fd_nefis_res ,grnamd ,namdes(2) ,
     &                       uindex  ,ord    ,buf    )
            if (error.ne.0) goto 1000
         endif
c     endif
c
      return
c
 1000 continue
      neferr = error
c
      end
