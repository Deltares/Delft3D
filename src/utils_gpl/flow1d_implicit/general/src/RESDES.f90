subroutine resdes (modnam ,fd_nefis_res ,grnamd ,kodes ,writim ,&
&rtim   ,ncelm  ,ncelh  ,neferr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Fileio module (interface to nefis)
!
! Programmer:         J.Kuipers
!
! Module:             RESDES (RESults; DEScription group is defined)
!
! Module description: Definition of description group for results of a
!                     module.
!
!                     Initial call in a run: If the description group is
!                     not present on file it will be made. If it is
!                     present the last cell numbers of map and history
!                     block will be read.
!
!                     Next calls: The current cell number(s) will be
!                     updated on file.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 dafdrs            P  -
!  2 defdrs            P  -
!  4 grnamd            I  Name of data group for description block.
!  5 kodes             I  Control code of routine resdes:
!                         0 =     Initial call; define description block
!                         1 =     Next calls; write map cell number
!                         2 =     Next calls; write history cell number
!                         3 =     Next calls; write map and history cell
!                                 numbers
!  1 modnam            I  Module name.
!  9 ncelh             IO Actual cell number of a history block of re-
!                         sult file.
!  8 ncelm             IO Actual cell number of a map block of result
!                         file.
! 10 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  7 rtim(nrtim)       I  Parameter list for a HIST block with results:
!                         (1)      = Number of places
!                         (2)      = Report grid point 1
!                         (3)      = Report grid point 2
!                         (i)      = Report grid point n
!                         (i+1)    = Report parameter code 1
!                         (i+2)    = Report parameter sub code 1
!                         (nrtim)  = Report parameter n sub code
!                         written immediately.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! credat  CREation of a data group in the DATa file
! defcel  DEFinition of a CELl
! defelm  DEFinition of an ELeMent
! defgrp  DEFinition of a GRouP
! getiel  GET Integer ELement from nefis file
! inqdat  INQuire for info of DATa group on data file
! putiel  PUT Integer ELement to nefis file
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
! $Log: resdes.pf,v $
! Revision 1.3  1995/10/18  08:59:08  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  06:57:16  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:15  hoeks_a
! Initial check-in
!
! Revision 1.3  1995/03/08  09:05:40  kuipe_j
! The number of report time steps are set initialy for both
! maps and histories.
!
! Revision 1.2  1993/11/26  15:30:07  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:43  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       kodes     ,ncelm     ,ncelh   ,neferr
   integer       fd_nefis_res ,rtim(*)
   character*(*) grnamd    ,modnam
   logical       writim
!
!     Declaration of local variables
!
   integer       error     ,i
   integer       dimn(1)   ,uindex(3) ,ord(1) ,buf(1)
   character*16  celnam
   character*16  namdes(4) ,names(4)
   character*64  desdes(4)
!
!     Declaration of external functions
!
   integer   defelm ,defcel ,defgrp ,credat ,inqdat ,putiel ,getiel
   external  defelm ,defcel ,defgrp ,credat ,inqdat ,putiel ,getiel
!
   data      uindex ,ord /1,1,1,1/
!
!     Definition of elements.
!
   data (desdes(i) ,namdes(i) ,i=1,4) /&
!
   &'No of time steps in MAP block (N1)' , 'NO_TIMES_MAP'  ,&
   &'No of time steps in HIST block (N2)', 'NO_TIMES_HIST' ,&
   &'Number of places in HIST block (N3)', 'NO_PLACE_HIST' ,&
   &'Grid point numbers in HIST block'   , 'PLACE_GRID'    /
!
   neferr = 0
   if (kodes.eq.0) then
!
!        Definition of Block Descriptor
!
      error  = inqdat (fd_nefis_res ,grnamd ,grnamd)
!
      if (error .eq. 6004) then
!
!           Data group for description doesn't exist, so
!           define elements, cel and group def.
!           Apparently a new output file starts.
!
         dimn(1) = 0
         do 10 i = 1,3
            error = defelm (fd_nefis_res ,namdes(i) ,'INTEGER' ,4  ,&
            &'n'        ,'-'       ,desdes(i) ,0  ,&
            &dimn       )
!
!              Remark: The elements may be defined by another module.
!
            if (error .ne. 0 .and. error .ne. 5007) goto 1000
            names(i) = namdes(i)
10       continue
!
         dimn(1)  = rtim(1)
         names(4) = modnam//'_'//namdes(4)
         error    = defelm (fd_nefis_res,names(4) ,'INTEGER' ,4,&
         &'n'        ,'-'   ,desdes(4) ,1  ,&
         &dimn       )
!
!           Derive cell name from group name.
!
         i      = index (grnamd,'GROUP') - 1
         celnam = grnamd(1:i)//'CEL'
!
         error  = defcel (fd_nefis_res, celnam ,4 ,names)
         if (error.ne.0) goto 1000
!
         dimn(1) = 0
         error = defgrp(fd_nefis_res, grnamd, celnam, 0 ,dimn ,ord )
         if (error.ne.0) goto 1000
!
!           Create data group on data file.
!
         error = credat (fd_nefis_res, grnamd, grnamd)
         if (error.ne.0) goto 1000
!
!           Write Block Descriptor
!
         ncelm  = 0
         ncelh  = 0
         buf(1) = ncelm
         error  = putiel(fd_nefis_res ,grnamd ,namdes(1) ,&
         &uindex  ,ord    ,buf    )
         if (error.ne.0) goto 1000

         buf(1) = ncelh
         error  = putiel(fd_nefis_res ,grnamd ,namdes(2) ,&
         &uindex  ,ord    ,buf    )
         if (error.ne.0) goto 1000

         buf(1) = rtim(1)
         error  = putiel(fd_nefis_res ,grnamd ,namdes(3) ,&
         &uindex  ,ord    ,buf    )
         if (error.ne.0) goto 1000
!
         if (rtim(1).gt.0) then
            error  = putiel(fd_nefis_res ,grnamd ,names(4) ,&
            &uindex  ,ord    ,rtim(2))
            if (error.ne.0) goto 1000
         endif
!
         neferr = 2
!
      else
!
!           Data group already exists, so read last cel numbers.
!           Apparently an existing output file will be extended.
!
         error = getiel(fd_nefis_res ,grnamd ,namdes(1) ,&
         &uindex  ,ord    ,4      ,buf       )
         ncelm = buf(1)
         if (error.ne.0) goto 1000
!
         error = getiel(fd_nefis_res ,grnamd ,namdes(2) ,&
         &uindex  ,ord    ,4      ,buf       )
         if (error.ne.0) goto 1000
         ncelh = buf(1)
!
      endif
   endif
!
!     If writim = .false. the time step counter must be updated at the
!     last time step.
!
!     Check has been switched off because of the following reason:
!
!     - If an error occurs the counters are never written to file.
!
!     if (writim) then
   if (kodes.ne.2 .and. ncelm .ge. 0) then
!
!           Update Map time step counter on file.
!
      buf(1) = ncelm
      error  = putiel(fd_nefis_res ,grnamd ,namdes(1) ,&
      &uindex  ,ord    ,buf    )
      if (error.ne.0) goto 1000
   endif
   if (kodes.ne.1 .and. ncelh .ge. 0) then
!
!           Update History time step counter on file.
!
      buf(1) = ncelh
      error  = putiel(fd_nefis_res ,grnamd ,namdes(2) ,&
      &uindex  ,ord    ,buf    )
      if (error.ne.0) goto 1000
   endif
!     endif
!
   return
!
1000 continue
   neferr = error
!
end
