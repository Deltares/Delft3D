subroutine resini (fd_nefis_res ,grnamd ,grnamm ,grnamh ,modnam,&
&nentri ,nrmap  ,nrtim  ,ngrid  ,itim   ,nameel,&
&quanel ,unitel ,descel ,codpre ,rmap   ,rtim  ,&
&ncelm  ,ncelh  ,nameac ,lastcod,neferr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Fileio module (interface to nefis)
!
! Programmer:         J.Kuipers
!
! Module:             RESINI (RESults; writing is INItialized)
!
! Module description: Initialization of writing of results of a module.
!
!                     If no results of this module are available on the
!                     result file the group definitions for map, history
!                     and description block will be made. If results do
!                     exist the file will be positioned according to the
!                     start time of the run.
!
! Precondition:       The NEFIS data and definition files are opened.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 16 codpre            P  -
!  2 dafdrs            P  -
!  1 defdrs            P  -
! 15 descel            P  -
!  3 grnamd            P  -
!  5 grnamh            P  -
!  4 grnamm            P  -
! 11 itim              P  -
!  6 modnam            P  -
! 21 nameac            P  -
! 12 nameel            P  -
! 20 ncelh             P  -
! 19 ncelm             P  -
! 22 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  7 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
! 10 ngrid             P  -
!  8 nrmap             I  Number of entries in rmap.
!  9 nrtim             I  Number of entries in rtim.
! 13 quanel            P  -
! 17 rmap              P  -
! 18 rtim              P  -
! 14 unitel            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flsdat  FLuSh buffers of DATa file
! resdef  RESults; groups are DEFined
! resdes  RESults; DEScription group is defined
! restim  RESults; writing of current TIMe
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
! $Log: resini.pf,v $
! Revision 1.2  1995/05/30  06:57:17  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:16  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:44  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       nentri    ,nrmap     ,ngrid       ,nrtim       ,&
   &ncelm     ,ncelh     ,lastcod     ,neferr
   integer       fd_nefis_res, rmap(nrmap) ,rtim(nrtim) ,&
   &codpre(*) ,itim  (2)
   character(len=*) grnamd         ,grnamm          ,grnamh        ,&
   &modnam
   character(len=*) nameel(nentri) ,quanel(nentri)  ,unitel(nentri),&
   &nameac(*)      ,&
   &descel(nentri)
!
!     Declaration of local variables
!
   integer       error  ,i      ,ie    ,nsk
   logical       writed ,incall ,nefis
!
!     Declaration of external functions
!
   integer       flsdat
   external      flsdat
!
!     Definition of Block descriptor. This descriptor is always present.
!
   incall = .true.
   call resdes (modnam , fd_nefis_res, grnamd ,0 ,incall ,rtim ,&
   &ncelm  ,ncelh  ,error  )
   writed = error .eq. 2
!
!     Codes > lastcod can only be used in his-format.
!     So if no other codes are present the cel counter is set to -1
!     to define that no Map Block will be written.
!
   nefis = .false.
   do i = 4,nrmap,2
      ie = codpre(rmap(i))+rmap(i+1)
      if (ie.le.lastcod) nefis = .true.
   enddo
   if (.not.nefis) ncelm = -1
!
   if (nrmap .gt. 3 .and. ncelm .ge. 0) then
      call restim (fd_nefis_res ,grnamm ,0     ,itim  ,ncelm ,&
      &nameac ,error  )
      if (error.ne.0) goto 1000
   endif
!
!     Codes > lastcod can only be used in his-format.
!     So if no other codes are present the cel counter is set to -1
!     to define that no History Block will be written.
!
   if (mod(nrtim-rtim(1),2) .eq. 0) then
      nsk = 3
   else
      nsk = 0
   endif
   nefis = .false.
   if (rtim(1) .ne. 0) then
      do i = rtim(1)+2+nsk,nrtim,2
         ie = codpre(rtim(i))+rtim(i+1)
         if (ie.le.lastcod) nefis = .true.
      enddo
   endif
   if (.not.nefis) ncelh = -1

   if (nrtim .gt. 1 .and. ncelh .ge. 0) then
      call restim (fd_nefis_res ,grnamh ,0     ,itim  ,ncelh ,&
      &nameac ,error  )
      if (error.ne.0) goto 1000
   endif
!
!     Definition of Map and History Block.
!
   call resdef (fd_nefis_res ,grnamm ,grnamh ,nentri ,nrmap  ,&
   &nrtim  ,ngrid  ,writed ,nameel ,quanel ,unitel ,&
   &descel ,codpre ,rmap   ,rtim   ,nameac ,lastcod,&
   &ncelh  ,ncelm  ,error  )
   if (error.ne.0) goto 1000
!
   error =  flsdat (fd_nefis_res)
!
1000 continue
   neferr = error
!
end
