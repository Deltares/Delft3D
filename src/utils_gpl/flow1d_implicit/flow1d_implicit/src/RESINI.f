      subroutine resini (fd_nefis_res ,grnamd ,grnamm ,grnamh ,modnam,
     &                   nentri ,nrmap  ,nrtim  ,ngrid  ,itim   ,nameel,
     &                   quanel ,unitel ,descel ,codpre ,rmap   ,rtim  ,
     &                   ncelm  ,ncelh  ,nameac ,lastcod,neferr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Fileio module (interface to nefis)
c
c Programmer:         J.Kuipers
c
c Module:             RESINI (RESults; writing is INItialized)
c
c Module description: Initialization of writing of results of a module.
c
c                     If no results of this module are available on the
c                     result file the group definitions for map, history
c                     and description block will be made. If results do
c                     exist the file will be positioned according to the
c                     start time of the run.
c
c Precondition:       The NEFIS data and definition files are opened.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 16 codpre            P  -
c  2 dafdrs            P  -
c  1 defdrs            P  -
c 15 descel            P  -
c  3 grnamd            P  -
c  5 grnamh            P  -
c  4 grnamm            P  -
c 11 itim              P  -
c  6 modnam            P  -
c 21 nameac            P  -
c 12 nameel            P  -
c 20 ncelh             P  -
c 19 ncelm             P  -
c 22 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  7 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c 10 ngrid             P  -
c  8 nrmap             I  Number of entries in rmap.
c  9 nrtim             I  Number of entries in rtim.
c 13 quanel            P  -
c 17 rmap              P  -
c 18 rtim              P  -
c 14 unitel            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flsdat  FLuSh buffers of DATa file
c resdef  RESults; groups are DEFined
c resdes  RESults; DEScription group is defined
c restim  RESults; writing of current TIMe
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
c $Log: resini.pf,v $
c Revision 1.2  1995/05/30  06:57:17  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:16  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:44  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       nentri    ,nrmap     ,ngrid       ,nrtim       ,
     &              ncelm     ,ncelh     ,lastcod     ,neferr
      integer       fd_nefis_res, rmap(nrmap) ,rtim(nrtim) ,
     &              codpre(*) ,itim  (2)
      character(len=*) grnamd         ,grnamm          ,grnamh        ,
     &              modnam
      character(len=*) nameel(nentri) ,quanel(nentri)  ,unitel(nentri),
     &              nameac(*)      ,
     &              descel(nentri)
c
c     Declaration of local variables
c
      integer       error  ,i      ,ie    ,nsk
      logical       writed ,incall ,nefis
c
c     Declaration of external functions
c
      integer       flsdat
      external      flsdat
c
c     Definition of Block descriptor. This descriptor is always present.
c
      incall = .true.
      call resdes (modnam , fd_nefis_res, grnamd ,0 ,incall ,rtim ,
     &             ncelm  ,ncelh  ,error  )
      writed = error .eq. 2
c
c     Codes > lastcod can only be used in his-format.
c     So if no other codes are present the cel counter is set to -1
c     to define that no Map Block will be written.
c     
      nefis = .false.
      do i = 4,nrmap,2
         ie = codpre(rmap(i))+rmap(i+1)
         if (ie.le.lastcod) nefis = .true.
      enddo   
      if (.not.nefis) ncelm = -1
c
      if (nrmap .gt. 3 .and. ncelm .ge. 0) then
         call restim (fd_nefis_res ,grnamm ,0     ,itim  ,ncelm ,
     &                nameac ,error  )
         if (error.ne.0) goto 1000
      endif
c
c     Codes > lastcod can only be used in his-format.
c     So if no other codes are present the cel counter is set to -1
c     to define that no History Block will be written.
c               
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
         call restim (fd_nefis_res ,grnamh ,0     ,itim  ,ncelh ,
     &                nameac ,error  )
         if (error.ne.0) goto 1000
      endif
c
c     Definition of Map and History Block.
c
      call resdef (fd_nefis_res ,grnamm ,grnamh ,nentri ,nrmap  ,
     &             nrtim  ,ngrid  ,writed ,nameel ,quanel ,unitel ,
     &             descel ,codpre ,rmap   ,rtim   ,nameac ,lastcod,
     &             ncelh  ,ncelm  ,error  )
      if (error.ne.0) goto 1000
c
      error =  flsdat (fd_nefis_res)
c
 1000 continue
      neferr = error
c
      end
