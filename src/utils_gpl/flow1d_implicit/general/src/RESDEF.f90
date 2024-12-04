subroutine resdef (fd_nefis_res ,grnamm ,grnamh ,nentri ,nrmap ,&
&nrtim  ,ngrid  ,writed ,nameel ,quanel ,unitel,&
&descel ,codpre ,rmap   ,rtim   ,nameac ,&
&lastcod,ncelh  ,ncelm  ,neferr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Fileio module (interface to nefis)
!
! Programmer:         J.Kuipers
!
! Module:             RESDEF (RESults; groups are DEFined)
!
! Module description: Definition of map and history group for results of
!                     a module.
!
!                     If the user has specified writing of map results
!                     of this module and the group definition for maps,
!                     does not exist, this group definition will be
!                     made.
!
!                     If the user has specified writing of history re-
!                     sults of this module the group definition will be
!                     made. The group definition must not exist already.
!
!                     The groups contain only user defined result loca-
!                     tions and quantities.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 14 codpre(*)         I  codpre(i) = index in block tabel (1..nentri)
!                         for main code i.
!  2 dafdrs            P  -
!  1 defdrs            P  -
! 13 descel            P  -
!  4 grnamh            I  Name of data group for History block.
!  3 grnamm            I  Name of data group for Map block.
! 17 nameac(nentri)    O  All element names of a data group.
! 10 nameel(nentri)    I  All possible element names of a block.
! 18 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  5 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  8 ngrid             I  Number of grid points in network.
!  6 nrmap             I  Number of entries in rmap.
!  7 nrtim             I  Number of entries in rtim.
! 11 quanel            P  -
! 15 rmap(nrmap)       I  Parameter list for a MAP block with results:
!                         (1)      = Report begin time
!                         (2)      = Report end time
!                         (3)      = Report time step
!                         (4)      = Report parameter 1 main code
!                         (5)      = Report parameter 1 sub code
!                         (i)      = Report parameter 1 main code
!                         (i+1)    = Report parameter 1 sub code
!                         (nrmap)  = Report parameter n sub code
! 16 rtim(nrtim)       I  Parameter list for a HIST block with results:
!                         (1)      = Number of places
!                         (2)      = Report grid point 1
!                         (3)      = Report grid point 2
!                         (i)      = Report grid point n
!                         (i+1)    = Report parameter code 1
!                         (i+2)    = Report parameter sub code 1
!                         (nrtim)  = Report parameter n sub code
! 12 unitel            P  -
!  9 writed            I  true, if descriptor block does not exist
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! credat  CREation of a data group in the DATa file
! defcel  DEFinition of a CELl
! defelm  DEFinition of an ELeMent
! defgrp  DEFinition of a GRouP
! flsdef  FLuSh buffers of DEFinition file
! inqdat  INQuire for info of DATa group on data file
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
! $Log: resdef.pf,v $
! Revision 1.7  1999/03/15  15:49:14  kuipe_j
! tabs removed
!
! Revision 1.6  1996/09/03  14:54:11  kuipe_j
! frequency time hist,etc
!
! Revision 1.5  1996/04/11  08:22:55  kuipe_j
! Kalman module added
!
! Revision 1.4  1995/11/21  14:15:52  kuipe_j
! proper initialization of error code
!
! Revision 1.3  1995/10/18  08:59:06  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  06:57:14  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:14  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:30:06  kuipe_j
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
   integer       nentri    ,nrmap     ,ngrid       ,nrtim       ,&
   &lastcod   ,ncelh     ,ncelm       ,neferr
   integer       fd_nefis_res ,rmap(nrmap) ,rtim(nrtim) ,&
   &codpre(*)
   logical       writed
   character*(*) grnamm         ,grnamh
   character*(*) nameel(nentri) ,quanel(nentri)  ,unitel(nentri) ,&
   &nameac(*)      ,&
   &descel(nentri)
!
!     Declaration of local variables
!
   integer        error  ,i     ,l   ,nelems ,ie  ,ie1  ,ie2
   integer        dimn(1),dimpi(1)   ,ord(1) ,nlc ,nsk
   logical        new
   character*16   celnam ,name  ,name1
   character*2    txt
!
!     Declaration of external functions
!
   integer        defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
   external       defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
!
   data           ord    /1/
!
!     Definition of Map block
!
   error    = 0
   dimpi(1) = 1
!
   if (nrmap .gt. 3 .and. ncelm .ge. 0) then
!
!        Maps will be written.
!
      error  = inqdat (fd_nefis_res ,grnamm ,grnamm)
!
      if (error .eq. 6004) then
!
!           Data group for Map doesn't exist, so
!           define elements, cel and group def. The time element has
!           been defined already.
!
         dimn(1) = ngrid
         nelems  = 1
         do 10 i = 4,nrmap,2
            ie = codpre(rmap(i))+rmap(i+1)
            if (ie.le.lastcod) then
!
!              Codes > lastcod can only be used in his format
!
!              If the entry does not exist the last definition from
!              the table will be used. The name will be made unique by
!              changing the last character of the name in a sequence
!              number.
!
               ie1 = min(ie,nentri)
               ie2 = ie-nentri
               if (ie2.gt.0) then
                  if (ie2 .gt. 9) then
                     write(txt,'(i2)') ie2
                  else
                     write(txt,'(i1,a)') ie2, ' '
                  endif
                  l    = index(nameel(nentri),' ') - 2
                  name = nameel(nentri)(1:l)//txt
               else
                  name = nameel(ie1)
               endif
!
!              The element names of the Map block start with
!              MAP instead of HIS.
!
               name1 = 'MAP'//name(4:)
               if (name(4:) .eq. '_pi' ) then
                  error = defelm (fd_nefis_res, name1, 'INTEGER'   ,&
                  &4, quanel(ie1) ,unitel(ie1) ,&
                  &descel(ie1), 1, dimpi )
                  if (error .ne. 0 .and. error .ne. 5007) goto 1000
                  error = 0
               else
                  error = defelm (fd_nefis_res, name1, 'REAL'      ,&
                  &4, quanel(ie1) ,unitel(ie1) ,&
                  &descel(ie1), 1,dimn        )
                  if (error.ne.0) goto 1000
               endif
               nelems         = nelems+1
               nameac(nelems) = name1
            endif
10       continue
!
!           Derive cell name from group name.
!
         l      = index (grnamm,'GROUP') - 1
         celnam = grnamm(1:l)//'CEL'
         error  = defcel (fd_nefis_res ,celnam ,nelems ,nameac)
         if (error.ne.0) goto 1000
!
         dimn(1) = 0
         error = defgrp (fd_nefis_res ,grnamm ,celnam ,1 ,dimn ,ord)
         if (error.ne.0) goto 1000
!
!           Create data group on data file.
!
         error = credat (fd_nefis_res, grnamm, grnamm)
         if (error.ne.0) goto 1000
      endif
   endif
!
!     Definition of History block
!
   nlc = rtim(1)
   new = mod(nrtim-nlc,2) .eq. 0

   if ( new ) then
      nsk = 3
   else
      nsk = 0
   endif
!
   if (nrtim .gt. 1+nsk .and. ncelh .ge. 0) then
!
!        Histories will be written.
!
      error  = inqdat (fd_nefis_res ,grnamh ,grnamh)
!
      if (writed .and. error .eq. 6004) then
!
!           Data group for History doesn't exist, so
!           define elements, cel and group def. The time element has
!           been defined already.
!
         nelems = 1
         dimn(1) = rtim(1)
         do 20 i = rtim(1)+2+nsk,nrtim,2
            ie    = codpre(rtim(i))+rtim(i+1)
            if (ie.le.lastcod) then
!
!              Codes > lastcod can only be used in his format
!
!
!              If the entry does not exist the last definition from
!              the table will be used. The name will be made unique by
!              changing the last character of the name in a sequence
!              number.
!
               ie1 = min(ie,nentri)
               ie2 = ie-nentri
               if (ie2.gt.0) then
                  if (ie2 .gt. 9) then
                     write(txt,'(i2)') ie2
                  else
                     write(txt,'(i1,a)') ie2, ' '
                  endif
                  l    = index(nameel(nentri),' ') - 2
                  name = nameel(nentri)(1:l)//txt
               else
                  name = nameel(ie1)
               endif
!
               if (name(4:) .eq. '_pi' ) then
                  error = defelm(fd_nefis_res, name, 'INTEGER'   ,&
                  &4, quanel(ie1) ,unitel(ie1) ,&
                  &descel(ie1), 1, dimpi )
                  if (error .ne. 0 .and. error .ne. 5007) goto 1000
                  error = 0
               else
                  error = defelm(fd_nefis_res, name, 'REAL',&
                  &4, quanel(ie1), unitel(ie1) ,&
                  &descel(ie1), 1, dimn)
                  if (error.ne.0) goto 1000
               endif
               nelems         = nelems+1
               nameac(nelems) = name
            endif
20       continue
!
!           Derive cell name from group name.
!
         l      = index (grnamh,'GROUP') - 1
         celnam = grnamh(1:l)//'CEL'
         error  = defcel (fd_nefis_res ,celnam ,nelems ,nameac)
         if (error.ne.0) goto 1000
!
         dimn(1) = 0
         error = defgrp (fd_nefis_res ,grnamh ,celnam ,1 ,dimn ,ord)
         if (error.ne.0) goto 1000
!
!           Create data group on data file.
!
         error = credat (fd_nefis_res ,grnamh ,grnamh)
         if (error.ne.0) goto 1000
      else
!
!           Data group should exist.
!
         if (error .ne. 0) goto 1000
      endif
   endif
!
   if (writed) error = flsdef(fd_nefis_res)
!
1000 continue
   neferr = error
!
end
