      subroutine resdef (fd_nefis_res ,grnamm ,grnamh ,nentri ,nrmap ,
     &                   nrtim  ,ngrid  ,writed ,nameel ,quanel ,unitel,
     &                   descel ,codpre ,rmap   ,rtim   ,nameac ,
     &                   lastcod,ncelh  ,ncelm  ,neferr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Fileio module (interface to nefis)
c
c Programmer:         J.Kuipers
c
c Module:             RESDEF (RESults; groups are DEFined)
c
c Module description: Definition of map and history group for results of
c                     a module.
c
c                     If the user has specified writing of map results
c                     of this module and the group definition for maps,
c                     does not exist, this group definition will be
c                     made.
c
c                     If the user has specified writing of history re-
c                     sults of this module the group definition will be
c                     made. The group definition must not exist already.
c
c                     The groups contain only user defined result loca-
c                     tions and quantities.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 14 codpre(*)         I  codpre(i) = index in block tabel (1..nentri)
c                         for main code i.
c  2 dafdrs            P  -
c  1 defdrs            P  -
c 13 descel            P  -
c  4 grnamh            I  Name of data group for History block.
c  3 grnamm            I  Name of data group for Map block.
c 17 nameac(nentri)    O  All element names of a data group.
c 10 nameel(nentri)    I  All possible element names of a block.
c 18 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  5 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  8 ngrid             I  Number of grid points in network.
c  6 nrmap             I  Number of entries in rmap.
c  7 nrtim             I  Number of entries in rtim.
c 11 quanel            P  -
c 15 rmap(nrmap)       I  Parameter list for a MAP block with results:
c                         (1)      = Report begin time
c                         (2)      = Report end time
c                         (3)      = Report time step
c                         (4)      = Report parameter 1 main code
c                         (5)      = Report parameter 1 sub code
c                         (i)      = Report parameter 1 main code
c                         (i+1)    = Report parameter 1 sub code
c                         (nrmap)  = Report parameter n sub code
c 16 rtim(nrtim)       I  Parameter list for a HIST block with results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nrtim)  = Report parameter n sub code
c 12 unitel            P  -
c  9 writed            I  true, if descriptor block does not exist
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c credat  CREation of a data group in the DATa file
c defcel  DEFinition of a CELl
c defelm  DEFinition of an ELeMent
c defgrp  DEFinition of a GRouP
c flsdef  FLuSh buffers of DEFinition file
c inqdat  INQuire for info of DATa group on data file
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
c $Log: resdef.pf,v $
c Revision 1.7  1999/03/15  15:49:14  kuipe_j
c tabs removed
c
c Revision 1.6  1996/09/03  14:54:11  kuipe_j
c frequency time hist,etc
c
c Revision 1.5  1996/04/11  08:22:55  kuipe_j
c Kalman module added
c
c Revision 1.4  1995/11/21  14:15:52  kuipe_j
c proper initialization of error code
c
c Revision 1.3  1995/10/18  08:59:06  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  06:57:14  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:14  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:30:06  kuipe_j
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
      integer       nentri    ,nrmap     ,ngrid       ,nrtim       ,
     &              lastcod   ,ncelh     ,ncelm       ,neferr
      integer       fd_nefis_res ,rmap(nrmap) ,rtim(nrtim) ,
     &              codpre(*)
      logical       writed
      character*(*) grnamm         ,grnamh
      character*(*) nameel(nentri) ,quanel(nentri)  ,unitel(nentri) ,
     &              nameac(*)      ,
     &              descel(nentri)
c
c     Declaration of local variables
c
      integer        error  ,i     ,l   ,nelems ,ie  ,ie1  ,ie2
      integer        dimn(1),dimpi(1)   ,ord(1) ,nlc ,nsk
      logical        new
      character*16   celnam ,name  ,name1
      character*2    txt
c
c     Declaration of external functions
c
      integer        defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
      external       defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
c
      data           ord    /1/
c
c     Definition of Map block
c
      error    = 0
      dimpi(1) = 1
c
      if (nrmap .gt. 3 .and. ncelm .ge. 0) then
c
c        Maps will be written.
c
         error  = inqdat (fd_nefis_res ,grnamm ,grnamm)
c
         if (error .eq. 6004) then
c
c           Data group for Map doesn't exist, so
c           define elements, cel and group def. The time element has
c           been defined already.
c
            dimn(1) = ngrid
            nelems  = 1
            do 10 i = 4,nrmap,2
             ie = codpre(rmap(i))+rmap(i+1)
             if (ie.le.lastcod) then
c             
c              Codes > lastcod can only be used in his format             
c
c              If the entry does not exist the last definition from
c              the table will be used. The name will be made unique by
c              changing the last character of the name in a sequence
c              number.
c
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
c
c              The element names of the Map block start with
c              MAP instead of HIS.
c
               name1 = 'MAP'//name(4:)
               if (name(4:) .eq. '_pi' ) then
                  error = defelm (fd_nefis_res, name1, 'INTEGER'   ,
     &                            4, quanel(ie1) ,unitel(ie1) ,
     &                            descel(ie1), 1, dimpi )
                  if (error .ne. 0 .and. error .ne. 5007) goto 1000
                  error = 0
               else
                  error = defelm (fd_nefis_res, name1, 'REAL'      ,
     &                            4, quanel(ie1) ,unitel(ie1) ,
     &                            descel(ie1), 1,dimn        )
                  if (error.ne.0) goto 1000
               endif
               nelems         = nelems+1
               nameac(nelems) = name1
             endif
   10       continue
c
c           Derive cell name from group name.
c
            l      = index (grnamm,'GROUP') - 1
            celnam = grnamm(1:l)//'CEL'
            error  = defcel (fd_nefis_res ,celnam ,nelems ,nameac)
            if (error.ne.0) goto 1000
c
            dimn(1) = 0
            error = defgrp (fd_nefis_res ,grnamm ,celnam ,1 ,dimn ,ord)
            if (error.ne.0) goto 1000
c
c           Create data group on data file.
c
            error = credat (fd_nefis_res, grnamm, grnamm)
            if (error.ne.0) goto 1000
         endif
      endif
c
c     Definition of History block
c
      nlc = rtim(1)
      new = mod(nrtim-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nrtim .gt. 1+nsk .and. ncelh .ge. 0) then
c
c        Histories will be written.
c
         error  = inqdat (fd_nefis_res ,grnamh ,grnamh)
c
         if (writed .and. error .eq. 6004) then
c
c           Data group for History doesn't exist, so
c           define elements, cel and group def. The time element has
c           been defined already.
c
            nelems = 1
            dimn(1) = rtim(1)
            do 20 i = rtim(1)+2+nsk,nrtim,2
             ie    = codpre(rtim(i))+rtim(i+1)
             if (ie.le.lastcod) then
c             
c              Codes > lastcod can only be used in his format             
c
c
c              If the entry does not exist the last definition from
c              the table will be used. The name will be made unique by
c              changing the last character of the name in a sequence
c              number.
c
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
c
               if (name(4:) .eq. '_pi' ) then
                  error = defelm(fd_nefis_res, name, 'INTEGER'   ,
     &                            4, quanel(ie1) ,unitel(ie1) ,
     &                            descel(ie1), 1, dimpi )
                  if (error .ne. 0 .and. error .ne. 5007) goto 1000
                  error = 0
               else
                  error = defelm(fd_nefis_res, name, 'REAL',
     &                            4, quanel(ie1), unitel(ie1) ,
     &                            descel(ie1), 1, dimn)
                  if (error.ne.0) goto 1000
               endif
               nelems         = nelems+1
               nameac(nelems) = name
             endif  
   20       continue
c
c           Derive cell name from group name.
c
            l      = index (grnamh,'GROUP') - 1
            celnam = grnamh(1:l)//'CEL'
            error  = defcel (fd_nefis_res ,celnam ,nelems ,nameac)
            if (error.ne.0) goto 1000
c
            dimn(1) = 0
            error = defgrp (fd_nefis_res ,grnamh ,celnam ,1 ,dimn ,ord)
            if (error.ne.0) goto 1000
c
c           Create data group on data file.
c
            error = credat (fd_nefis_res ,grnamh ,grnamh)
            if (error.ne.0) goto 1000
         else
c
c           Data group should exist.
c
            if (error .ne. 0) goto 1000
         endif
      endif
c
      if (writed) error = flsdef(fd_nefis_res)
c
 1000 continue
      neferr = error
c
      end
