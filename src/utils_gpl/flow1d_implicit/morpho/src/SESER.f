      subroutine seser  (nseman ,nsemap ,nsetim ,ngrid  ,nsedrd ,itim  ,
     &                   istep, nstep, first, writim, juer,
     &                   fd_nefis_res,
     &                   sedmap ,sedtim ,sedtr  ,ncelse ,secpre,
     &                   buf    ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SESER (SEdiment write SEdiment Results)
c
c Module description: Writing of user defined sediment results to the
c                     result file.
c
c                     The user selected sediment results at user sup-
c                     plied locations and time levels will be stored on
c                     the result file. The stored data can be processed
c                     further by the User Interface.
c
c                     The user can select functions of place and of
c                     time. Writing can start on a new file or in case
c                     the data model is unchanged an existing file can
c                     be extended.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 19 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c 13 dafdrs            P  -
c 12 defdrs            P  -
c  9 first             I  True in case of first call.
c  7 istep             I  Current time step number (t(n+1)).
c  6 itim              P  -
c 11 juer              P  -
c 20 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 17 ncelse(2)         IO Contains actual cell numbers of sediment modu-
c                         le:
c                         (1) = ncelm (Map)
c                         (2) = ncelh (History)
c  4 ngrid             I  Number of grid points in network.
c  5 nsedrd            I  Number of defined sedredge branches.
c  1 nseman            I  Number of main codes of sediment results.
c  2 nsemap            I  Number of entries in sedmap.
c  3 nsetim            I  Number of entries in sedtim.
c  8 nstep             I  Last time step number in simulation.
c 18 secpre(nseman)    I  secpre(i) = index in block tabel (1..nentri)
c                         for main code i of sediment results.
c 14 sedmap(nsemap)    I  Parameter list for MAP block with sediment
c                         results:
c                         (1)      = Report begin time
c                         (2)      = Report end time
c                         (3)      = Report time step
c                         (4)      = Report parameter 1 main code
c                         (5)      = Report parameter 1 sub code
c                         (i)      = Report parameter 1 main code
c                         (i+1)    = Report parameter 1 sub code
c                         (nsemap) = Report parameter n sub code
c 15 sedtim(nsetim)    I  Parameter list for HIST block with sediment
c                         results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nsetim) = Report parameter n sub code
c 16 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c 10 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flsdat  FLuSh buffers of DATa file
c putrel  PUT Real ELement to a nefis file
c resadm  RESults; make ADMinistration for element selection
c resdes  RESults; DEScription group is defined
c resini  RESults; writing is INItialized
c restim  RESults; writing of current TIMe
c yesmap  YES MAP results are written
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: seser.pf,v $
c Revision 1.7  1997/06/17  11:27:16  kuipe_j
c output in history format
c
c Revision 1.6  1996/12/03  08:23:12  kuipe_j
c dimension of element names improved
c
c Revision 1.5  1996/09/03  14:46:58  kuipe_j
c frequency time hist,Power distribution added
c
c Revision 1.4  1995/10/18  09:00:45  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:56:32  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:31  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:30  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:05  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:23  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      nseman  ,nsemap,nsetim ,ngrid ,juer  ,istep  ,nstep , 
     &             nsedrd  ,ker
      integer      fd_nefis_res, sedmap(nsemap),
     &             sedtim(nsetim) ,secpre(nseman),
     &             itim  (2)      ,ncelse(2)
      real         sedtr (ngrid,*),buf(ngrid)
      logical      first   ,writim
c
c     Declaration of local variables
c
      integer      nentri
      parameter   (nentri=4)
      integer      neferr  ,i      ,j     ,ie       ,nrerr   ,ind   ,
     &             ncelm   ,ncelh  ,nsk   ,nlc      ,lastcod
      integer      usrord(1),
     &             uindex(3)
      character*16 grnamm          ,grnamh          ,grnamd         ,
     &             name
      character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,
     &             nameac(nentri+1)
      character*64 descel(nentri)
      character*8  txt
      logical      llog  ,new ,newuit
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     Declaration of external functions
c
      integer      putrel ,flsdat
      logical      yesmap
      external     putrel ,flsdat ,yesmap
c
      data  usrord /1/
c
c     Definition of elements.
c
      data (descel(i) ,nameel(i) ,quanel(i) ,
     &      unitel(i),i=1,nentri) /
c
c      1 
     &    'Sediment transport'         ,'HIS_St' ,'St' ,'m3/s' ,
c      2
     &    'Sediment transport left'    ,'HIS_Sl' ,'Sl' ,'m3/s' ,
c      2
     &    'Sediment transport right'   ,'HIS_Sr' ,'Sr' ,'m3/s' ,
c      2
     &    'Sediment transport exchange','HIS_Se' ,'Se' ,'m3/s' /
c
      data  grnamm   ,grnamh ,grnamd/
     &      'SEDT-MAP-GROUP' ,
     &      'SEDT-HIS-GROUP' ,
     &      'SEDT-DES-GROUP' /
c
      ncelm   = ncelse(1)
      ncelh   = ncelse(2)
      lastcod = nentri
c
c     Initialize.
c
      if (first) then
         nrerr = eseboo
c
         call resini (fd_nefis_res ,grnamd ,grnamm ,grnamh ,'SEDT' ,
     &                nentri ,nsemap ,nsetim ,ngrid  ,itim   ,nameel ,
     &                quanel ,unitel ,descel ,secpre ,sedmap ,sedtim ,
     &                ncelm  ,ncelh  ,nameac ,lastcod,neferr )
c
         if (neferr.ne.0) goto 1000
      endif
c
c     Write Map results
c
      if (nsemap .gt. 3 .and. ncelm .ge. 0) then
         if (yesmap(sedmap(1),sedmap(2),sedmap(3),istep)) then
            nrerr = esemap
c
            call restim (fd_nefis_res ,grnamm ,1  ,itim  ,ncelm  ,
     &                   nameac ,neferr )
            if (neferr.ne.0) goto 1000
c
            call resdes ('SEDT' ,fd_nefis_res ,grnamd ,1 ,writim ,
     &                   sedtim ,ncelm  ,ncelh  ,neferr )
            if (neferr.ne.0) goto 1000
c
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
c
            do 20 i = 4,nsemap,2
             ie = secpre(sedmap(i))+sedmap(i+1)
             if (ie.le.lastcod) then
c             
c              Codes > lastcod can only be used in his format             
c
c              The element names of the Map block start with
c              MAP instead of HIS.
c
               name  = 'MAP'//nameel(ie)(4:)
               if (ie.eq.1) then
                  if (nsedrd .eq. 0) then
c
c                    There are no Sedredge branches.
c
                     neferr = putrel(fd_nefis_res, grnamm ,name   ,
     &                                uindex  ,usrord ,sedtr(1,1)     )
                     if (neferr.ne.0) goto 1000
c
c                    There are no Sedredge branches.
c
                  else
c
c                    Sedredge branches are present. The total transport
c                    must be calculated.
c
                     do 10 j =1,ngrid
                        buf(j) = sedtr(j,1) + sedtr(j,2)
   10                continue
                     neferr = putrel(fd_nefis_res, grnamm ,name   ,
     &                                uindex  ,usrord ,buf    )
                     if (neferr.ne.0) goto 1000
                  endif
               else if (ie.gt.1) then
                  neferr = putrel(fd_nefis_res, grnamm ,name   ,
     &                             uindex  ,usrord ,sedtr(1,ie-1)  )
                  if (neferr.ne.0) goto 1000
               endif
             endif
   20       continue
         endif
      endif
c
c     Write History results.
c
      nlc = sedtim(1)
      new = mod(nsetim-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nsetim .gt. 1+nsk .and. ncelh .ge. 0) then
         if (new) then
            newuit = yesmap(sedtim(nlc+2),sedtim(nlc+3),sedtim(nlc+4),
     &                      istep)
         else
            newuit = .false.
         endif
         if ( (sedtim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = esehis
c
            call restim (fd_nefis_res ,grnamh ,1  ,itim  ,ncelh  ,
     &                   nameac ,neferr )
            if (neferr.ne.0) goto 1000
c
            call resdes ('SEDT' ,fd_nefis_res ,grnamd ,2 ,writim ,
     &                   sedtim ,ncelm  ,ncelh  ,neferr )
            if (neferr.ne.0) goto 1000
c
            uindex(1) = ncelh
            uindex(2) = ncelh
            uindex(3) = 1
c
            do 60 i = sedtim(1)+2+nsk,nsetim,2
             ie = secpre(sedtim(i))+sedtim(i+1)
             if (ie.le.lastcod) then
c             
c              Codes > lastcod can only be used in his format             
c               
               if (ie.eq.1) then
                  if (nsedrd .eq. 0) then
c
c                    There are no Sedredge branches.
c
                     do 30 j=1,sedtim(1)
                        buf(j) = sedtr(sedtim(j+1),1)
   30                continue
                  else
c
c                    Sedredge branches are present. The total transport
c                    must be calculated.
c
                     do 40 j=1,sedtim(1)
                        ind    = sedtim(j+1)
                        buf(j) = sedtr(ind,1) + sedtr(ind,2)
   40                continue
                  endif
               else if (ie.gt.1) then
                  do 50 j=1,sedtim(1)
                     buf(j) = sedtr(sedtim(j+1),ie-1)
   50             continue
               endif
               neferr = putrel(fd_nefis_res, grnamh ,nameel(ie) ,
     &                          uindex  ,usrord ,buf    )
               if (neferr.ne.0) goto 1000
             endif
   60       continue
         endif
      endif
c
c     Be sure that at the last step the number of cells has been
c     written correctly.
c
      if (istep.ge.nstep) then
         nrerr = eseeoo
         llog  = .true.
c
         call resdes ('SEDT'  ,fd_nefis_res ,grnamd ,3 ,llog  ,
     &                 sedtim ,ncelm  ,ncelh  ,neferr )
         if (neferr.ne.0) goto 1000
c
         neferr = flsdat (fd_nefis_res)
         if (neferr.ne.0) goto 1000
      endif
c
      ncelse(1) = ncelm
      ncelse(2) = ncelh
c
      return
c
 1000 continue
c
      ker = fatal
      write (txt,'(i8)') neferr
      call error (juer ,'SEWRES @'//txt//'@' ,nrerr ,ker)
c
      end
