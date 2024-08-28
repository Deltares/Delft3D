      subroutine flhy1 (fd_nefis_res ,nhyman ,nhymap ,nhytim ,ngrid  ,
     &                  itim   ,istep  ,nstep  ,first  ,writim ,juer   ,
     &                  hyrmap ,hyrtim ,h2     ,q2     ,q2s    ,at     ,
     &                  af     ,afs    ,c      ,cs     ,wf     ,wfs    ,
     &                  ncelm  ,ncelh  ,hycpre ,buf    ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLHY1 (FLow HYdrodynamic results 1)
c
c Module description: Subroutine FLHY1 writes the first part of the user
c                     selected flow results to the result file. The
c                     result file is processed by the User Interface.
c
c                     In subroutine FLHY1 the user selected water flow
c                     results at user supplied locations and time levels
c                     (System Specifications for 1D Modelling System
c                     Front End) will be stored on the result file. The
c                     stored data can be processed further by the User
c                     Interface.
c
c                     The user can select functions of place or func-
c                     tions of time.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 19 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c 20 afs(ngrid,2)      I  Actual flow area per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c 18 at(ngrid)         I  Actual total area at every grid point.
c 28 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c 21 c(ngrid)          I  Actual Chezy coefficient for total channel in
c                         every grid point.
c 22 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
c                         sub 1, sub 2) for every grid point.
c  2 dafdrs            P  -
c  1 defdrs            P  -
c 10 first             I  True in case of first call.
c 15 h2(ngrid)         I  Water level in every grid point at time
c                         t(n+1).
c 27 hycpre(nhyman)    I  hycpre(i) = index in block table (1...nentri)
c                         for main code i of the hydrodynamic results.
c 13 hyrmap(nhymap)    I  Parameter list for MAP block with hydrodynamic
c                         results:
c                         (1)      = Report begin time
c                         (2)      = Report end time
c                         (3)      = Report time step
c                         (4)      = Report parameter 1 main code
c                         (5)      = Report parameter 1 sub code
c                         (i)      = Report parameter 1 main code
c                         (i+1)    = Report parameter 1 sub code
c                         (nhymap) = Report parameter n sub code
c 14 hyrtim(nhytim)    I  Parameter list for HIST block with hydrodyna-
c                         mic results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nhytim) = Report parameter n sub code
c  8 istep             I  Current time step number (t(n+1)).
c  7 itim              P  -
c 12 juer              P  -
c 29 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 26 ncelh             I  Actual cell number of a history block of re-
c                         sult file.
c 25 ncelm             I  Actual cell number of a map block of result
c                         file.
c  6 ngrid             I  Number of grid points in network.
c  3 nhyman            I  Number of main codes of hydrodynamic results.
c  4 nhymap            I  Number of entries in hyrmap.
c  5 nhytim            I  Number of entries in hyrtim.
c  9 nstep             I  Last time step number in simulation.
c 16 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 17 q2s(ngrid,2)      I  Flow through main and sub section 1 at time
c                         t(n+1).
c 23 wf(ngrid)         I  Actual flow width at every grid point.
c 24 wfs(ngrid,2)      I  Actual flow width per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c 11 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flsdat  FLuSh buffers of DATa file
c putrel  PUT Real ELement to a nefis file
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
c $Log: flhy1.pf,v $
c Revision 1.7  1999/03/15  15:49:59  kuipe_j
c tabs removed
c
c Revision 1.6  1997/06/17  11:26:30  kuipe_j
c output in history format
c
c Revision 1.5  1996/12/02  15:41:24  kuipe_j
c dimension for histories improvred
c
c Revision 1.4  1996/09/03  14:51:57  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.3  1995/05/30  09:55:03  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:01  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:45  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:30:56  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:50  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      nhyman ,nhymap ,nhytim ,ngrid  ,juer   ,istep  ,
     &             nstep  ,ncelm  ,ncelh  ,ker
      integer      fd_nefis_res, hyrmap(nhymap),
     &             hyrtim(nhytim) ,hycpre(nhyman),itim  (2)
      real         q2s(ngrid,2)  ,
     &             at(ngrid)      ,af(ngrid)     ,afs(ngrid,2)  ,
     &             c(ngrid)       ,cs(ngrid,3)   ,wf(ngrid)     ,
     &             wfs(ngrid,2)   ,buf(ngrid)
      double precision h2(ngrid), q2(ngrid)
      logical      first   ,writim
c
c     Declaration of local variables
c
      integer      nentri
      parameter   (nentri=18)
      integer      errr  ,i ,j ,ie  ,nrerr, igr ,nsk ,nlc ,lastcod
      integer      usrord(1),
     &             uindex(3)
      character(len=16) grnamm, grnamh, grnamd,
     &             name
      character(len=16) nameel(nentri),quanel(nentri),unitel(nentri),
     &             nameac(nentri+1)
      character(len=64) descel(nentri)
      character(len=8)  txt
      logical      llog   ,new   ,newuit
c
c     Declaration of external functions
c
      integer      putrel ,flsdat
      logical      yesmap
      external     putrel ,flsdat ,yesmap
c
c     Include sobek error code file
c
      include '../include/errcod.i'
c
      data  usrord /1/
c
c     Definition of elements.
c
      data (descel(i) ,nameel(i) ,quanel(i) ,
     &      unitel(i),i=1,nentri) /
c
c      1  
     &    'Water level'                    ,'HIS_h'  ,'h'  , 'm'      ,
c      2  
     &    'Total discharge'                ,'HIS_Qt' ,'Qt' , 'm3/s'   ,
c      2  
     &    'Discharge main section'         ,'HIS_Qm' ,'Qm' , 'm3/s'   ,
c      2  
     &    'Discharge sub section 1'        ,'HIS_Q1' ,'Q1' , 'm3/s'   ,
c      2  
     &    'Discharge sub section 2'        ,'HIS_Q2' ,'Q2' , 'm3/s'   ,
c      3  
     &    'Total area'                     ,'HIS_At' ,'At' , 'm2'     ,
c      4  
     &    'Flow area'                      ,'HIS_Af' ,'Af' , 'm2'     ,
c      4  
     &    'Flow area main section'         ,'HIS_Am' ,'Am' , 'm2'     ,
c      4  
     &    'Flow area sub section 1'        ,'HIS_A1' ,'A1' , 'm2'     ,
c      4  
     &    'Flow area sub section 2'        ,'HIS_A2' ,'A2' , 'm2'     ,
c      5  
     &    'Chezy coefficient'              ,'HIS_Ct' ,'Ct' , 'm1/2 /s',
c      5  
     &    'Chezy coefficient main section' ,'HIS_Cm' ,'Cm' , 'm1/2 /s',
c      5  
     &    'Chezy coefficient sub section 1','HIS_C1' ,'C1' , 'm1/2 /s',
c      5  
     &    'Chezy coefficient sub section 2','HIS_C2' ,'C2' , 'm1/2 /s',
c      6  
     &    'Flow width'                     ,'HIS_Wf' ,'Wf' , 'm'      ,
c      6  
     &    'Flow width main section'        ,'HIS_Wm' ,'Wm' , 'm'      ,
c      6  
     &    'Flow width sub section 1'       ,'HIS_W1' ,'W1' , 'm'      ,
c      6  
     &    'Flow width sub section 2'       ,'HIS_W2' ,'W2' , 'm'      /
c 
      data  grnamm   ,grnamh ,grnamd/
     &      'FLOW-MAP-GROUP' ,
     &      'FLOW-HIS-GROUP' ,
     &      'FLOW-DES-GROUP' /
c
      lastcod = nentri
c
c     Initialize.
c
      if (first) then
         nrerr = eflboo
c
         call resini (fd_nefis_res ,grnamd ,grnamm ,grnamh ,'FLOW' ,
     &                nentri ,nhymap ,nhytim ,ngrid  ,itim   ,nameel ,
     &                quanel ,unitel ,descel ,hycpre ,hyrmap ,hyrtim ,
     &                ncelm  ,ncelh  ,nameac ,lastcod,errr   )
c
         if (errr.ne.0) goto 1000
      endif
c
c     Write Map results
c
      if (nhymap .gt. 3 .and. ncelm .ge. 0) then
         if (yesmap(hyrmap(1),hyrmap(2),hyrmap(3),istep)) then
            nrerr = eflmap
c
            call restim (fd_nefis_res ,grnamm ,1  ,itim  ,ncelm  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('FLOW' ,fd_nefis_res ,grnamd ,1 ,writim ,
     &                   hyrtim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
c
            do i = 4,nhymap,2
             ie = hycpre(hyrmap(i))+hyrmap(i+1)
             if (ie.le.lastcod) then
c             
c              Codes > lastcod can only be used in his format             
c
c              The element names of the Map block start with
c              MAP instead of HIS.
c
               name  = 'MAP'//nameel(ie)(4:)
               if ( ie <= 1 .or. ie > 18 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord , real(h2))
               else if ( ie == 2 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord , real(q2))
               else if ( ie == 3 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,q2s(1,1))
               else if ( ie == 4 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,q2s(1,2))
               else if ( ie == 5 ) then
                  do igr = 1, ngrid
                     buf(igr) = real(q2(igr)) - q2s(igr,1) - q2s(igr,2)
                  end do
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,buf    )
               else if ( ie == 6 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,at     )
               else if ( ie == 7 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,af     )
               else if ( ie == 8 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,afs(1,1))
               else if ( ie == 9 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,afs(1,2))
               else if ( ie == 10 ) then
                  do igr = 1, ngrid
                     buf(igr) = af(igr) - afs(igr,1) - afs(igr,2)
                  end do
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,buf    )
               else if ( ie == 11 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,c      )
               else if ( ie == 12 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,cs(1,1))
               else if ( ie == 13 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,cs(1,2))
               else if ( ie == 14 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,cs(1,3))
               else if ( ie == 15 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,wf     )
               else if ( ie == 16 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,wfs(1,1))
               else if ( ie == 17 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,wfs(1,2))
               else if ( ie == 18 ) then
                  do igr = 1, ngrid
                     buf(igr) = wf(igr) - wfs(igr,1) - wfs(igr,2)
                  end do
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,
     &                           uindex  ,usrord ,buf    )
               end if
               if (errr.ne.0) goto 1000
             end if     
          end do
        end if
      endif
c
c     Write History results.
c
      nlc = hyrtim(1)
      new = mod(nhytim-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif

      if (nhytim .gt. 1+nsk .and. ncelh .ge. 0) then
         if (new) then
            newuit = yesmap(hyrtim(nlc+2),hyrtim(nlc+3),hyrtim(nlc+4),
     +                      istep)
         else
            newuit = .false.
         endif
         if ((hyrtim(1) .gt.0  .and.  .not. new)  .or. newuit) then
            nrerr = eflhis
c
            call restim (fd_nefis_res ,grnamh ,1  ,itim  ,ncelh  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('FLOW' ,fd_nefis_res ,grnamd ,2 ,writim ,
     &                   hyrtim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelh
            uindex(2) = ncelh
            uindex(3) = 1
c
            do i = hyrtim(1)+2+nsk,nhytim,2
             ie = hycpre(hyrtim(i))+hyrtim(i+1)
             if (ie.le.lastcod) then
c             
c              Codes > lastcod can only be used in his format             
c                              
               if ( ie <= 1 .or. ie > 18 ) then
                  do j=1,hyrtim(1)
                     buf(j) = real( h2(hyrtim(j+1)), kind=kind(buf) )
                  end do
               else if ( ie == 2 ) then
                  do j=1,hyrtim(1)
                     buf(j) = real( q2(hyrtim(j+1)), kind=kind(buf) )
                  end do
               else if ( ie == 3 ) then
                  do j=1,hyrtim(1)
                     buf(j) = q2s(hyrtim(j+1),1)
                  end do
               else if ( ie == 4 ) then
                  do j=1,hyrtim(1)
                     buf(j) = q2s(hyrtim(j+1),2)
                  end do
               else if ( ie == 5 ) then
                  do j=1,hyrtim(1)
                     igr = hyrtim(j+1)
                     buf(j) = q2(igr) - q2s(igr,1) - q2s(igr,2)
                  end do
               else if ( ie == 6 ) then
                  do j=1,hyrtim(1)
                     buf(j) = at(hyrtim(j+1))
                  end do
               else if ( ie == 7 ) then
                  do j=1,hyrtim(1)
                     buf(j) = af(hyrtim(j+1))
                  end do
               else if ( ie == 8 ) then
                  do j=1,hyrtim(1)
                     buf(j) = afs(hyrtim(j+1),1)
                  end do
               else if ( ie == 9 ) then
                  do j=1,hyrtim(1)
                     buf(j) = afs(hyrtim(j+1),2)
                  end do
               else if ( ie == 10 ) then
                  do j=1,hyrtim(1)
                     igr = hyrtim(j+1)
                     buf(j) = af(igr) - afs(igr,1) - afs(igr,2)
                  end do
               else if ( ie == 11 ) then
                  do j=1,hyrtim(1)
                     buf(j) = c(hyrtim(j+1))
                  end do
               else if ( ie == 12 ) then
                  do j=1,hyrtim(1)
                     buf(j) = cs(hyrtim(j+1),1)
                  end do
               else if ( ie == 13 ) then
                  do j=1,hyrtim(1)
                     buf(j) = cs(hyrtim(j+1),2)
                  end do
               else if ( ie == 14 ) then
                  do j=1,hyrtim(1)
                     buf(j) = cs(hyrtim(j+1),3)
                  end do
               else if ( ie == 15 ) then
                  do j=1,hyrtim(1)
                     buf(j) = wf(hyrtim(j+1))
                  end do
               else if ( ie == 16 ) then
                  do j=1,hyrtim(1)
                     buf(j) = wfs(hyrtim(j+1),1)
                  end do
               else if ( ie == 17 ) then
                  do j=1,hyrtim(1)
                     buf(j) = wfs(hyrtim(j+1),2)
                  end do
               else if ( ie == 18 ) then
                  do j=1,hyrtim(1)
                     igr = hyrtim(j+1)
                     buf(j) = wf(igr) - wfs(igr,1) - wfs(igr,2)
                  end do
               end if
               errr = putrel (fd_nefis_res ,grnamh ,nameel(ie) ,
     &                           uindex  ,usrord ,buf    )
               if (errr.ne.0) goto 1000
            end if
         end do
        end if
      endif
c
c     Be sure that at the last step the number of cells has been
c     written correctly.
c
      if (istep.ge.nstep) then
         nrerr = efleoo
         llog  = .true.
c
         call resdes ('FLOW'  ,fd_nefis_res ,grnamd ,3 ,llog ,
     &                 hyrtim ,ncelm  ,ncelh  ,errr   )
         if (errr.ne.0) goto 1000
c
         errr = flsdat (fd_nefis_res)
         if (errr.ne.0) goto 1000
      endif
c
      return
c
 1000 continue
c
      ker = fatal
      write (txt,'(i8)') errr
      call sre_error (juer ,'FLHY1 @'//txt//'@' ,nrerr ,ker)
c
      end
