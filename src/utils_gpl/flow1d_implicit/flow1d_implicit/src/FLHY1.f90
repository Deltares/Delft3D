subroutine flhy1 (fd_nefis_res ,nhyman ,nhymap ,nhytim ,ngrid  ,&
&itim   ,istep  ,nstep  ,first  ,writim ,juer   ,&
&hyrmap ,hyrtim ,h2     ,q2     ,q2s    ,at     ,&
&af     ,afs    ,c      ,cs     ,wf     ,wfs    ,&
&ncelm  ,ncelh  ,hycpre ,buf    ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLHY1 (FLow HYdrodynamic results 1)
!
! Module description: Subroutine FLHY1 writes the first part of the user
!                     selected flow results to the result file. The
!                     result file is processed by the User Interface.
!
!                     In subroutine FLHY1 the user selected water flow
!                     results at user supplied locations and time levels
!                     (System Specifications for 1D Modelling System
!                     Front End) will be stored on the result file. The
!                     stored data can be processed further by the User
!                     Interface.
!
!                     The user can select functions of place or func-
!                     tions of time.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 19 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 20 afs(ngrid,2)      I  Actual flow area per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
! 18 at(ngrid)         I  Actual total area at every grid point.
! 28 buf(ngrid)        O  Buffer for results to be written to NEFIS.
! 21 c(ngrid)          I  Actual Chezy coefficient for total channel in
!                         every grid point.
! 22 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
!                         sub 1, sub 2) for every grid point.
!  2 dafdrs            P  -
!  1 defdrs            P  -
! 10 first             I  True in case of first call.
! 15 h2(ngrid)         I  Water level in every grid point at time
!                         t(n+1).
! 27 hycpre(nhyman)    I  hycpre(i) = index in block table (1...nentri)
!                         for main code i of the hydrodynamic results.
! 13 hyrmap(nhymap)    I  Parameter list for MAP block with hydrodynamic
!                         results:
!                         (1)      = Report begin time
!                         (2)      = Report end time
!                         (3)      = Report time step
!                         (4)      = Report parameter 1 main code
!                         (5)      = Report parameter 1 sub code
!                         (i)      = Report parameter 1 main code
!                         (i+1)    = Report parameter 1 sub code
!                         (nhymap) = Report parameter n sub code
! 14 hyrtim(nhytim)    I  Parameter list for HIST block with hydrodyna-
!                         mic results:
!                         (1)      = Number of places
!                         (2)      = Report grid point 1
!                         (3)      = Report grid point 2
!                         (i)      = Report grid point n
!                         (i+1)    = Report parameter code 1
!                         (i+2)    = Report parameter sub code 1
!                         (nhytim) = Report parameter n sub code
!  8 istep             I  Current time step number (t(n+1)).
!  7 itim              P  -
! 12 juer              P  -
! 29 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 26 ncelh             I  Actual cell number of a history block of re-
!                         sult file.
! 25 ncelm             I  Actual cell number of a map block of result
!                         file.
!  6 ngrid             I  Number of grid points in network.
!  3 nhyman            I  Number of main codes of hydrodynamic results.
!  4 nhymap            I  Number of entries in hyrmap.
!  5 nhytim            I  Number of entries in hyrtim.
!  9 nstep             I  Last time step number in simulation.
! 16 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 17 q2s(ngrid,2)      I  Flow through main and sub section 1 at time
!                         t(n+1).
! 23 wf(ngrid)         I  Actual flow width at every grid point.
! 24 wfs(ngrid,2)      I  Actual flow width per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
! 11 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flsdat  FLuSh buffers of DATa file
! putrel  PUT Real ELement to a nefis file
! resdes  RESults; DEScription group is defined
! resini  RESults; writing is INItialized
! restim  RESults; writing of current TIMe
! yesmap  YES MAP results are written
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flhy1.pf,v $
! Revision 1.7  1999/03/15  15:49:59  kuipe_j
! tabs removed
!
! Revision 1.6  1997/06/17  11:26:30  kuipe_j
! output in history format
!
! Revision 1.5  1996/12/02  15:41:24  kuipe_j
! dimension for histories improvred
!
! Revision 1.4  1996/09/03  14:51:57  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.3  1995/05/30  09:55:03  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:01  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:45  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:30:56  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:50  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      nhyman ,nhymap ,nhytim ,ngrid  ,juer   ,istep  ,&
   &nstep  ,ncelm  ,ncelh  ,ker
   integer      fd_nefis_res, hyrmap(nhymap),&
   &hyrtim(nhytim) ,hycpre(nhyman),itim  (2)
   real         q2s(ngrid,2)  ,&
   &at(ngrid)      ,af(ngrid)     ,afs(ngrid,2)  ,&
   &c(ngrid)       ,cs(ngrid,3)   ,wf(ngrid)     ,&
   &wfs(ngrid,2)   ,buf(ngrid)
   double precision h2(ngrid), q2(ngrid)
   logical      first   ,writim
!
!     Declaration of local variables
!
   integer      nentri
   parameter   (nentri=18)
   integer      errr  ,i ,j ,ie  ,nrerr, igr ,nsk ,nlc ,lastcod
   integer      usrord(1),&
   &uindex(3)
   character(len=16) grnamm, grnamh, grnamd,&
   &name
   character(len=16) nameel(nentri),quanel(nentri),unitel(nentri),&
   &nameac(nentri+1)
   character(len=64) descel(nentri)
   character(len=8)  txt
   logical      llog   ,new   ,newuit
!
!     Declaration of external functions
!
   integer      putrel ,flsdat
   logical      yesmap
   external     putrel ,flsdat ,yesmap
!
!     Include sobek error code file
!
   include '../include/errcod.i'
!
   data  usrord /1/
!
!     Definition of elements.
!
   data (descel(i) ,nameel(i) ,quanel(i) ,&
   &unitel(i),i=1,nentri) /&
!
!      1
   &'Water level'                    ,'HIS_h'  ,'h'  , 'm'      ,&
!      2
   &'Total discharge'                ,'HIS_Qt' ,'Qt' , 'm3/s'   ,&
!      2
   &'Discharge main section'         ,'HIS_Qm' ,'Qm' , 'm3/s'   ,&
!      2
   &'Discharge sub section 1'        ,'HIS_Q1' ,'Q1' , 'm3/s'   ,&
!      2
   &'Discharge sub section 2'        ,'HIS_Q2' ,'Q2' , 'm3/s'   ,&
!      3
   &'Total area'                     ,'HIS_At' ,'At' , 'm2'     ,&
!      4
   &'Flow area'                      ,'HIS_Af' ,'Af' , 'm2'     ,&
!      4
   &'Flow area main section'         ,'HIS_Am' ,'Am' , 'm2'     ,&
!      4
   &'Flow area sub section 1'        ,'HIS_A1' ,'A1' , 'm2'     ,&
!      4
   &'Flow area sub section 2'        ,'HIS_A2' ,'A2' , 'm2'     ,&
!      5
   &'Chezy coefficient'              ,'HIS_Ct' ,'Ct' , 'm1/2 /s',&
!      5
   &'Chezy coefficient main section' ,'HIS_Cm' ,'Cm' , 'm1/2 /s',&
!      5
   &'Chezy coefficient sub section 1','HIS_C1' ,'C1' , 'm1/2 /s',&
!      5
   &'Chezy coefficient sub section 2','HIS_C2' ,'C2' , 'm1/2 /s',&
!      6
   &'Flow width'                     ,'HIS_Wf' ,'Wf' , 'm'      ,&
!      6
   &'Flow width main section'        ,'HIS_Wm' ,'Wm' , 'm'      ,&
!      6
   &'Flow width sub section 1'       ,'HIS_W1' ,'W1' , 'm'      ,&
!      6
   &'Flow width sub section 2'       ,'HIS_W2' ,'W2' , 'm'      /
!
   data  grnamm   ,grnamh ,grnamd/&
   &'FLOW-MAP-GROUP' ,&
   &'FLOW-HIS-GROUP' ,&
   &'FLOW-DES-GROUP' /
!
   lastcod = nentri
!
!     Initialize.
!
   if (first) then
      nrerr = eflboo
!
      call resini (fd_nefis_res ,grnamd ,grnamm ,grnamh ,'FLOW' ,&
      &nentri ,nhymap ,nhytim ,ngrid  ,itim   ,nameel ,&
      &quanel ,unitel ,descel ,hycpre ,hyrmap ,hyrtim ,&
      &ncelm  ,ncelh  ,nameac ,lastcod,errr   )
!
      if (errr.ne.0) goto 1000
   endif
!
!     Write Map results
!
   if (nhymap .gt. 3 .and. ncelm .ge. 0) then
      if (yesmap(hyrmap(1),hyrmap(2),hyrmap(3),istep)) then
         nrerr = eflmap
!
         call restim (fd_nefis_res ,grnamm ,1  ,itim  ,ncelm  ,&
         &nameac ,errr   )
         if (errr.ne.0) goto 1000
!
         call resdes ('FLOW' ,fd_nefis_res ,grnamd ,1 ,writim ,&
         &hyrtim ,ncelm  ,ncelh  ,errr   )
         if (errr.ne.0) goto 1000
!
         uindex(1) = ncelm
         uindex(2) = ncelm
         uindex(3) = 1
!
         do i = 4,nhymap,2
            ie = hycpre(hyrmap(i))+hyrmap(i+1)
            if (ie.le.lastcod) then
!
!              Codes > lastcod can only be used in his format
!
!              The element names of the Map block start with
!              MAP instead of HIS.
!
               name  = 'MAP'//nameel(ie)(4:)
               if ( ie <= 1 .or. ie > 18 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord , real(h2))
               else if ( ie == 2 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord , real(q2))
               else if ( ie == 3 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,q2s(1,1))
               else if ( ie == 4 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,q2s(1,2))
               else if ( ie == 5 ) then
                  do igr = 1, ngrid
                     buf(igr) = real(q2(igr)) - q2s(igr,1) - q2s(igr,2)
                  end do
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,buf    )
               else if ( ie == 6 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,at     )
               else if ( ie == 7 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,af     )
               else if ( ie == 8 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,afs(1,1))
               else if ( ie == 9 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,afs(1,2))
               else if ( ie == 10 ) then
                  do igr = 1, ngrid
                     buf(igr) = af(igr) - afs(igr,1) - afs(igr,2)
                  end do
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,buf    )
               else if ( ie == 11 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,c      )
               else if ( ie == 12 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,cs(1,1))
               else if ( ie == 13 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,cs(1,2))
               else if ( ie == 14 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,cs(1,3))
               else if ( ie == 15 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,wf     )
               else if ( ie == 16 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,wfs(1,1))
               else if ( ie == 17 ) then
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,wfs(1,2))
               else if ( ie == 18 ) then
                  do igr = 1, ngrid
                     buf(igr) = wf(igr) - wfs(igr,1) - wfs(igr,2)
                  end do
                  errr = putrel (fd_nefis_res ,grnamm ,name   ,&
                  &uindex  ,usrord ,buf    )
               end if
               if (errr.ne.0) goto 1000
            end if
         end do
      end if
   endif
!
!     Write History results.
!
   nlc = hyrtim(1)
   new = mod(nhytim-nlc,2) .eq. 0

   if ( new ) then
      nsk = 3
   else
      nsk = 0
   endif

   if (nhytim .gt. 1+nsk .and. ncelh .ge. 0) then
      if (new) then
         newuit = yesmap(hyrtim(nlc+2),hyrtim(nlc+3),hyrtim(nlc+4),&
         &istep)
      else
         newuit = .false.
      endif
      if ((hyrtim(1) .gt.0  .and.  .not. new)  .or. newuit) then
         nrerr = eflhis
!
         call restim (fd_nefis_res ,grnamh ,1  ,itim  ,ncelh  ,&
         &nameac ,errr   )
         if (errr.ne.0) goto 1000
!
         call resdes ('FLOW' ,fd_nefis_res ,grnamd ,2 ,writim ,&
         &hyrtim ,ncelm  ,ncelh  ,errr   )
         if (errr.ne.0) goto 1000
!
         uindex(1) = ncelh
         uindex(2) = ncelh
         uindex(3) = 1
!
         do i = hyrtim(1)+2+nsk,nhytim,2
            ie = hycpre(hyrtim(i))+hyrtim(i+1)
            if (ie.le.lastcod) then
!
!              Codes > lastcod can only be used in his format
!
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
               errr = putrel (fd_nefis_res ,grnamh ,nameel(ie) ,&
               &uindex  ,usrord ,buf    )
               if (errr.ne.0) goto 1000
            end if
         end do
      end if
   endif
!
!     Be sure that at the last step the number of cells has been
!     written correctly.
!
   if (istep.ge.nstep) then
      nrerr = efleoo
      llog  = .true.
!
      call resdes ('FLOW'  ,fd_nefis_res ,grnamd ,3 ,llog ,&
      &hyrtim ,ncelm  ,ncelh  ,errr   )
      if (errr.ne.0) goto 1000
!
      errr = flsdat (fd_nefis_res)
      if (errr.ne.0) goto 1000
   endif
!
   return
!
1000 continue
!
   ker = fatal
   write (txt,'(i8)') errr
   call sre_error (juer ,'FLHY1 @'//txt//'@' ,nrerr ,ker)
!
end
