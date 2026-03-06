subroutine seser  (nseman ,nsemap ,nsetim ,ngrid  ,nsedrd ,itim  ,&
&istep, nstep, first, writim, juer,&
&fd_nefis_res,&
&sedmap ,sedtim ,sedtr  ,ncelse ,secpre,&
&buf    ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SESER (SEdiment write SEdiment Results)
!
! Module description: Writing of user defined sediment results to the
!                     result file.
!
!                     The user selected sediment results at user sup-
!                     plied locations and time levels will be stored on
!                     the result file. The stored data can be processed
!                     further by the User Interface.
!
!                     The user can select functions of place and of
!                     time. Writing can start on a new file or in case
!                     the data model is unchanged an existing file can
!                     be extended.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 19 buf(ngrid)        O  Buffer for results to be written to NEFIS.
! 13 dafdrs            P  -
! 12 defdrs            P  -
!  9 first             I  True in case of first call.
!  7 istep             I  Current time step number (t(n+1)).
!  6 itim              P  -
! 11 juer              P  -
! 20 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 17 ncelse(2)         IO Contains actual cell numbers of sediment modu-
!                         le:
!                         (1) = ncelm (Map)
!                         (2) = ncelh (History)
!  4 ngrid             I  Number of grid points in network.
!  5 nsedrd            I  Number of defined sedredge branches.
!  1 nseman            I  Number of main codes of sediment results.
!  2 nsemap            I  Number of entries in sedmap.
!  3 nsetim            I  Number of entries in sedtim.
!  8 nstep             I  Last time step number in simulation.
! 18 secpre(nseman)    I  secpre(i) = index in block tabel (1..nentri)
!                         for main code i of sediment results.
! 14 sedmap(nsemap)    I  Parameter list for MAP block with sediment
!                         results:
!                         (1)      = Report begin time
!                         (2)      = Report end time
!                         (3)      = Report time step
!                         (4)      = Report parameter 1 main code
!                         (5)      = Report parameter 1 sub code
!                         (i)      = Report parameter 1 main code
!                         (i+1)    = Report parameter 1 sub code
!                         (nsemap) = Report parameter n sub code
! 15 sedtim(nsetim)    I  Parameter list for HIST block with sediment
!                         results:
!                         (1)      = Number of places
!                         (2)      = Report grid point 1
!                         (3)      = Report grid point 2
!                         (i)      = Report grid point n
!                         (i+1)    = Report parameter code 1
!                         (i+2)    = Report parameter sub code 1
!                         (nsetim) = Report parameter n sub code
! 16 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
! 10 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flsdat  FLuSh buffers of DATa file
! putrel  PUT Real ELement to a nefis file
! resadm  RESults; make ADMinistration for element selection
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
! $Log: seser.pf,v $
! Revision 1.7  1997/06/17  11:27:16  kuipe_j
! output in history format
!
! Revision 1.6  1996/12/03  08:23:12  kuipe_j
! dimension of element names improved
!
! Revision 1.5  1996/09/03  14:46:58  kuipe_j
! frequency time hist,Power distribution added
!
! Revision 1.4  1995/10/18  09:00:45  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:56:32  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:07:31  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:30  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:05  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:23  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      nseman  ,nsemap,nsetim ,ngrid ,juer  ,istep  ,nstep ,&
   &nsedrd  ,ker
   integer      fd_nefis_res, sedmap(nsemap),&
   &sedtim(nsetim) ,secpre(nseman),&
   &itim  (2)      ,ncelse(2)
   real         sedtr (ngrid,*),buf(ngrid)
   logical      first   ,writim
!
!     Declaration of local variables
!
   integer      nentri
   parameter   (nentri=4)
   integer      neferr  ,i      ,j     ,ie       ,nrerr   ,ind   ,&
   &ncelm   ,ncelh  ,nsk   ,nlc      ,lastcod
   integer      usrord(1),&
   &uindex(3)
   character*16 grnamm          ,grnamh          ,grnamd         ,&
   &name
   character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,&
   &nameac(nentri+1)
   character*64 descel(nentri)
   character*8  txt
   logical      llog  ,new ,newuit
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     Declaration of external functions
!
   integer      putrel ,flsdat
   logical      yesmap
   external     putrel ,flsdat ,yesmap
!
   data  usrord /1/
!
!     Definition of elements.
!
   data (descel(i) ,nameel(i) ,quanel(i) ,&
   &unitel(i),i=1,nentri) /&
!
!      1
   &'Sediment transport'         ,'HIS_St' ,'St' ,'m3/s' ,&
!      2
   &'Sediment transport left'    ,'HIS_Sl' ,'Sl' ,'m3/s' ,&
!      2
   &'Sediment transport right'   ,'HIS_Sr' ,'Sr' ,'m3/s' ,&
!      2
   &'Sediment transport exchange','HIS_Se' ,'Se' ,'m3/s' /
!
   data  grnamm   ,grnamh ,grnamd/&
   &'SEDT-MAP-GROUP' ,&
   &'SEDT-HIS-GROUP' ,&
   &'SEDT-DES-GROUP' /
!
   ncelm   = ncelse(1)
   ncelh   = ncelse(2)
   lastcod = nentri
!
!     Initialize.
!
   if (first) then
      nrerr = eseboo
!
      call resini (fd_nefis_res ,grnamd ,grnamm ,grnamh ,'SEDT' ,&
      &nentri ,nsemap ,nsetim ,ngrid  ,itim   ,nameel ,&
      &quanel ,unitel ,descel ,secpre ,sedmap ,sedtim ,&
      &ncelm  ,ncelh  ,nameac ,lastcod,neferr )
!
      if (neferr.ne.0) goto 1000
   endif
!
!     Write Map results
!
   if (nsemap .gt. 3 .and. ncelm .ge. 0) then
      if (yesmap(sedmap(1),sedmap(2),sedmap(3),istep)) then
         nrerr = esemap
!
         call restim (fd_nefis_res ,grnamm ,1  ,itim  ,ncelm  ,&
         &nameac ,neferr )
         if (neferr.ne.0) goto 1000
!
         call resdes ('SEDT' ,fd_nefis_res ,grnamd ,1 ,writim ,&
         &sedtim ,ncelm  ,ncelh  ,neferr )
         if (neferr.ne.0) goto 1000
!
         uindex(1) = ncelm
         uindex(2) = ncelm
         uindex(3) = 1
!
         do 20 i = 4,nsemap,2
            ie = secpre(sedmap(i))+sedmap(i+1)
            if (ie.le.lastcod) then
!
!              Codes > lastcod can only be used in his format
!
!              The element names of the Map block start with
!              MAP instead of HIS.
!
               name  = 'MAP'//nameel(ie)(4:)
               if (ie.eq.1) then
                  if (nsedrd .eq. 0) then
!
!                    There are no Sedredge branches.
!
                     neferr = putrel(fd_nefis_res, grnamm ,name   ,&
                     &uindex  ,usrord ,sedtr(1,1)     )
                     if (neferr.ne.0) goto 1000
!
!                    There are no Sedredge branches.
!
                  else
!
!                    Sedredge branches are present. The total transport
!                    must be calculated.
!
                     do 10 j =1,ngrid
                        buf(j) = sedtr(j,1) + sedtr(j,2)
10                   continue
                     neferr = putrel(fd_nefis_res, grnamm ,name   ,&
                     &uindex  ,usrord ,buf    )
                     if (neferr.ne.0) goto 1000
                  endif
               else if (ie.gt.1) then
                  neferr = putrel(fd_nefis_res, grnamm ,name   ,&
                  &uindex  ,usrord ,sedtr(1,ie-1)  )
                  if (neferr.ne.0) goto 1000
               endif
            endif
20       continue
      endif
   endif
!
!     Write History results.
!
   nlc = sedtim(1)
   new = mod(nsetim-nlc,2) .eq. 0

   if ( new ) then
      nsk = 3
   else
      nsk = 0
   endif
!
   if (nsetim .gt. 1+nsk .and. ncelh .ge. 0) then
      if (new) then
         newuit = yesmap(sedtim(nlc+2),sedtim(nlc+3),sedtim(nlc+4),&
         &istep)
      else
         newuit = .false.
      endif
      if ( (sedtim(1).gt.0 .and. .not. new) .or. newuit ) then
         nrerr = esehis
!
         call restim (fd_nefis_res ,grnamh ,1  ,itim  ,ncelh  ,&
         &nameac ,neferr )
         if (neferr.ne.0) goto 1000
!
         call resdes ('SEDT' ,fd_nefis_res ,grnamd ,2 ,writim ,&
         &sedtim ,ncelm  ,ncelh  ,neferr )
         if (neferr.ne.0) goto 1000
!
         uindex(1) = ncelh
         uindex(2) = ncelh
         uindex(3) = 1
!
         do 60 i = sedtim(1)+2+nsk,nsetim,2
            ie = secpre(sedtim(i))+sedtim(i+1)
            if (ie.le.lastcod) then
!
!              Codes > lastcod can only be used in his format
!
               if (ie.eq.1) then
                  if (nsedrd .eq. 0) then
!
!                    There are no Sedredge branches.
!
                     do 30 j=1,sedtim(1)
                        buf(j) = sedtr(sedtim(j+1),1)
30                   continue
                  else
!
!                    Sedredge branches are present. The total transport
!                    must be calculated.
!
                     do 40 j=1,sedtim(1)
                        ind    = sedtim(j+1)
                        buf(j) = sedtr(ind,1) + sedtr(ind,2)
40                   continue
                  endif
               else if (ie.gt.1) then
                  do 50 j=1,sedtim(1)
                     buf(j) = sedtr(sedtim(j+1),ie-1)
50                continue
               endif
               neferr = putrel(fd_nefis_res, grnamh ,nameel(ie) ,&
               &uindex  ,usrord ,buf    )
               if (neferr.ne.0) goto 1000
            endif
60       continue
      endif
   endif
!
!     Be sure that at the last step the number of cells has been
!     written correctly.
!
   if (istep.ge.nstep) then
      nrerr = eseeoo
      llog  = .true.
!
      call resdes ('SEDT'  ,fd_nefis_res ,grnamd ,3 ,llog  ,&
      &sedtim ,ncelm  ,ncelh  ,neferr )
      if (neferr.ne.0) goto 1000
!
      neferr = flsdat (fd_nefis_res)
      if (neferr.ne.0) goto 1000
   endif
!
   ncelse(1) = ncelm
   ncelse(2) = ncelh
!
   return
!
1000 continue
!
   ker = fatal
   write (txt,'(i8)') neferr
   call error (juer ,'SEWRES @'//txt//'@' ,nrerr ,ker)
!
end
