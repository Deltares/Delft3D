subroutine gsser  (nseman ,nsemap ,nsetim ,ngrid  ,nlayer ,&
&itim   ,istep  ,nstep  ,first  ,writim ,juer  ,&
&fd_nefis_res, sedmap ,sedtim ,sedtr  ,grsize,&
&deff   ,ncelse ,secpre ,buf    ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Module
!
! Programmer:         J.Kuipers
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
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsser.F,v $
! Revision 1.2  1995/09/27  10:12:52  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!
!     Declaration of parameters
!
   integer      nseman  ,nsemap,nsetim ,ngrid ,nlayer ,juer  ,&
   &istep   ,nstep ,ker
   integer      fd_nefis_res, sedmap(nsemap),&
   &sedtim(nsetim) ,secpre(nseman),&
   &itim  (2)      ,ncelse(2)
   real         sedtr (ngrid)  ,buf(ngrid)    ,deff(ngrid,2) ,&
   &grsize(4,ngrid,*)
   logical      first   ,writim
!
!     Declaration of local variables
!
   integer      nentri
   parameter   (nentri=14)
   integer      neferr  ,i      ,j     ,k        ,ie       ,ie1  ,&
   &nrerr   ,ind    ,ncelm   ,ncelh  ,nsk      ,nlc  ,&
   &lastcod
   integer      usrord(1)       ,uindex(3)
   character*16 grnamm          ,grnamh          ,grnamd         ,&
   &name
   character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,&
   &nameac(nentri)
   character*64 descel(nentri)
   character*8  txt
   logical      llog  ,new      ,newuit
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
!      1
   &'Sediment transport'       ,'HIS_Sg'     ,'Sg'     ,'m3/s' ,&
!      2
   &'Grain size D10'           ,'HIS_D10'    ,'D10'    ,'m'    ,&
!      2
   &'Grain size D50'           ,'HIS_D50'    ,'D50'    ,'m'    ,&
!      2
   &'Grain size D90'           ,'HIS_D90'    ,'D90'    ,'m'    ,&
!      2
   &'Grain size Dmed'          ,'HIS_Dmed'   ,'Dmed'   ,'m'    ,&
!      2
   &'Grain size D10 ex. layer' ,'HIS_D10(e)' ,'D10(e)' ,'m'    ,&
!      2
   &'Grain size D50 ex. layer' ,'HIS_D50(e)' ,'D50(e)' ,'m'    ,&
!      2
   &'Grain size D90 ex. layer' ,'HIS_D90(e)' ,'D90(e)' ,'m'    ,&
!      2
   &'Grain size Dmed ex. layer','HIS_Dmed(e)','Dmed(e)','m'    ,&
!      2
   &'Grain size D10 un. layer' ,'HIS_D10(u)' ,'D10(e)' ,'m'    ,&
!      2
   &'Grain size D50 un. layer' ,'HIS_D50(u)' ,'D50(e)' ,'m'    ,&
!      2
   &'Grain size D90 un. layer' ,'HIS_D90(u)' ,'D90(e)' ,'m'    ,&
!      2
   &'Grain size Dmed un. layer','HIS_Dmed(u)','Dmed(e)','m'    ,&
!      3
   &'Effective layer thickness','HIS_Elth'   ,'Elth'   ,'m'    /
!
   data  grnamm   ,grnamh ,grnamd/&
   &'GSEDT-MAP-GROUP' ,&
   &'GSEDT-HIS-GROUP' ,&
   &'GSEDT-DES-GROUP' /
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
      call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'GSEDT' ,&
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
         call resdes ('GSEDT', fd_nefis_res, grnamd ,1 ,writim ,&
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
!                Codes > lastcod can only be used in his format
!
!
!                The element names of the Map block start with
!                MAP instead of HIS.
!
               name  = 'MAP'//nameel(ie)(4:)

               if (ie.eq.1) then
!
                  neferr = putrel (fd_nefis_res, grnamm ,name   ,&
                  &uindex  ,usrord ,sedtr  )
                  if (neferr.ne.0) goto 1000
               else if (ie.eq.14) then
!
                  neferr = putrel (fd_nefis_res, grnamm ,name   ,&
                  &uindex  ,usrord ,deff(1,2))
                  if (neferr.ne.0) goto 1000
               else
                  if (ie .gt. 9) then
                     ie1 = ie - 9
                     k   = nlayer+1
                  else if (ie .gt. 5) then
                     ie1 = ie - 5
                     k   = 2
                  else
                     ie1 = ie - 1
                     k   = 1
                  endif
!
                  do 10 j =1,ngrid
                     buf(j) = grsize(ie1,j,k)
10                continue
                  neferr = putrel (fd_nefis_res, grnamm ,name   ,&
                  &uindex  ,usrord ,buf    )
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
   if (new) then
      nsk = 3
   else
      nsk = 0
   endif

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
         call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,&
         &nameac ,neferr )
         if (neferr.ne.0) goto 1000
!
         call resdes ('GSEDT', fd_nefis_res, grnamd ,2 ,writim ,&
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
                  do 30 j=1,sedtim(1)
                     buf(j) = sedtr(sedtim(j+1))
30                continue
               else if (ie.eq.14) then
                  do 35 j=1,sedtim(1)
                     buf(j) = deff(sedtim(j+1),2)
35                continue
               else
                  if (ie .gt. 9) then
                     ie1 = ie - 9
                     k   = nlayer+1
                  else if (ie .gt. 5) then
                     ie1 = ie - 5
                     k   = 2
                  else
                     ie1 = ie - 1
                     k   = 1
                  endif
!
                  do 40 j=1,sedtim(1)
                     ind    = sedtim(j+1)
                     buf(j) = grsize(ie1,ind,k)
40                continue
               endif
!
               neferr = putrel (fd_nefis_res, grnamh ,nameel(ie) ,&
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
      call resdes ('GSEDT', fd_nefis_res, grnamd ,3 ,llog  ,&
      &sedtim ,ncelm  ,ncelh  ,neferr )
      if (neferr.ne.0) goto 1000
!
      neferr = flsdat(fd_nefis_res)
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
   call error (juer ,'GSSER @'//txt//'@' ,nrerr ,ker)
!
end
