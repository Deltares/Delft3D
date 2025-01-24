subroutine GAODSVIEWResults (gaviewnam ,dattimsim ,request)
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Getij Analyse Module
!
! Programmer:         J.Kuipers
!
! Module:             GetijAnalyse ODSVIEW results
!
! Module description: The results of the tidal analyses of one
!                     variable type are written to an Ascii file which
!                     is readable by ODSVIEW.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1  gaviewnam(4)      I  Contains names of files readable by ODSVIEW
! 2  dattimsim(2)      I  start date and time of simulation
!                         (Sobeksim format)
! 3  request           I  type of variable to be printed
!-----------------------------------------------------------------------
! Subprogram calls:
! gaviewdattim        Date and time for ODSVIEW
!=======================================================================
!
!     Declaration of Parameters:
!
   use            gadata
!
   integer        request
   character(len=256) gaviewnam(4)
   integer        dattimsim(2)
!
!     Declaration of local variables:
!
   integer      vartype,ivar  ,iloc  ,ntides ,ind    ,ir   ,&
   &i      ,l1    ,l2
   integer      dattimhis1(6),dattimhis2(6),dattimhis3(6)
   real         tbegin ,tduur ,gem    ,tslag  ,tmin   ,tmax ,&
   &minimum,maximum      ,vloedv ,ebv
   logical      open
   character(len=25) head(6),txt*20
!
   integer,     parameter :: luview = 152

   data  head/'water level             ','discharge                ',&
   &'velocity                ','salt concentration       ',&
   &'salinity                ','chloride concentration   '/
!
   open    = .false.
   ind     = 0
   vartype = garesult(ind+type)
   do while (vartype.gt.0)
      ntides = garesult(ind+ngetij)
      if (vartype.eq.request) then
         if (.not.open) then
            open = .true.
            ir = min(request,4)
            open(luview , file = gaviewnam(ir) )
!              print header
            write (luview,100) head(vartype)
         endif
         ivar   = garesult(ind+grooth)
         iloc   = garesult(ind+locat)
         ind    = ind + 4
         do i=1,ntides
!              nr      = garesult(ind+igetij)
            tbegin  = garesult(ind+begin)
!              tend    = garesult(ind+eind)
            tduur   = garesult(ind+duur)/60.
            gem     = garesult(ind+gemidd)
            tslag   = garesult(ind+slag)
            Tmin    = garesult(ind+mintijd)
            Tmax    = garesult(ind+maxtijd)
            minimum = garesult(ind+minw)
            maximum = garesult(ind+maxw)
            vloedv  = garesult(ind+vloed)
            ebv     = garesult(ind+eb)
            call gaviewdattim (dattimsim,tbegin,dattimhis1)
            call gaviewdattim (dattimsim,tmin  ,dattimhis2)
            call gaviewdattim (dattimsim,tmax  ,dattimhis3)
            write (luview,200) 'Av '//parnam(ivar)(1:17),&
            &locnam(iloc),dattimhis1,gem
            write (luview,200) 'Dur '//parnam(ivar)(1:16),&
            &locnam(iloc),dattimhis1,tduur
            write (luview,200) 'Min '//parnam(ivar)(1:17),&
            &locnam(iloc),dattimhis2,minimum
            write (luview,200) 'Max '//parnam(ivar)(1:17),&
            &locnam(iloc),dattimhis3,maximum
!
            if (vartype.eq.hreeks) then
!                 water level
               write (luview,200) 'D-'//parnam(ivar)(1:18),&
               &locnam(iloc),dattimhis1,tslag
            else if (vartype.eq.qreeks) then
!                 discharge
               l1 = index (parnam(ivar),' ')
               l2 = min(20,20-l1+13)
               txt = 'Flood Volume'//parnam(ivar)(l1:l2)
               write (luview,200) txt,&
               &locnam(iloc),dattimhis2,vloedv
               l2 = min(20,20-l1+11)
               txt = 'Ebb Volume'//parnam(ivar)(l1:l2)
               write (luview,200) txt,&
               &locnam(iloc),dattimhis3,ebv
            else if (vartype.eq.vreeks) then
!                 velocity
               write (luview,200) 'Av Pos '//parnam(ivar)(1:13),&
               &locnam(iloc),dattimhis2,vloedv
               write (luview,200) 'Av Neg '//parnam(ivar)(1:13),&
               &locnam(iloc),dattimhis3,ebv
            else
!                 concentration
               write (luview,200) 'D-'//parnam(ivar)(1:18),&
               &locnam(iloc),dattimhis1,tslag
            endif
            ind = ind + 12
         enddo
      else
         ind = ind + 4 +ntides * 12
      endif
      vartype = garesult(ind+type)
   enddo
   if (open) then
      close (luview)
   endif
!
100 format ('SOBEK'/a25)
200 format (2a20,i4,'/',i2.2,'/',i2.2,';',i2.2,':',i2.2,':',i2.2,&
   &e15.7)
!
end
!
subroutine gaviewdattim (dattimsim,dt,dattimhis)
!
!     Convert time in seconds to printable date and time
!     The time is rounded to minutes
!
   real                  dt
   integer               dattimsim(2),dattimhis(6)
!
   integer               dattimcur(2)
   real                  dtr
   double precision      dtd
!
   dtr   = nint(dt/60.)
   dtd   = nint(dtr*60.)
   dattimcur = dattimsim
   call sotime(dattimcur,dtd)
   call parsdt(dattimcur   ,dattimhis(1) ,dattimhis(2) ,dattimhis(3),&
   &dattimhis(4),dattimhis(5) ,dattimhis(6) )
end
