      subroutine GAODSVIEWResults (gaviewnam ,dattimsim ,request)   
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Getij Analyse Module
c
c Programmer:         J.Kuipers
c
c Module:             GetijAnalyse ODSVIEW results
c
c Module description: The results of the tidal analyses of one 
c                     variable type are written to an Ascii file which
c                     is readable by ODSVIEW.
c                     
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1  gaviewnam(4)      I  Contains names of files readable by ODSVIEW  
c 2  dattimsim(2)      I  start date and time of simulation 
c                         (Sobeksim format) 
c 3  request           I  type of variable to be printed 
c-----------------------------------------------------------------------
c Subprogram calls:
c gaviewdattim        Date and time for ODSVIEW
c=======================================================================
c
c     Declaration of Parameters:
c
      use            gadata
c      
      integer        request 
      character*256  gaviewnam(4)
      integer        dattimsim(2)     
c
c     Declaration of local variables:
c      
      integer      vartype,ivar  ,iloc  ,ntides ,ind    ,ir   ,
     +             i      ,l1    ,l2
      integer      dattimhis1(6),dattimhis2(6),dattimhis3(6)
      real         tbegin ,tduur ,gem    ,tslag  ,tmin   ,tmax ,
     +             minimum,maximum      ,vloedv ,ebv     
      logical      open
      character*25 head(6),txt*20 
c      
      integer,     parameter :: luview = 152

      data  head/'water level             ','discharge                ',
     +           'velocity                ','salt concentration       ',
     +           'salinity                ','chloride concentration   '/
c
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
c              print header 
               write (luview,100) head(vartype)
            endif   
            ivar   = garesult(ind+grooth)
            iloc   = garesult(ind+locat)
            ind    = ind + 4
            do i=1,ntides
c              nr      = garesult(ind+igetij)
               tbegin  = garesult(ind+begin)
c              tend    = garesult(ind+eind)
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
               write (luview,200) 'Av '//parnam(ivar)(1:17),
     +                            locnam(iloc),dattimhis1,gem 
               write (luview,200) 'Dur '//parnam(ivar)(1:16),
     +                            locnam(iloc),dattimhis1,tduur 
               write (luview,200) 'Min '//parnam(ivar)(1:17),
     +                            locnam(iloc),dattimhis2,minimum 
               write (luview,200) 'Max '//parnam(ivar)(1:17),
     +                            locnam(iloc),dattimhis3,maximum 
c                      
               if (vartype.eq.hreeks) then
c                 water level               
                  write (luview,200) 'D-'//parnam(ivar)(1:18),
     +                               locnam(iloc),dattimhis1,tslag 
               else if (vartype.eq.qreeks) then
c                 discharge 
                  l1 = index (parnam(ivar),' ')
                  l2 = min(20,20-l1+13)
                  txt = 'Flood Volume'//parnam(ivar)(l1:l2)
                  write (luview,200) txt,
     +                               locnam(iloc),dattimhis2,vloedv
                  l2 = min(20,20-l1+11)
                  txt = 'Ebb Volume'//parnam(ivar)(l1:l2)
                  write (luview,200) txt,
     +                               locnam(iloc),dattimhis3,ebv 
               else if (vartype.eq.vreeks) then
c                 velocity               
                  write (luview,200) 'Av Pos '//parnam(ivar)(1:13),
     +                               locnam(iloc),dattimhis2,vloedv 
                  write (luview,200) 'Av Neg '//parnam(ivar)(1:13),
     +                               locnam(iloc),dattimhis3,ebv 
               else
c                 concentration
                  write (luview,200) 'D-'//parnam(ivar)(1:18),
     +                               locnam(iloc),dattimhis1,tslag 
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
c      
  100 format ('SOBEK'/a25) 
  200 format (2a20,i4,'/',i2.2,'/',i2.2,';',i2.2,':',i2.2,':',i2.2,
     +        e15.7)
c
      end
c
      subroutine gaviewdattim (dattimsim,dt,dattimhis)
c     
c     Convert time in seconds to printable date and time 
c     The time is rounded to minutes
c
      real                  dt
      integer               dattimsim(2),dattimhis(6)
c
      integer               dattimcur(2)
      real                  dtr
      double precision      dtd
c
      dtr   = nint(dt/60.)
      dtd   = nint(dtr*60.)
      dattimcur = dattimsim
      call sotime(dattimcur,dtd)
      call parsdt(dattimcur   ,dattimhis(1) ,dattimhis(2) ,dattimhis(3),
     +            dattimhis(4),dattimhis(5) ,dattimhis(6) )
      end
