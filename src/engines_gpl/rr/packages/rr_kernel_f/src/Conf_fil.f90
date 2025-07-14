!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

 ! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 25-07-97 10:45a  $
!
! current revision: $Revision:: 6               $
!

MODULE CONF_FIL

  ! *** File arrays voor Delft_3b
  ! *** --------------------------
  ! *** NFIL  = max. aantal files


  use DH_Alloc
  use ReadLib

  implicit none

  PRIVATE     ! alles is default private
              ! alleen de operaties worden publiek toegankelijk gemaakt
  ! public operaties
  PUBLIC  :: confFil,               &
             ConfFil_get_iDebug,    ConfFil_set_iDebug,&
             ConfFil_get_iOut1,     ConfFil_set_iOut1, &
             ConfFil_get_inXFil,    &
             ConfFil_get_namFil,    ConfFil_set_namFil, &
             ConfFil_get_OpenMIExename, Conffil_set_openMIExename, &
             ConfFil_get_OpenMIModelname, Conffil_set_openMIModelname, &
             ConfFil_get_OpenMIReturnname, Conffil_set_openMIReturnname, &
             ConfFil_get_Commandline, Conffil_set_Commandline

  ! public variabelen
  PUBLIC :: nFile, nFil, iFlRtn, VersionNumberBinFile, FilCharIdLength, IdebugLunRR, TableHandle, LanguageHandle

  Integer, parameter :: FilCharIdLength = 128 ! 256
  Integer, parameter :: NFILE = 128
  INTEGER, parameter :: NFIL =  130      !GP: April 2021 NFile at 128 :NetCdf meteo inputfiles (use up to filenr 128)
                                         !GP: Juli 2020 NFile at 127  :LGSI caching file
                                         !GP: Feb 2017 NFile at 126 RR OpenDA ASCII restart
                                         !JZ: Nov 2016 NFile at 125 RR Boundary conditions file for SOBEK3
                                         !GP: Sept 2016 NFILE op 124 voor nieuw NAM
                                         !GP: august 2010 van 120--> 125
                                         !GP: august 1998 met 5 verhoogd;
                                         !GP: March 1999: + 3 voor 2 extra tussentijdse restart files + 1 ready file
                                         !GP: July 99: + 1 linkflows HIS file.
                                         !GP: Feb  00: + 2 ivm Modflow-RR en RR-Modflow HIS files
                                         !GP: April00: + 1 ivm RRWRLM balance file
                                         !GP: April01: + 1 ivm Sacramento files
                                         !GP: Aug  01: + 1 ivm Pluvius.Tbl file
                                         !GP: Oct  01: + 1 ivm RRBALANS.HIS file
                                         !GP: March 2002: ivm nieuwe opzet Fixed Files Crop, Soil, Greenhouse
                                         !GP: Oct 2002 : additional DIO config file
                                         !GP: Dec2002: 2 additional files NWRW-HIS
  Character(9) VersionNumberBinFile

  ! *** Number of input- and output data files


  Character(Len=FilCharIdLength), Pointer, SAVE ::  NAMFIL(:)
  INTEGER, Pointer, SAVE ::       INXFIL(:)

! HarmonIT, command line arguments
  Character (Len=FilCharIdLength) OpenMIExeName, OpenMIModelName, OpenMIReturnFile
  Character (Len=3*FilCharIdLength) CommandLine

  INTEGER  IFLRTN, IOUT1
  Integer  Idebug, iDebugLunRR, TableHandle, LanguageHandle

contains

    SUBROUTINE CONFFIL

      ! variables
      Integer I
      Logical Success

!     IFLRTN= 10

      Success = Dh_AllocInit (Nfil, NAMFIL, ' ')
      Success = success .and. Dh_AllocInit (Nfil, InxFil, 0)
      if (.not. success) stop ' Error in ConfFil Allocating Arrays'

!     Initialise
      Do I=1,NFIL
         INXFIL(I) = 10 + i
      Enddo

      return
    End subroutine ConfFil



    subroutine ConfFil_set_iDebug(fileNr)
      Integer fileNr
      iDebug = fileNr
      return
    end subroutine ConfFil_set_iDebug

    Integer function ConfFil_get_iDebug()
      ConfFil_get_iDebug = iDebug
      return
    end function ConfFil_get_iDebug



    Integer function ConfFil_get_iOut1()
      ConfFil_get_iOut1 = iOut1
      return
    end function ConfFil_get_iOut1

    subroutine ConfFil_set_iOut1(waarde)
      Integer waarde

      iOut1 = waarde
      return
    end subroutine ConfFil_set_iOut1


    Integer function ConfFil_get_inXFil(field)
      Integer field
      ConfFil_get_inXFil = inXFil(field)
    return
    end function ConfFil_get_inXFil


    Character(FilCharIdLength) function ConfFil_get_namFil(field)
      Integer field
      ConfFil_get_namFil= namFil(field)
    return
    end function ConfFil_get_namFil

    subroutine ConfFil_set_namFil(field, name)
      Integer field
      Character(FilCharIdLength) name
      namFil(field) = name
    return
    end subroutine ConfFil_set_namFil


! HarmonIT, command line arguments
    subroutine ConfFil_set_OpenMIExeName(name)
      Character(len=*) name
      OpenMiExeName = name
    return
    end subroutine ConfFil_set_OpenMIExeName

    subroutine ConfFil_set_OpenMIModelName(name)
      Character(len=*) name
      OpenMiModelName = name
    return
    end subroutine ConfFil_set_OpenMIModelName

    subroutine ConfFil_set_OpenMIReturnName(name)
      Character(len=*) name
      OpenMiReturnFile = name
    return
    end subroutine ConfFil_set_OpenMIReturnName

    subroutine ConfFil_get_OpenMIExeName(name)
      Character(FilCharIdLength) name
      name  = OpenMiExeName
    return
    end subroutine ConfFil_get_OpenMIExeName

    subroutine ConfFil_get_OpenMIModelName(name)
      Character(FilCharIdLength) name
      name  = OpenMiModelName
    return
    end subroutine ConfFil_get_OpenMIModelName

    subroutine ConfFil_get_OpenMIReturnName(name)
      Character(FilCharIdLength) name
      name = OpenMiReturnFile
    return
    end subroutine ConfFil_get_OpenMIReturnName

    subroutine ConfFil_set_commandline()
      Character(FilCharIdLength) exename, modelname, returnname
      Integer   length1, length2, length3

      call Conffil_get_OpenMIExename(exename)
      call Conffil_get_OpenMIModelname(modelname)
      call Conffil_get_OpenMIReturnname(returnname)
      length1 = len_trim (exename)
      length2 = len_trim (modelname)
      length3 = len_trim (returnname)
      Commandline = exename(1:length1) // ' ' // modelname(1:length2) // ' ' // returnname(1:length3) // ' ctrl.ini' // Char(0)
    return
    end subroutine ConfFil_set_commandline

    subroutine ConfFil_get_commandline(name)
      Character(len=3*FilCharIdLength) name
      name = CommandLine
    return
    end subroutine ConfFil_get_commandline




END MODULE CONF_FIL
