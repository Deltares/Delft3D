module Cplusplus
    use,intrinsic :: ISO_C_Binding

    interface
        subroutine wrapAddPolySubj(Nel,x,y,absMAXx,absMAXy) bind(C,name="wrapAddPolySubj") ! bind(C,name="wrapperClipper") add name if you wanna retain uppercase letter, otherwise are all made lowercase and C++ is case sensitive
          import :: c_double, c_int
          integer(kind=c_int) Nel
          real(kind=c_double) :: absMAXx,absMAXy
          real(kind=c_double), dimension(*) :: x,y
        end subroutine
    end interface
!
    interface
        subroutine wrapAddPolyClip(Nel,x,y,absMAXx,absMAXy) bind(C,name="wrapAddPolyClip") ! bind(C,name="wrapperClipper") add name if you wanna retain uppercase letter, otherwise are all made lowercase and C++ is case sensitive
          import :: c_double, c_int
          integer(kind=c_int) Nel(1)
          real(kind=c_double) :: absMAXx,absMAXy
          real(kind=c_double), dimension(*) :: x,y
        end subroutine
    end interface
!
    interface
        subroutine wrapClipperRes(oper,POLresX,POLresY,VERTres,absMAXx,absMAXy) bind(C,name="wrapClipperRes") ! bind(C,name="wrapperClipper") add name if you wanna retain uppercase letter, otherwise are all made lowercase and C++ is case sensitive
          import :: c_double, c_int
          integer(kind=c_int) oper
          real(kind=c_double) :: absMAXx,absMAXy
          real(kind=c_double), dimension(100,13) :: POLresX,POLresY
          integer(kind=c_int), dimension(13) :: VERTres
        end subroutine
    end interface 
!
    interface
        subroutine wrapUNIintersNM(Ndry,xclip,yclip,&
                                   POLYStoBEjoinedX,POLYStoBEjoinedY,NPOLYStoBEjoined,VERTtoBEjoined,&
                                   POLYSunionX,POLYSunionY,NPOLYSunion,VERTunion, & 
                                   POLYintersX,POLYintersY,NPOLYSinters,VERTinters,&
                                   absMAXx,absMAXy) bind(C,name="wrapUNIintersNM") ! bind(C,name="wrapperClipper") add name if you wanna retain uppercase letter, otherwise are all made lowercase and C++ is case sensitive
          import :: c_double, c_int
          real(kind=c_double), dimension(5) :: xclip,yclip
          real(kind=c_double), dimension(100,13) :: POLYintersX,POLYintersY,POLYStoBEjoinedX,POLYStoBEjoinedY,&
                                                    POLYSunionX,POLYSunionY 
          real(kind=c_double) :: absMAXx(1),absMAXy(1)
          integer(kind=c_int), dimension(13) :: VERTinters,VERTunion,VERTtoBEjoined
          integer(kind=c_int) :: Ndry,NPOLYStoBEjoined,NPOLYSinters,NPOLYSunion
        end subroutine
    end interface 

    interface
        subroutine wrapUNI_intCEL(Ndry,xclip,yclip,& 
                                 POLYStoBEjoinedX,POLYStoBEjoinedY,NPOLYStoBEjoined,VERTtoBEjoined,&
                                 POLYSunionX,POLYSunionY,NPOLYSunion,VERTunion, & 
                                 POLYintersX,POLYintersY,NPOLYSinters,VERTinters,&
                                 absMAXx,absMAXy) bind(C,name="wrapUNI_intCEL") ! bind(C,name="wrapperClipper") add name if you wanna retain uppercase letter, otherwise are all made lowercase and C++ is case sensitive
          import :: c_double, c_int
          real(kind=c_double), dimension(5) :: xclip,yclip
          real(kind=c_double), dimension(200000000,1) :: POLYintersX,POLYintersY,POLYStoBEjoinedX,POLYStoBEjoinedY,&
                                                    POLYSunionX,POLYSunionY 
          real(kind=c_double) :: absMAXx(1),absMAXy(1)
          integer(kind=c_int), dimension(1) :: VERTinters,VERTunion,VERTtoBEjoined
          integer(kind=c_int) :: Ndry,NPOLYStoBEjoined,NPOLYSinters,NPOLYSunion
        end subroutine
    end interface 

end module Cplusplus