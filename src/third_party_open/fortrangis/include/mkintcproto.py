#!/usr/bin/python
import sys
import re
import textwrap

# usage:
# cpp -Ixx src.c | cproto | ../mkintcproto.py > interface.f90
# no!
# mkintcproto.py gdalproto.c

declre = re.compile(r"^(const )?(.*[ *] ?)([a-zA-Z_][a-zA-Z_0-9]*)\((.*)\) *; *(/\* *out\*/)? *$")
argre = re.compile(r"^(const )?([a-zA-Z_][a-zA-Z_0-9]* ?)(\*\*?)? ?([a-zA-Z_][a-zA-Z_0-9]*)")

enumtypes = {
    'GDALDataType':'int',
    'GDALAsyncStatusType':'int',
    'GDALAccess':'int',
    'GDALRWFlag':'int',
    'GDALColorInterp':'int',
    'GDALPaletteInterp':'int',
    'GDALRATFieldType':'int',
    'GDALRATFieldUsage':'int',
    'CPLErr':'int'
}

dertypes_ptr = {
    'GDALDatasetH':'TYPE(gdaldataseth)',
    'GDALRasterBandH':'TYPE(gdalrasterbandh)',
    'GDALDriverH':'TYPE(gdaldriverh)',
    'GDALColorTableH':'TYPE(gdalcolortableh)',
    'GDALRasterAttributeTableH':'TYPE(gdalrasterattributetableh)',
    'GDALMajorObjectH':'TYPE(gdalmajorobjecth)'
}
dertypes = {
    'GDAL_GCP':'TYPE(gdal_gcp)',
    'GDALRPCInfo':'TYPE(gdalrpcinfo)',
    'GDALColorEntry':'TYPE(gdalcolorentry)',
}

def makearg(carg, valuetag=',VALUE', result=False, ptrisout=False):
    argmatch = argre.search(carg)
    if argmatch is None: return ('unknown1:'+carg,'err')
    gr = argmatch.groups()
    cdef = gr[1].strip()
    name = gr[3].lower()
    comm = ''
    intent = ''
    arr = ''
    if enumtypes.has_key(cdef):
        comm = ' ! '+cdef
        cdef = enumtypes[cdef]
    if (cdef == 'void' or result) and gr[2] == '*':
        refval = valuetag
        type = 'TYPE(c_ptr)'
        comm = ' ! '+cdef+'*'
    else:
        if gr[2] == '*':
            refval = ''
            if ptrisout:
                arr = ''
                intent = ',INTENT(inout)'
            else:
                arr = '(*)'
        elif gr[2] == '**':
            cdef = 'c_ptr_ptr'
            comm = ' ! TYPE(c_ptr_ptr)'
            refval = ''
        else:
            refval = valuetag
        if cdef == 'char':
            type = 'CHARACTER(kind=c_char)'
        elif cdef == 'int':
            type = 'INTEGER(kind=c_int)'
        elif cdef in ('float', 'double'):
            type = 'REAL(kind=c_%s)' % (cdef,)
        else:
            if dertypes.has_key(cdef):
                type = dertypes[cdef]
            elif dertypes_ptr.has_key(cdef):
                type = dertypes_ptr[cdef]
                refval = valuetag
            else:
                type = 'TYPE(c_ptr)'
                refval = valuetag
            arr = ''
#            else:
#                return 'unknown2:'+gr[1]+':'+carg
    if gr[0] is not None and refval == '' and not result:
        intent = ',INTENT(in)'
    return (type+refval+intent+' :: '+name+arr+comm, name)

ic = open(sys.argv[1])
dot = sys.argv[1].rfind('.')
if dot == -1: dot=len(sys.argv[1])
otype = open(sys.argv[1][:dot]+'_type.f90', "w")
ointerf = open(sys.argv[1][:dot]+'_interf.f90', "w")
odoxy = open(sys.argv[1][:dot]+'_doxy.f90', "w")
oproc = open(sys.argv[1][:dot]+'_proc.f90', "w")

for ctype,ftype in dertypes_ptr.iteritems():
    otype.write("""TYPE,BIND(C) :: %s
  PRIVATE
  TYPE(c_ptr) :: ptr = C_NULL_PTR
END TYPE %s

""" % (ctype, ctype))

otype.write("INTERFACE gdalassociated\n")
for ctype,ftype in dertypes_ptr.iteritems():
    otype.write("  MODULE PROCEDURE gdalassociated_%s\n" % (ctype.lower()))
otype.write("END INTERFACE\n")

otype.write("INTERFACE gdalnullify\n")
for ctype,ftype in dertypes_ptr.iteritems():
    otype.write("  MODULE PROCEDURE gdalnullify_%s\n" % (ctype.lower()))
otype.write("END INTERFACE\n")

# otype.write("INTERFACE gdalget_c_ptr\n")
# for ctype,ftype in dertypes_ptr.iteritems():
#     otype.write("  MODULE PROCEDURE gdalget_c_ptr_%s\n" % (ctype.lower()))
# otype.write("END INTERFACE\n")

for ctype,ftype in dertypes_ptr.iteritems():

    oproc.write("""FUNCTION gdalassociated_%s(arg1, arg2) RESULT(associated_)
%s,INTENT(in) :: arg1
%s,INTENT(in),OPTIONAL :: arg2
LOGICAL :: associated_
IF(PRESENT(arg2)) THEN
  associated_ = C_ASSOCIATED(arg1%%ptr, arg2%%ptr)
ELSE
  associated_ = C_ASSOCIATED(arg1%%ptr)
ENDIF
END FUNCTION gdalassociated_%s


SUBROUTINE gdalnullify_%s(arg1)
%s,INTENT(inout) :: arg1
arg1%%ptr = C_NULL_PTR
END SUBROUTINE gdalnullify_%s
""" % (ctype.lower(), ftype, ftype, ctype.lower(), ctype.lower(), ftype, ctype.lower()))

for line in ic.readlines():
    declmatch = declre.search(line)
    if declmatch is not None:
        gr = declmatch.groups()
        type = gr[1].strip()
        name = gr[2]
        if gr[3].strip() == '':
            args = ()
        else:
            args = tuple(arg.strip() for arg in gr[3].split(','))
        ointerf.write("INTERFACE\n")
        if type == 'void': 
            proc = 'SUBROUTINE'
            proctype = ''
        else:
            if gr[0] == 'const ':
                type = gr[0]+type
            proc = 'FUNCTION'
            proctype, dummy = makearg(type+' '+name, valuetag='', result=True)
        arglist = ''
        argdecllist = []
        ptrisout = False
# found ad hoc "out" comment indicating intent((in)out) rather than array
        if gr[4] is not None:
            ptrisout = len(gr[4]) > 0
        for arg in args:
            fdecl, fname = makearg(arg, ptrisout=ptrisout)
            argdecllist.append(fdecl)
            if arglist == '': arglist = fname
            else: arglist = arglist+', '+fname


        ointerf.write(" &\n".join(textwrap.wrap( \
            ("%s %s(%s) BIND(C,name='%s')") % \
            (proc, name.lower(), arglist, name),\
                width=127, initial_indent="  ", subsequent_indent="   ")))
        ointerf.write("\n  IMPORT\n")
# for interfacing to win32 libs with gcc/gfortran
# double commented because of a bug with gfortran 5>
        ointerf.write("!!GCC$ ATTRIBUTES STDCALL :: %s\n" % (name,))
        odoxy.write("!!  - %s() -> %s %s()\n" % (name, proc, name.lower()))

        for line in argdecllist: ointerf.write("  "+line+"\n")

        if proctype != '':
                ointerf.write("  "+proctype+"\n")
        ointerf.write("""  END %s %s
END INTERFACE

""" % (proc,name.lower()))
