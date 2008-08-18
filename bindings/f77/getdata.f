C     (C) 2008 D. V. Wiebe
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This file is part of the GetData project.
C
C     This program is free software; you can redistribute it and/or
C     modify it under the terms of the GNU General Public License as
C     published by the Free Software Foundation; either version 2 of the
C     License, or (at your option) any later version.
C
C     GetData is distributed in the hope that it will be useful, but
C     WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C     General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with GetData; if not, write to the Free Software Foundation,
C     Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
C

C     Fortran 77 parameters for getdata.  This file defines handy
C     constants useful to Fortran programs

C     Error codes
C     Corresponding to GD_E_OK
      INTEGER GD_EOK
      PARAMETER (GD_EOK=0)
C     Corresponding to GD_E_OPEN
      INTEGER GD_EOP
      PARAMETER (GD_EOP=1)
C     Corresponding to GD_E_FORMAT
      INTEGER GD_EFO
      PARAMETER (GD_EFO=2)
C     Corresponding to GD_E_TRUNC
      INTEGER GD_ETR
      PARAMETER (GD_ETR=3)
C     Corresponding to GD_E_CREAT
      INTEGER GD_ECR
      PARAMETER (GD_ECR=4)
C     Corresponding to GD_E_BAD_CODE
      INTEGER GD_EBC
      PARAMETER (GD_EBC=5)
C     Corresponding to GD_E_BAD_TYPE
      INTEGER GD_EBT
      PARAMETER (GD_EBT=6)
C     Corresponding to GD_E_RAW_IO
      INTEGER GD_ERW
      PARAMETER (GD_ERW=7)
C     Corresponding to GD_E_OPEN_INCLUDE
      INTEGER GD_EOI
      PARAMETER (GD_EOI=8)
C     Corresponding to GD_E_INTERNAL_ERROR
      INTEGER GD_EIE
      PARAMETER (GD_EIE=9)
C     Corresponding to GD_E_EMPTY
      INTEGER GD_EEM
      PARAMETER (GD_EEM=10)
C     Corresponding to GD_E_ALLOC
      INTEGER GD_EAL
      PARAMETER (GD_EAL=11)
C     Corresponding to GD_E_RANGE
      INTEGER GD_ERA
      PARAMETER (GD_ERA=12)
C     Corresponding to GD_E_OPEN_LINFILE
      INTEGER GD_EOL
      PARAMETER (GD_EOL=13)
C     Corresponding to GD_E_RECURSE_LEVEL
      INTEGER GD_ERL
      PARAMETER (GD_ERL=14)
C     Corresponding to GD_E_BAD_DIRFILE
      INTEGER GD_EBD
      PARAMETER (GD_EBD=15)
C     Corresponding to GD_E_BAD_PUT_FIELD
      INTEGER GD_EBP
      PARAMETER (GD_EBP=16)
C     Corresponding to GD_E_ACCMODE
      INTEGER GD_EAC
      PARAMETER (GD_EAC=17)
C Open flags
C     Correponding to GD_RDONLY
      INTEGER GD_RO
      PARAMETER (GD_RO=0)
C     Corresponding to GD_RDWR
      INTEGER GD_RW
      PARAMETER (GD_RW=2)
      INTEGER GD_CR
C     Corresponding to GD_CREAT
      PARAMETER (GD_CR=64)
      INTEGER GD_EX
C     Corresponding to GD_EXCL
      PARAMETER (GD_EX=128)
      INTEGER GD_TR
C     Corresponding to GD_TRUNC
      PARAMETER (GD_TR=512)
      INTEGER GD_BE
C     Corresponding to GD_BIG_ENDIAN
      PARAMETER (GD_BE=65536)
      INTEGER GD_LE
C     Corresponding to GD_LITTLE_ENDIAN
      PARAMETER (GD_LE=131072)
      INTEGER GD_FE
C     Corresponding to GD_FORCE_ENDIAN
      PARAMETER (GD_FE=262144)
      INTEGER GD_PE
C     Corresponding to GD_PEDANTIC
      PARAMETER (GD_PE=1048576)
C Data types -- F77 can't handle unsigned types, so we skip them
C     Corresponding to GD_NLL
      INTEGER GD_NLL
      PARAMETER (GD_NLL=0)
C     Corresponding to GD_INT8
      INTEGER GD_I8
      PARAMETER (GD_I8=17)
C     Corresponding to GD_INT16
      INTEGER GD_I16
      PARAMETER (GD_I16=18)
C     Corresponding to GD_INT32
      INTEGER GD_I32
      PARAMETER (GD_I32=20)
C     Corresponding to GD_INT64
      INTEGER GD_I64
      PARAMETER (GD_I64=24)
C     Corresponding to GD_FLOAT32
      INTEGER GD_F32
      PARAMETER (GD_F32=36)
C     Corresponding to GD_FLOAT64
      INTEGER GD_F64
      PARAMETER (GD_F64=40)
