// (C) 2008 D. V. Wiebe
//
//#########################################################################
//
// This file is part of the GetData project.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// GetData is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GetData; if not, write to the Free Software Foundation,
// Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
//

#ifndef GETDATA_ENTRY_H
#define GETDATA_ENTRY_H

#define NO_GETDATA_LEGACY_API

extern "C" {
#include <getdata.h>
}

namespace GetData {

  enum DataType {
    Null    = GD_NULL,    Unknown = GD_UNKNOWN,
    UInt8   = GD_UINT8,   Int8    = GD_INT8,
    UInt16  = GD_UINT16,  Int16   = GD_INT16,
    UInt32  = GD_UINT32,  Int32   = GD_INT32,
    UInt64  = GD_UINT64,  Int64   = GD_INT64,
    Float32 = GD_FLOAT32, Float64 = GD_FLOAT64
  };

  enum EntryType {
    NoEntry       = GD_NO_ENTRY,
    RawEntry      = GD_RAW_ENTRY,
    LincomEntry   = GD_LINCOM_ENTRY,
    LinterpEntry  = GD_LINTERP_ENTRY,
    BitEntry      = GD_BIT_ENTRY,
    MultiplyEntry = GD_MULTIPLY_ENTRY,
    PhaseEntry    = GD_PHASE_ENTRY
  };

  class Entry {
    friend class Dirfile;

    public:
      Entry();

      ~Entry();

      /* Generic data */
      const char *Code() { return E.field; };

      EntryType Type() { return (EntryType)E.field_type; };

      /* Specific data */
      const char *Input(int index = 0) {
        return (CheckIndex(E.field_type, E.n_fields, index)) ? 
            E.in_fields[index] : NULL;
      };

      /* RAW methods */
      const char *File() {
        return (E.field_type == GD_RAW_ENTRY) ? E.file : NULL;
      };

      unsigned int SamplesPerFrame() {
        return (E.field_type == GD_RAW_ENTRY) ? E.spf : 0;
      };

      DataType RawType() {
        return (E.field_type == GD_RAW_ENTRY) ? (DataType)E.data_type : Unknown;
      };

      /* LINCOM methods */
      int NFields() {
        return (E.field_type == GD_LINCOM_ENTRY) ? E.n_fields : 0;
      };

      double Scale(int index = 0) {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.n_fields, index)) ?  E.m[index] : NULL;
      }

      double Offset(int index = 0) {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.n_fields, index)) ?  E.b[index] : NULL;
      }

      /* LINTERP methods */
      const char *Table() {
        return (E.field_type == GD_LINTERP_ENTRY) ? E.table : NULL;
      };

      /* BIT methods */
      int FirstBit() {
        return (E.field_type == GD_BIT_ENTRY) ? E.bitnum : -1;
      };

      int NumBits() {
        return (E.field_type == GD_BIT_ENTRY) ? E.numbits : -1;
      };

      /* PHASE methods */
      int Shift() {
        return (E.field_type == GD_PHASE_ENTRY) ? E.shift : 0;
      };

    protected:
      static int CheckIndex(gd_entype_t field_type, int n_fields, int index);
      gd_entry_t E;

    private:
      Entry(DIRFILE *dirfile, const char* field_code);
  };
};

#endif
