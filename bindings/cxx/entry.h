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
// Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//

#ifndef GETDATA_ENTRY_H
#define GETDATA_ENTRY_H

#define NO_GETDATA_LEGACY_API

extern "C" {
#include <getdata.h>
}

#define __gd_unused __attribute__ (( unused ))

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
    NoEntryType       = GD_NO_ENTRY,
    RawEntryType      = GD_RAW_ENTRY,
    LincomEntryType   = GD_LINCOM_ENTRY,
    LinterpEntryType  = GD_LINTERP_ENTRY,
    BitEntryType      = GD_BIT_ENTRY,
    MultiplyEntryType = GD_MULTIPLY_ENTRY,
    PhaseEntryType    = GD_PHASE_ENTRY,
    ConstEntryType    = GD_CONST_ENTRY,
    StringEntryType   = GD_STRING_ENTRY,
    IndexEntryType    = GD_INDEX_ENTRY
  };

  class Entry {
    friend class Dirfile;

    public:
      Entry();

      ~Entry();

      /* Generic data */
      const char *Code() { return E.field; };

      EntryType Type() { return (EntryType)E.field_type; };
      
      int FragmentIndex() { return E.fragment_index; };

      /* Specific data */
      virtual const char *Input(int index = 0) {
        return (CheckIndex(E.field_type, E.n_fields, index)) ? 
            E.in_fields[index] : NULL;
      };

      /* RAW methods */
      virtual unsigned int SamplesPerFrame() {
        return (E.field_type == GD_RAW_ENTRY) ? E.spf : 0;
      };

      virtual DataType RawType() {
        return (E.field_type == GD_RAW_ENTRY) ? (DataType)E.data_type : Unknown;
      };

      /* LINCOM methods */
      virtual int NFields() {
        return (E.field_type == GD_LINCOM_ENTRY) ? E.n_fields : 0;
      };

      virtual double Scale(int index = 0) {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.n_fields, index)) ? E.m[index] : 0;
      }

      virtual double Offset(int index = 0) {
        return (E.field_type == GD_LINCOM_ENTRY &&
            CheckIndex(E.field_type, E.n_fields, index)) ? E.b[index] : 0;
      }

      /* LINTERP methods */
      virtual const char *Table() {
        return (E.field_type == GD_LINTERP_ENTRY) ? E.table : NULL;
      };

      /* BIT methods */
      virtual int FirstBit() {
        return (E.field_type == GD_BIT_ENTRY) ? E.bitnum : -1;
      };

      virtual int NumBits() {
        return (E.field_type == GD_BIT_ENTRY) ? E.numbits : -1;
      };

      /* PHASE methods */
      virtual int Shift() {
        return (E.field_type == GD_PHASE_ENTRY) ? E.shift : 0;
      };

    protected:
      static int CheckIndex(gd_entype_t field_type, int n_fields, int index);
      gd_entry_t E;

    private:
      Entry(DIRFILE *dirfile, const char* field_code);
  };

  class RawEntry : public Entry {
    public:
      RawEntry(const char* field_code, DataType data_type, unsigned int spf,
          int fragment_index = 0);

      virtual unsigned int SamplesPerFrame() {
        return E.spf;
      };

      virtual DataType RawType() {
        return (DataType)E.data_type;
      };
  };

  class LincomEntry : public Entry {
    public:
      LincomEntry(const char* field_code, int n_fields, const char** in_fields,
          double* m, double* b, int fragment_index = 0);

      virtual const char *Input(int index = 0) {
        return (CheckIndex(E.field_type, E.n_fields, index)) ? 
            E.in_fields[index] : NULL;
      };

      virtual int NFields() {
        return E.n_fields;
      };

      virtual double Scale(int index = 0) {
        return (CheckIndex(E.field_type, E.n_fields, index)) ? E.m[index] : 0;
      };

      virtual double Offset(int index = 0) {
        return (CheckIndex(E.field_type, E.n_fields, index)) ? E.b[index] : 0;
      };
  };

  class LinterpEntry : public Entry {
    public:
      LinterpEntry(const char* field_code, const char* in_field,
          const char* table, int fragment_index = 0);

      virtual const char *Input(int __gd_unused index = 0) {
        return E.in_fields[0];
      };

      virtual const char *Table() {
        return E.table;
      };
  };

  class BitEntry : public Entry {
    public:
      BitEntry(const char* field_code, const char* in_field, int bitnum,
          int numbits = 1, int fragment_index = 0);

      virtual const char *Input(int __gd_unused index = 0) {
        return E.in_fields[0];
      };

      virtual int FirstBit() {
        return E.bitnum;
      };

      virtual int NumBits() {
        return E.numbits;
      };
  };

  class MultiplyEntry : public Entry {
    public:
      MultiplyEntry(const char* field_code, const char* in_field1,
          const char* in_field2, int fragment_index = 0);

      virtual const char *Input(int index = 0) {
        return E.in_fields[(index == 0) ? 0 : 1];
      };
  };

  class PhaseEntry : public Entry {
    public:
      PhaseEntry(const char* field_code, const char* in_field, int shift,
          int fragment_index = 0);

      virtual const char *Input(int __gd_unused index = 0) {
        return E.in_fields[0];
      };

      virtual int Shift() {
        return E.shift;
      };
  };

}

#endif
