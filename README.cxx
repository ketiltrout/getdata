C++ BINDINGS FOR GETDATA
========================

This README describes the C++ bindings for the GetData library.  These bindings
consist of a several C++ classes in the `GetData' namespace.  Header files
defining these classes are installed into the ${includedir}/getdata directory.
The following classes are available:

DIRFILE CLASS
=============

Defined in getdata/dirfile.h, the Getdata::Dirfile class encapsulates the
DIRFILE object, providing a thin wrapper to the C API.  The following methods
are available:

* Dirfile::Dirfile(const char *dirfilename, unsigned int flags = GD_RDWR)

  The constructor takes the name of the dirfile and the dirfile flags and will
  call dirfile_open(3) on the provided path name.  If flags is omitted, the
  default GD_RDWR will be used.

* ~Dirfile::Dirfile()

  The destructor will take care of calling dirfile_close(3).

* int Dirfile::Error()

  The Error method provides access to the error member of the underlying
  DIRFILE* object.

* const char *Dirfile::ErrorString(size_t len = 4096)

  The ErrorString method will return a buffer containing a description of the
  last GetData library error as obtained from get_error_string(3).  This
  buffer is local to the object, and subsequent calls to ErrorString() will
  overwrite the buffer.  The string written to the buffer will be at most
  len characters long, up to a maximum of 4096 characters.

* GetData::Entry *Dirfile::Entry(const char *field_code)

  This method will return a pointer to a newly allocated GetData::Entry object
  created after calling get_entry with the supplied field_code.  See below for a
  description of the Entry class.

* int Dirfile::Add(const Entry &entry)
* int Dirfile::AddMeta(const Entry &entry, const char *parent)
* int Dirfile::AddMetaSpec(const char *spec, const char *parent)
* int Dirfile::AddSpec(const char *spec, int format_file = 0)
* const void *Dirfile::ConstantValues(GetData::DataType type = Float64)
* const char **Dirfile::FieldList()
* const char **Dirfile::FieldListByType(GetData::EntryType type)
* int Dirfile::Flush(const char *field_code = NULL)
* int Dirfile::FlushMetaData()
* const char *Dirfile::FormatFilename(int index)
* size_t Dirfile::GetConstant(const char *field_code, GetData::DataType type,
    void *data_out)
* size_t Dirfile::GetData(const char *field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    GetData::DataType type, void *data_out)
* size_t Dirfile::GetString(const char *field_code, size_t len, char *data_out)
* int Include(const char *file, int format_file, unsigned int flags)
* const void *Dirfile::MetaConstantValues(const char *parent,
    GetData::DataType type)
* const char **Dirfile::MetaFieldList(const char *parent)
* const char **Dirfile::MetaFieldListByType(const char *parent,
    GetData::EntryType type)
* const char **Dirfile::MetaStringValues(const char *parent)
* const char **Dirfile::MetaVectorList(const char *parent)
* unsigned int Dirfile::NFields()
* unsigned int Dirfile::NFieldsByType(GetData::EntryType type)
* off_t Dirfile::NFrames()
* int Dirfile::NFormats()
* unsigned int Dirfile::NMetaFieldsByType(const char *parent,
    GetData::EntryType type)
* unsigned int Dirfile::NMetaVectors(const char *parent)
* int Dirfile::NVectors()
* size_t Dirfile::PutConstant(const char *field_code, GetData::DataType type,
    const void *data_in)
* size_t Dirfile::PutData(const char *field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    GetData::DataType type, const void *data_in)
* size_t Dirfile::PutString(const char *field_code, const char *data_in)
* unsigned int Dirfile::SamplesPerFrame(const char *field_code)
* const char **Dirfile::StringValues()
* const char **Dirfile::VectorList()

  These methods call the corresponding function from the C API on the C DIRFILE
  object associated with the C++ object.  Arguments of type GetData::DataType
  should be one of:

    Null, Unknown, UInt8, Int8, UInt16, Int16, UInt32, Int32, UInt64, Int64,
    Float32, Float64

  which are aliases for the gd_type_t values GD_NULL, GD_UNKNOWN, GD_UINT8, &c.
  Arguments of type GetData::EntryType should be one of 

    NoEntryType, RawEntryType, LincomEntryType, LinterpEntryType, BitEntryType,
    MultiplyEntryType, PhaseEntryType, ConstEntryType, StringEntryType,
    IndexEntryType

  which are aliases for the gd_entype_t values GD_NO_ENTRY, GD_RAW_ENTRY, &c.
  Note that the arguments to AddSpec are opposite of the corresponding function
  in add_spec.

ENTRY CLASS
===========

Defined in getdata/entry.h, the GetData::Entry class encapsulates the gd_entry_t
object.  The following methods are available:

* Entry::Entry()

  This will create an empty gd_entry_t object.

* Entry::~Entry()

  This will take care of de-allcoating the gd_entry_t object and its allocated
  strings.

* EntryType Type()

  This will return the field type of the Entry's field.  This will be one of:

    NoEntry, RawEntry, LincomEntry, LinterpEntry, BitEntry, MultiplyEntry,
    PhaseEntry

* const char *Code()

  This method returns the name of the field.  

* int FormatFile()
* unsigned int SamplesPerFrame()
* DataType RawType()
* int NFields()
* int FirstBit()
* int NumBits()
* int Shift()
* const char *Table()

  These methods will return the corresponding member of the gd_entry_t object.
  Only methods reasonable to be queried for the given field type will return
  meaningful results.

* const char *Input(int index = 0)
* double Scale(int index = 0)
* double Offset(int index = 0)

  These methods will return an element from  the gd_entry_t members in_fields[],
  m[], and b[], indexed by the supplied parameter.  Attempts to access elements
  out of range for the field that the Entry class describes will not return
  meaningful results.

ENTRY CHILD CLASSES
===================

The following classes are provided to create Entry objects of the corresponding
field type.

* RawEntry::RawEntry(const char *field_code, DataType data_type,
    unsigned int spf, int format_file = 0)

* LincomEntry::LincomEntry(const char *field_code, int n_fields,
    const char **in_fields, double *m, double *b, int format_file = 0)

* LinterpEntry::LinterpEntry(const char *field_code, const char *in_field,
    const char *table, int format_file = 0)

* BitEntry::BitEntry(const char *field_code, const char *in_field, int bitnum,
    int numbits = 1, int format_file = 0)

* MultiplyEntry::MultiplyEntry(const char *field_code, const char *in_field1,
    const char *in_field2, int format_file = 0)

* PhaseEntry::PhaseEntry(const char *field_code, const char *in_field,
    int shift, int format_file = 0)
