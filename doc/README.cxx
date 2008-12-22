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

* Dirfile::Dirfile(const char *dirfilename, unsigned int flags = GD_RDWR,
    int (*sehandler)(const DIRFILE*, int, char*) = NULL)

  The constructor takes the name of the dirfile, the dirfile flags, and an
  optional pointer to a syntax error callback handler.  The constructor will 
  call dirfile_cbopen(3) on the provided path name.  If flags is omitted, the
  default GD_RDWR will be used.

* Dirfile::Dirfile(DIRFILE *dirfile)
  
  This constructor creates a Dirfile object from the supplied C API's DIRFILE
  object.  This may be used especially in the supplied parser callback,
  sehandler, to instantiate a Dirfile object from the supplied DIRFILE pointer.

* ~Dirfile::Dirfile()

  If not done explicitly (see below), the destructor will take care of calling
  dirfile_close(3).

* int Dirfile::Close()
* int Dirfile::Discard()
  
  These call dirfile_close(3) and dirfile_discard(3) respectively.  If they
  return successfully (return value zero), the Dirfile should immediately be
  destroyed, by calling its destructor.

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

  This method will return a pointer to a newly allocated object of the
  appropriate Entry Child class, cast as a plain GetData::Entry, created after
  calling get_entry(3) with the supplied field_code.  See below for a
  description of the Entry classes.

* GetData::Fragment *Dirfile::Fragment(int index)
  
  This method will return a pointer to a newly allocated GetData::Fragment
  object corresponding to the fragment with the specified index.

* void Dirfile::SetCallback(int (*sehandler)(const DIRFILE*, int, char*))
  
  This method will call dirfile_parser_callback(3) to change or remove the
  parser callback function.

* GetData::RawEntry *Dirfile::Reference(const char *field_code = NULL)
  
  This method will call dirfile_reference to set and/or retrieve the
  reference field.  It returns a RawEntry object describing the reference field.

* const char* Dirfile::ReferenceFilename()

  This method is equivalent to calling Dirfile::Reference()->FileName() to
  return the binary file pathname associated with the dirfile reference field,
  except that it properly check that Dirfile::Reference() hasn't returned NULL,
  and it does not create a RawEntry object.

* int Dirfile::UnInclude(int fragment_index, int del = 0);

  This method will call dirfile_uninclude(3) to remove the indicated fragment
  from the dirfile.  Because dirfile_uninclude may re-arrange the order of
  fragments in the dirfile, the caller should destroy any GetData::Fragment
  objects it has retained.

* int Dirfile::Add(const Entry &entry)
* int Dirfile::AddSpec(const char *spec, int format_file = 0)
* int Dirfile::AlterSpec(const char *line, int recode = 0)
* const void *Dirfile::Constants(GetData::DataType type = Float64)
* int Delete(const char* field_code, int flags = 0)
* const char **Dirfile::FieldList()
* const char **Dirfile::FieldListByType(GetData::EntryType type)
* int Dirfile::Flush(const char *field_code = NULL)
* const char *Dirfile::FormatFilename(int index)
* size_t Dirfile::GetConstant(const char *field_code, GetData::DataType type,
    void *data_out)
* size_t Dirfile::GetData(const char *field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    GetData::DataType type, void *data_out)
* size_t Dirfile::GetString(const char *field_code, size_t len, char *data_out)
* int Include(const char *file, int format_file, unsigned int flags)
* int Dirfile::MAdd(const Entry &entry, const char *parent)
* int Dirfile::MAddSpec(const char *spec, const char *parent)
* int Dirfile::MAlterSpec(const char *line, const char *parent)
* const void *Dirfile::MConstants(const char *parent, GetData::DataType type)
* const char **Dirfile::MFieldList(const char *parent)
* const char **Dirfile::MFieldListByType(const char *parent,
    * GetData::EntryType type)
* int Dirfile::MetaFlush()
* const char **Dirfile::MStrings(const char *parent)
* const char **Dirfile::MVectorList(const char *parent)
* unsigned int Dirfile::NFields()
* unsigned int Dirfile::NFieldsByType(GetData::EntryType type)
* off_t Dirfile::NFrames()
* int Dirfile::NFormats()
* unsigned int Dirfile::NMFields(const char *parent)
* unsigned int Dirfile::NMFieldsByType(const char *parent,
    GetData::EntryType type)
* unsigned int Dirfile::NMVectors(const char *parent)
* int Dirfile::NVectors()
* size_t Dirfile::PutConstant(const char *field_code, GetData::DataType type,
    const void *data_in)
* size_t Dirfile::PutData(const char *field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    GetData::DataType type, const void *data_in)
* size_t Dirfile::PutString(const char *field_code, const char *data_in)
* unsigned int Dirfile::SamplesPerFrame(const char *field_code)
* const char **Dirfile::Strings()
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


FRAGMENT CLASS
==============

Define in getdata/fragment.h, the GetData::Fragment class provides information
about an individual fragment in a dirfile.  This class has no public
constructor, but may be created by calling Dirfile::Fragment.

Note: The Fragment class caches the format file index for the associated
fragment.  As a result, if Dirfile::UnInclude is called, these indicies will
be incorrect, and all pre-existing Fragment objects should be destroyed.

* GetData::EncodingScheme Fragment::Encoding()
* unsigned long Fragment::Endianness()
* off_t Fragment::FrameOffset()
* const char* Fragment::Name()
* int Fragment::Parent()
* int Fragment::Protection()

  These methods return the specified information on the associated fragment.
  Variables of type EncodingScheme will be one of
  
    AutoEncoding, RawEncoding, TextEncoding, SlimEncoding, GzipEncoding,
    Bzip2Encoding, UnsupportedEncoding

  which are aliases for GD_AUTO_ENCODED, GD_UNENCODED, GD_TEXT_ENCODED, &c.

* int SetEncoding(EncodingScheme encoding, int recode = 0);
* int SetEndianness(unsigned long byte_sex, int recode = 0);
* int SetFrameOffset(off_t offset, int recode = 0);
* int SetProtection(int protection_level);

  These methods set the specified information on the associated fragment by
  calling dirfile_alter_encoding(3), dirfile_alter_endianness(3),
  dirfile_alter_frameoffset(3), or dirfile_protect(3) as appropriate.


ENTRY CLASS
===========

Defined in getdata/entry.h, the GetData::Entry class encapsulates the gd_entry_t
object.  The following methods are available:

* Entry::Entry()

  This will create an empty gd_entry_t object.

* Entry::~Entry()

  This will take care of de-allocating the gd_entry_t object and its allocated
  strings.

* EntryType Type()

  This will return the field type of the Entry's field.  This will be one of:

  NoEntry, RawEntry, LincomEntry, LinterpEntry, BitEntry, MultiplyEntry,
  PhaseEntry

* const char *Code()

  This method returns the name of the field.  

* int Move(int new_fragment, int move_data = 0)

  This will call dirfile_move(3) to move the field to a different fragment.

* int Rename(const char* new_name, int move_data = 0);

  This will change the name of the field associated with the Entry object by
  calling dirfile_rename(3).

* int FragmentIndex()
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


RawEntry Class
--------------

Defined in getdata/rawentry.h

* RawEntry::RawEntry(const char *field_code, DataType data_type,
    unsigned int spf, int format_file = 0)

* const char* RawEntry::FileName()

  This calls get_raw_filename(3) and returns the pathname of the binary file
  associated with the RAW field.

* int RawEntry::SetSamplesPerFrame(unsigned int spf, int recode = 0)
* int RawEntry::SetType(DataType type, int recode = 0)

  These methods will change the specified field parameter by calling
  dirfile_alter_raw(3).  If recode is non-zero, the binary file will also
  be translated.


LincomEntry Class
-----------------

Defined in getdata/lincomentry.h

* LincomEntry::LincomEntry(const char *field_code, int n_fields,
    const char **in_fields, double *m, double *b, int format_file = 0)

* int LincomEntry::SetInput(const char* field, int index = 0)
* int LincomEntry::SetScale(double scale, int index = 0)
* int LincomEntry::SetOffset(double offset, int index = 0)

  These functions will change the specified field parameter associated with the
  input field with the given index, which should be between zero and two.

* int LincomEntry::SetNFields(int nfields)

  This will set the number of input fields for the LINCOM.  If this is
  greater than its previous value, the Set methods above should be used
  to initialise the data.


LinterpEntry Class
------------------

Defined in getdata/linterpentry.h

* LinterpEntry::LinterpEntry(const char *field_code, const char *in_field,
    const char *table, int format_file = 0)

* int SetInput(const char* field)
* int SetTable(const char* table, int move_table)

  These methods will change the specified field parameter by calling
  dirfile_alter_raw(3).  If move_table is non-zero, the existing look-up table
  will be renamed to account for the change in name.


BitEntry Class
--------------

Defined in getdata/bitentry.h

* BitEntry::BitEntry(const char *field_code, const char *in_field, int bitnum,
    int numbits = 1, int format_file = 0)

* int SetInput(const char* field)
* int SetFirstBit(int first_bit)
* int SetNumBits(int num_bits)

  These methods will change the specified field parameter by calling
  dirfile_alter_raw(3).


MultiplyEntry Class
-------------------

Defined in getdata/multiplyentry.h

* MultiplyEntry::MultiplyEntry(const char *field_code, const char *in_field1,
    const char *in_field2, int format_file = 0)

* int LincomEntry::SetInput(const char* field, int index = 0)

  These functions will change the specified input field with the given index,
  which should be between zero or one.


PhaseEntry Class
----------------

Defined in getdata/phaseentry.h

* PhaseEntry::PhaseEntry(const char *field_code, const char *in_field,
    int shift, int format_file = 0)

* int SetInput(const char* field)
* int SetShift(int shift)

  These methods will change the specified field parameter by calling
  dirfile_alter_raw(3).


ConstEntry Class
----------------

Defined in getdata/constentry.h

* ConstEntry::ConstEntry(const char *field_code, DataType type,
    int format_file = 0)

* int SetType(DataType field)

  This method will change the data type of the CONST field.


StringEntry Class
-----------------

Defined in getdata/stringentry.h

* StringEntry::StringEntry(const char *field_code, int format_file = 0)


IndexEntry Class
----------------

Defined in getdata/indexentry.h

The IndexEntry has no public constructor, nor any methods other than the ones
provided by the Entry base class.
