C++ BINDINGS FOR GETDATA
========================

This README describes the C++ bindings for the GetData library.  These bindings
consist of a several C++ classes in the `GetData' namespace, plus a few
associated named constants.  Header files defining these data types are
installed into the ${includedir}/getdata directory.  The classes defined in
the GetData namespace are: Dirfile, Entry, Fragment, plus an additional Entry
subclass for each field type (i.e. RawEntry, LincomEntry, &c.)

CONSTANTS
=========

Because the C++ headers include the C API header getdata.h, all named constants
defined in the C API may also be used in the C++ API without change in meaning.

In addition to the classes documented below, the C++ bindings define the
following enumerated constants.  These are re-encapsulations of various C API
named constants permitting better type safety in C++.  In all cases, the
corresponding C API constant can be used in place of the C++ named constant,
with an appropriate type-cast (so that, e.g., (GetData::DataType)GD_NULL can
be used in place of GetData::Null).

The GetData data types (defined in getdata/entry.h):

  enum DataType {
    Null      = GD_NULL,      Unknown    = GD_UNKNOWN,
    UInt8     = GD_UINT8,     Int8       = GD_INT8,
    UInt16    = GD_UINT16,    Int16      = GD_INT16,
    UInt32    = GD_UINT32,    Int32      = GD_INT32,
    UInt64    = GD_UINT64,    Int64      = GD_INT64,
    Float32   = GD_FLOAT32,   Float64    = GD_FLOAT64,
    Complex64 = GD_COMPLEX64, Complex128 = GD_COMPLEX128
  };

The GetData entry types (defined in getdata/entry.h):

  enum EntryType {
    NoEntryType       = GD_NO_ENTRY,
    RawEntryType      = GD_RAW_ENTRY,
    LincomEntryType   = GD_LINCOM_ENTRY,
    LinterpEntryType  = GD_LINTERP_ENTRY,
    BitEntryType      = GD_BIT_ENTRY,
    MultiplyEntryType = GD_MULTIPLY_ENTRY,
    PhaseEntryType    = GD_PHASE_ENTRY,
    SBitEntryType     = GD_SBIT_ENTRY,
    PolynomEntryType  = GD_POLYNOM_ENTRY,
    ConstEntryType    = GD_CONST_ENTRY,
    CarrayEntryType   = GD_CARRAY_ENTRY,
    StringEntryType   = GD_STRING_ENTRY,
    IndexEntryType    = GD_INDEX_ENTRY,
    DivideEntryType   = GD_DIVIDE_ENTRY,
    RecipEntryType    = GD_RECIP_ENTRY,
    WindowEntryType   = GD_WINDOW_ENTRY,
    MPlexEntryType    = GD_MPLEX_ENTRY
  };

The GetData window operations (defined in getdata/entry.h):

  enum WindOpType {
    WindOpEq = GD_WINDOP_EQ,
    WindOpNe = GD_WINDOP_NE,
    WindOpGe = GD_WINDOP_GE,
    WindOpGt = GD_WINDOP_GT,
    WindOpLe = GD_WINDOP_LE,
    WindOpLt = GD_WINDOP_LT,
    WindOpSet = GD_WINDOP_SET,
    WindOpClr = GD_WINDOP_CLR
  };

The GetData encoding schemes (defined in getdata/fragment.h):

  enum EncodingScheme {
    AutoEncoding   = GD_AUTO_ENCODED, 
    Bzip2Encoding  = GD_BZIP2_ENCODED,
    FlacEncoding   = GD_FLAC_ENCODED,
    GzipEncoding   = GD_GZIP_ENCODED,
    RawEncoding    = GD_UNENCODED,
    SieEncoding    = GD_SIE_ENCODED,  
    SlimEncoding   = GD_SLIM_ENCODED,
    TextEncoding   = GD_TEXT_ENCODED,
    ZzipEncoding   = GD_ZZIP_ENCODED,
    ZzslimEncoding = GD_ZZSLIM_ENCODED, 
    UnsupportedEncoding = GD_ENC_UNSUPPORTED
  };

NON-MEMBER FUNCTIONS
====================

The following non-member functions in the GetData namespace are defined in
getdata/dirfile.h:

* GetData::EncodingSupport(GetData::EncodingScheme encoding)

  This function returns GD_RDWR if the library supports both reading from and
  writing to the specified encoding, GD_RDONLY, if the library can only read
  from the encoding, or -1 if the library supports neither reading nor writing
  for the specified encoding.  See gd_encoding_support(3).


DIRFILE CLASS
=============

Defined in getdata/dirfile.h, the Getdata::Dirfile class encapsulates the
DIRFILE object, providing a thin wrapper to the C API.  The following methods
are available:

* Dirfile::Dirfile()
  
  The empty constructor creates an invalid dirfile.  Attempting to call any
  member function will result in a GD_E_BAD_DIRFILE error.

* Dirfile::Dirfile(const char *dirfilename, unsigned int flags = GD_RDONLY,
    gd_parser_callback_t sehandler = NULL, void *extra = NULL)

  This constructor takes the name of the dirfile, the dirfile flags, and
  optional pointers to a syntax error callback handler, and a caller pointer
  passed to that callback.  The constructor will call gd_cbopen(3) on the
  provided path name.  If flags is omitted, the default GD_RDWR will be used.

* Dirfile::Dirfile(DIRFILE *dirfile)
  
  This constructor creates a Dirfile object from the supplied C API's DIRFILE
  object.  This may be used especially in the supplied parser callback,
  sehandler, to instantiate a Dirfile object from the supplied DIRFILE pointer.

* ~Dirfile::Dirfile()

  If not done explicitly (see below), the destructor will take care of calling
  gd_close(3).

* int Dirfile::Close()
* int Dirfile::Discard()
  
  These call gd_close(3) and gd_discard(3) respectively.  If they return
  successfully (return value zero), the Dirfile should immediately be destroyed,
  by calling its destructor.  Calling any member function after these functions
  return successfully will result in a GD_E_BAD_DIRFILE error.

* int Dirfile::Error()

  The Error method calls gd_error(3) to return the error status of the last
  GetData library call on this Dirfile object.

* int Dirfile::ErrorCount()

  The ErrorCount method calls gd_error_count(3) to return the number of errors
  encountered by the GetData library on this Dirfile object since this method
  was last called (or, the first time, since the object was created).

* const char *Dirfile::ErrorString()
* const char *Dirfile::ErrorString(size_t len)

  The ErrorString method will return a buffer containing a description of the
  last GetData library error as obtained from gd_error_string(3).  This buffer
  is local to the object, and subsequent calls to ErrorString() will overwrite
  the buffer.  The len parameter, if specified, is ignored.

* off_t BoF(const char *field_code)
  
  This method will call gd_bof(3) and return the location of the
  beginning-of-field marker of the specified field.

* GetData::Entry *Dirfile::Entry(const char *field_code)

  This method will return a pointer to a newly allocated object of the
  appropriate Entry Child class, cast as a plain GetData::Entry, created after
  calling gd_entry(3) with the supplied field_code.  See below for a
  description of the Entry classes.

* off_t EoF(const char *field_code)
  
  This method will call gd_eof(3) and return the location of the end-of-field
  marker of the specified field.

* GetData::Fragment *Dirfile::Fragment(int index)
  
  This method will return a pointer to a newly allocated GetData::Fragment
  object corresponding to the fragment with the specified index.

* int Dirfile::FragmentIndex(const char *field_code)

  This method will call gd_fragment_index(3) and return the index number of
  the fragment defining the specified field.

* double Dirfile::FrameNum(const char *field_code, double value,
    off_t frame_start = 0, off_t frame_end = 0)

  This method will call gd_framenum_subset(3) to perform a reverse look-up
  on the specified field.  If frame_start or frame_end are omitted, the start or
  end of the field will be used as the limit.

* const char *Dirfile::Name()
  
  This method returns the name of the dirfile, that is the value of the
  dirfilename parameter passed to the constructor.

* GetData::RawEntry *Dirfile::Reference(const char *field_code = NULL)
  
  This method will call gd_reference(3) to set and/or retrieve the reference
  field.  It returns a RawEntry object describing the reference field.

* const char *Dirfile::ReferenceFilename()

  This method is equivalent to calling Dirfile::Reference()->FileName() to
  return the binary file pathname associated with the dirfile reference field,
  except that it properly check that Dirfile::Reference() hasn't returned NULL,
  and it does not create a RawEntry object.

* void Dirfile::SetCallback(gd_parser_callback_t sehandler, void *extra = NULL)
  
  This method will call gd_parser_callback(3) to change or remove the parser
  callback function.

* int Dirfile::Standards(int version = GD_VERSION_CURRENT)

  This method will call gd_dirfile_standards(3) to set or report the current
  Standards Version of the loaded dirfile.

* int Dirfile::UnInclude(int fragment_index, int del = 0)

  This method will call gd_uninclude(3) to remove the indicated fragment from
  the dirfile.  Because gd_uninclude may re-arrange the order of fragments in
  the dirfile, the caller should destroy any GetData::Fragment objects it has
  retained.

* int Dirfile::Add(const Entry &entry)
* int Dirfile::AddAlias(const char* field_code, const char* target,
    int fragment_index = 0)
* int Dirfile::AddSpec(const char *spec, int fragment_index = 0)
* const char** Dirfile::Aliases(const char* field_code)
* const char* Dirfile::AliasTarget(const char* field_code)
* int Dirfile::AlterSpec(const char *line, int recode = 0)
* const size_t Dirfile::ArrayLen(const char *field_code)
* const gd_carray_t *Dirfile::Carrays(GetData::DataType type = Float64)
* const void *Dirfile::Constants(GetData::DataType type = Float64)
* int Dirfile::Delete(const char *field_code, unsigned flags = 0)
* const char **Dirfile::EntryList(const char *parent = NULL, int type = 0,
    unsigned int flags = 0)
* const char **Dirfile::FieldList()
* const char **Dirfile::FieldListByType(GetData::EntryType type)
* unsigned long Dirfile::Flags(unsigned long set = 0, unsigned long reset = 0)
* int Dirfile::Flush(const char *field_code = NULL)
* const char *Dirfile::FormatFilename(int index)
* int Dirfile::GetCarray(const char *field_code, GetData::DataType type,
    void *data_out, unsigned int start = 0, size_t len = 0)
* int Dirfile::GetConstant(const char *field_code, GetData::DataType type,
    void *data_out)
* size_t Dirfile::GetData(const char *field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    GetData::DataType type, void *data_out)
* size_t Dirfile::GetString(const char *field_code, size_t len, char *data_out)
* int Dirfile::Hidden(const char* field_code)
* int Dirfile::Hide(const char* field_code)
* int Dirfile::Include(const char *file, int fragment_index, unsigned int flags)
* int Dirfile::IncludeAffix(const char *file, int fragment_index = 0,
    const char* prefix = NULL, const char* suffix = NULL,
    unsigned long flags = 0)
* int Dirfile::MAdd(const Entry &entry, const char *parent)
* int Dirfile::MAddAlias(const char* parent, const char* name,
    const char* target)
* int Dirfile::MAddSpec(const char *spec, const char *parent)
* int Dirfile::MAlterSpec(const char *line, const char *parent)
* unsigned int MatchEntries(const char *regex = NULL,
    int fragment = GD_ALL_FRAGMENTS, int type = 0, unsigned int flags = 0,
    const char ***entries = NULL);
* const gd_carray_t *Dirfile::MCarrays(GetData::DataType type = Float64)
* const void *Dirfile::MConstants(const char *parent, GetData::DataType type)
* int Dirfile::MetaFlush()
* const char **Dirfile::MFieldList(const char *parent)
* const char **Dirfile::MFieldListByType(const char *parent,
    GetData::EntryType type)
* void MplexLookback(int lookback)
* const char **Dirfile::MStrings(const char *parent)
* const char **Dirfile::MVectorList(const char *parent)
* int Dirfile::NAliases(const char* field_code)
* DataType Dirfile::NativeType(const char *field_code)
* unsigned int Dirfile::NEntries(const char *parent = NULL, int type = 0,
    unsigned int flags = 0)
* unsigned int Dirfile::NFields()
* unsigned int Dirfile::NFieldsByType(GetData::EntryType type)
* off_t Dirfile::NFrames()
* int Dirfile::NFormats()
* unsigned int Dirfile::NMFields(const char *parent)
* unsigned int Dirfile::NMFieldsByType(const char *parent,
    GetData::EntryType type)
* unsigned int Dirfile::NMVectors(const char *parent)
* int Dirfile::NVectors()
* int Dirfile::PutCarray(const char *field_code, GetData::DataType type,
    const void *data_in, unsigned int start = 0, size_t len = 0)
* int Dirfile::PutConstant(const char *field_code, GetData::DataType type,
    const void *data_in)
* size_t Dirfile::PutData(const char *field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    GetData::DataType type, const void *data_in)
* int Dirfile::PutString(const char *field_code, const char *data_in)
* int RawClose(const char *field_code = NULL)
* unsigned int Dirfile::SamplesPerFrame(const char *field_code)
* off_t Dirfile::Seek(const char *field_code, off_t frame_num, off_t sample_num,
    int whence)
* const char **Dirfile::Strings()
* char *StrTok(const char *string = NULL)
* int Dirfile::Sync(const char* field_code = NULL)
* off_t Dirfile::Tell(const char *field_code)
* int Dirfile::UnHide(const char* field_code)
* int Dirfile::Validate(const char *field_code)
* const char **Dirfile::VectorList()
* int VerbosePrefix(const char *prefix = NULL)

  These methods call the corresponding function from the C API on the C DIRFILE
  object associated with the C++ object.  For allowed values for arguments of
  type GetData::DataType or GetData::EntryType, see the CONSTANTS section above.

  Note that the arguments to AddSpec are opposite to that of the corresponding
  function add_spec(3) in the C API.


FRAGMENT CLASS
==============

Define in getdata/fragment.h, the GetData::Fragment class provides information
about an individual fragment in a dirfile.  This class has no public
constructor, but may be created by calling Dirfile::Fragment.

Note: The Fragment class caches the format file index for the associated
fragment.  As a result, if Dirfile::UnInclude is called, these indices will
be incorrect, and all pre-existing Fragment objects should be destroyed.

* GetData::EncodingScheme Fragment::Encoding()
* unsigned long Fragment::Endianness()
* off_t Fragment::FrameOffset()
* const char *Fragment::Name()
* int Fragment::Parent()
* int Fragment::Protection()
* int Fragment::Index()

  These methods return the specified information on the associated fragment.
  For allowed values for arguments of type EncodingScheme, see the CONSTANTS
  section above.

* int SetEncoding(EncodingScheme encoding, int recode = 0)
* int SetEndianness(unsigned long byte_sex, int recode = 0)
* int SetFrameOffset(off_t offset, int recode = 0)
* int SetPrefix(const char *prefix)
* int SetProtection(int protection_level)
* int SetSuffix(const char *suffix)

  These methods set the specified information on the associated fragment by
  calling gd_alter_encoding(3), gd_alter_endianness(3), gd_alter_frameoffset(3),
  gd_protect(3), or gd_alter_affixes(3) as appropriate.


ENTRY CLASS
===========

Defined in getdata/entry.h, the GetData::Entry class encapsulates the gd_entry_t
object.  An entry object may be "associated" with a dirfile.  An entry object
returned by Dirfile::Entry will be associated with that dirfile.  Entry objects
created by using one of the constructors will not be associated.

Changing the value of one of the data members of an associated entry object will
result in a call to the C API to update the corresponding entry in the dirfile.
Changing the value of one of the data members of an unassociated entry has no
such side effect.

The following methods are available:

* Entry::Entry()

  This will create an empty gd_entry_t object.

* int Entry::Associated()
  
  Returns non-zero if this entry object is associated with a dirfile.

* void Entry::Dissociate()
  
  Dissociates this entry object.  If the object is already dissociated, this
  function does nothing.

* EntryType Entry::Type()

  This will return the field type of the Entry's field.  This will be one of the
  Entry types listed above in the CONSTANTS section.

* int Entry::SetFragmentIndex(int fragment_index)
* int Entry::Move(int new_fragment, unsigned flags = 0)

  These will update the fragment index of the entry.  If the entry is
  associated, these will call gd_move(3) to move the field to a different
  fragment.  These two functions are equivalent, except Entry::Move allows
  specifying flags for the move.  Entry::SetFragmentIndex always calls gd_move
  with flags = 0.

* const char *Entry::Name()

  This method returns the name of the field.  

* int Entry::SetName(const char *new_name)
* int Entry::Rename(const char *new_name, unsigned flags = 0)

  These will change the name of the field of this entry.  If the entry object
  is associated, these will also call calling gd_rename(3).  These two functions
  are equivalent, except Entry::Rename allows specifying the flags explicitly.
  Entry::SetName always calls gd_rename with flags = 0.

* unsigned int Entry::Flags()
* virtual int Entry::ComplexScalars()
* virtual int Entry::FragmentIndex()
* virtual int Entry::PolyOrd()
* virtual unsigned int Entry::SamplesPerFrame()
* virtual DataType Entry::RawType()
* virtual int Entry::NFields()
* virtual int Entry::FirstBit()
* virtual int Entry::NumBits()
* virtual int Entry::Shift()
* virtual DataType Entry::ConstType()
* virtual size_t Entry::ArrayLen()
* virtual const char *Entry::Table()
* virtual WindOpType Entry::WindOp()
* virtual gd_triplet_t Entry::Threshold()
* virtual int Entry::CountVal()
* virtual int Entry::Period()

  These methods will return the corresponding member of the gd_entry_t object.
  Only methods reasonable to be queried for the given field type will return
  meaningful results.

* virtual const char *Entry::Input(int index)
* virtual double Entry::Scale(int index)
* virtual std::complex<double> Entry::CScale(int index)
* virtual double Entry::Offset(int index)
* virtual std::complex<double> Entry::COffset(int index)
* virtual double Entry::Coefficient(int index)
* virtual std::complex<double> Entry::CCoefficient(int index)
* virtual const char *Entry::Scalar(int index)
* virtual int Entry::ScalaIndex(int index)

  These methods will return an element from the gd_entry_t members in_fields[],
  m[], or b[], indexed by the supplied parameter.  Attempts to access elements
  out of range for the field that the Entry class describes will return zero
  or NULL.


ENTRY CHILD CLASSES
===================

The following classes are provided to create Entry objects of the corresponding
field type.  They inherit from Entry, and behave in the same way: if they are
associated, changing field parameters, will result in a call to the C API to
update the entry in the dirfile.


RawEntry Class
--------------

Defined in getdata/rawentry.h

* RawEntry::RawEntry()
  
  This creates a new RAW entry object with default parameters.

* RawEntry::RawEntry(const char *field_code, DataType data_type,
    unsigned int spf, int fragment_index = 0)

* const char *RawEntry::FileName()

  This calls gd_raw_filename(3) and returns the pathname of the binary file
  associated with the RAW field.

* virtual unsigned int RawEntry::SamplesPerFrame()
* virtual DataType RawEntry::RawType()
* virtual const char *Scalar(int index = 0)
* virtual int ScalarIndex(int index = 0)

  These methods, re-implemented from the Entry class, return the corresponding
  field parameter.

* int RawEntry::SetSamplesPerFrame(unsigned int spf, int recode = 0)
* int RawEntry::SetSamplesPerFrame(const char *spf, int recode = 0)
* int RawEntry::SetType(DataType type, int recode = 0)

  These methods will change the specified field parameter by calling
  gd_alter_raw(3).  If recode is non-zero, the binary file will also be
  translated.  To use a CONST field code as the sample per frame, pass a string
  to SetSamplesPerFrame().


LincomEntry Class
-----------------

Defined in getdata/lincomentry.h

* LincomEntry::LincomEntry()
  
  This creates a new LINCOM entry object with default parameters.

* LincomEntry::LincomEntry(const char *field_code, int n_fields,
    const char **in_fields, double *m, double *b, int fragment_index = 0)
* LincomEntry::LincomEntry(const char *field_code, int n_fields,
    const char **in_fields, std::complex<double> *m, std::complex<double> *b,
    int fragment_index = 0)

* virtual const char *LincomEntry::Input(int index)
* virtual int LincomEntry::ComplexScalars()
* virtual int LincomEntry::NFields()
* virtual double LincomEntry::Scale(int index)
* virtual std::complex<double> LincomEntry::CScale(int index)
* virtual double LincomEntry::Offset(int index)
* virtual std::complex<double> LincomEntry::COffset(int index)
* virtual const char *LincomEntry::Scalar(int index)
* virtual int LincomEntry::ScalarIndex(int index)

  These methods, re-implemented from the Entry class, return the corresponding
  field parameter.

* int LincomEntry::SetInput(const char *field, int index)
* int LincomEntry::SetScale(double scale, int index)
* int LincomEntry::SetScale(const char* scale, int index)
* int LincomEntry::SetScale(std::complex<double> scale, int index)
* int LincomEntry::SetOffset(double offset, int index)
* int LincomEntry::SetOffset(const char* scale, int index)
* int LincomEntry::SetOffset(std::complex<double> offset, int index)

  These functions will change the specified field parameter associated with the
  input field with the given index, which should be between zero and two.  To
  use a CONST field code as a parameter, pass a string to the appropriate
  function.  The complex scalar flag will be updated automatically as
  appropriate.

* int LincomEntry::SetNFields(int nfields)

  This will set the number of input fields for the LINCOM.  If this is greater
  than its previous value, the Set methods above should be used to initialise
  the data.


LinterpEntry Class
------------------

Defined in getdata/linterpentry.h

* LinterpEntry::LinterpEntry()
  
  This creates a new LINTERP entry object with default parameters.

* LinterpEntry::LinterpEntry(const char *field_code, const char *in_field,
    const char *table, int fragment_index = 0)

* virtual const char *Entry::Input(int index = 0)
* virtual const char *LinterpEntry::Table()

  These methods, re-implemented from the Entry class, return the corresponding
  field parameter.

* int LinterpEntry::SetInput(const char *field)
* int LinterpEntry::SetTable(const char *table, int move_table = 0)

  These methods will change the specified field parameter by calling
  gd_alter_raw(3).  If move_table is non-zero, the existing look-up table will
  be renamed to account for the change in name.


BitEntry and SBitEntry Classes
------------------------------

Defined in getdata/bitentry.h and getdata/sbitentry.h

* BitEntry::BitEntry()
* SBitEntry::SBitEntry()
  
  These creates a new BIT or SBIT entry object with default parameters.

* BitEntry::BitEntry(const char *field_code, const char *in_field, int bitnum,
    int numbits = 1, int fragment_index = 0)
* SBitEntry::SBitEntry(const char *field_code, const char *in_field, int bitnum,
    int numbits = 1, int fragment_index = 0)

* virtual const char *Input(int index = 0)
* virtual int FirstBit()
* virtual int NumBits()
* virtual const char *Scalar(int index)
* virtual int ScalarIndex(int index)

  These methods, re-implemented from the Entry class, return the corresponding
  field parameter.

* int (S)BitEntry::SetInput(const char *field)
* int (S)BitEntry::SetFirstBit(int first_bit)
* int (S)BitEntry::SetFirstBit(const char *first_bit)
* int (S)BitEntry::SetNumBits(int num_bits)
* int (S)BitEntry::SetNumBits(const char *num_bits)

  These methods will change the specified field parameter.  To use a CONST
  field code as a parameter, pass a string to the appropriate function.


MultiplyEntry Class
-------------------

Defined in getdata/multiplyentry.h

* MultiplyEntry::MultiplyEntry()
  
  This creates a new MULTIPLY entry object with default parameters.

* MultiplyEntry::MultiplyEntry(const char *field_code, const char *in_field1,
    const char *in_field2, int fragment_index = 0)

* virtual const char *MultiplyEntry::Input(int index)

  This method, re-implemented from the Entry class, returns one of the input
  fields.

* int MultiplyEntry::SetInput(const char *field, int index)

  This function will change the specified input field with the given index,
  which should be zero or one.


PhaseEntry Class
----------------

Defined in getdata/phaseentry.h

* PhaseEntry::PhaseEntry()
  
  This creates a new PHASE entry object with default parameters.

* PhaseEntry::PhaseEntry(const char *field_code, const char *in_field,
    int shift, int fragment_index = 0)

* virtual const char *PhaseEntry::Input(int index = 0)
* virtual long int PhaseEntry::Shift()
* virtual const char *PhasEntry::Scalar(int index = 0)
* virtual int PhaseEntry::ScalarIndex(int index = 0)

  These methods, re-implemented from the Entry class, return the corresponding
  field parameter.

* int PhaseEntry::SetInput(const char *field)
* int PhaseEntry::SetShift(long int shift)
* int PhaseEntry::SetShift(const char *shift)

  These functions will change the specified input field with the given index,
  which should be between zero or one.


PolynomEntry Class
-------------------

Defined in getdata/lincomentry.h

* PolynomEntry::PolynomEntry()
  
  This creates a new LINCOM entry object with default parameters.

* PolynomEntry::PolynomEntry(const char *field_code, int poly_ord,
    const char *in_field, double *a, int fragment_index = 0)
* PolynomEntry::PolynomEntry(const char *field_code, int poly_ord,
    const char *in_field, std::complex<double> *a int fragment_index = 0)

* virtual const char *PolynomEntry::Input(int index = 0)
* virtual int PolynomEntry::ComplexScalars()
* virtual int PolynomEntry::PolyOrd()
* virtual double PolynomEntry::Coefficient(int index)
* virtual std::complex<double> PolynomEntry::CCoefficient(int index)
* virtual const char *PolynomEntry::Scalar(int index)
* virtual int PolynomEntry::ScalarIndex(int index)

  These methods, re-implemented from the Entry class, return the corresponding
  field parameter.

* int PolynomEntry::SetInput(const char *field)
* int PolynomEntry::SetCoefficient(double scale, int index)
* int PolynomEntry::SetCoefficient(const char* scale, int index)
* int PolynomEntry::SetCoefficient(std::complex<double> scale, int index)

  These functions will change the specified field parameter associated with the
  input field with the given index, which should be between zero and two.  To
  use a CONST field code as a parameter, pass a string to the appropriate
  function.  The complex scalar flag will be updated automatically as
  appropriate.

* int PolynomEntry::SetPolyOrd(int nfields)

  This will set the polynomial order for the POLYNOM.  If this is greater than
  its previous value, the Set methods above should be used to initialise the
  data.


DivideEntry Class
-------------------

Defined in getdata/divideentry.h

* DivideEntry::DivideEntry()
  
  This creates a new DIVIDE entry object with default parameters.

* DivideEntry::DivideEntry(const char *field_code, const char *in_field1,
    const char *in_field2, int fragment_index = 0)

* virtual const char *DivideEntry::Input(int index)

  This method, re-implemented from the Entry class, returns one of the input
  fields.

* int DivideEntry::SetInput(const char *field, int index)

  This function will change the specified input field with the given index,
  which should be zero or one.


RecipEntry Class
-------------------

Defined in getdata/recipentry.h

* RecipEntry::RecipEntry()
  
  This creates a new RECIP entry object with default parameters.

* RecipEntry::RecipEntry(const char *field_code, const char *in_field1,
    const char *in_field2, int fragment_index = 0)

* virtual const char *RecipEntry::Input(int index = 0)
* virtual int RecipEntry::ComplexScalars()
* virtual double RecipEntry::Dividend()
* virtual std::complex<double> RecipEntry::CDividend()
* virtual const char *RecipEntry::Scalar()
* virtual int RecipEntry::ScalarIndex(int index = 0)

  These methods, re-implemented from the Entry class, return the corresponding
  field parameter.

* int RecipEntry::SetInput(const char *field)
* int RecipEntry::SetDividend(double coeff)
* int RecipEntry::SetDividend(const char* coeff)
* int RecipEntry::SetDividend(std::complex<double> coeff)

  These functions will change the specified input field or the field dividend
  to the the value or field code supplied.


WindowEntry Class
-------------------

Defined in getdata/windowentry.h

* WindowEntry::WindowEntry()
  
  This creates a new WINDOW entry object with default parameters.

* WindowEntry::WindowEntry(const char *field_code, const char *in_field,
    const char *check_field, int fragment_index = 0)

* virtual const char *WindowEntry::Input(int index)
* virtual const char *WindowEntry::Scalar(int index = 0)
* virtual int WindowEntry::ScalarIndex(int index = 0)
* virtual WindOpType WindowEntry::WindOp()
* virtual gd_triplet_t WindowEntry::Threshold()

  These methods, re-implemented from the Entry class, return the corresponding
  field parameter.

* int WindowEntry::SetInput(const char *field, int index)
* int WindowEntry::SetWindOp(WindOpType coeff)
* int WindowEntry::SetThreshold(gd_triplet_t threshold)
* int WindowEntry::SetThreshold(const char *threhsold)

  These functions will change the specified input field, check field, or the
  window operation or threshold to the the value or field code supplied.  For
  allowed values of variables of type WindOpType, see the CONSTANTS section
  above.


MplexEntry Class
-------------------

Defined in getdata/mplex.h

* Mplex::MplexEntry()
  
  This creates a new MPLEX entry object with default parameters.

* MplexEntry::MplexEntry(const char *field_code, const char *in_field,
    const char *check_field, int fragment_index = 0)

* virtual const char *MplexEntry::Input(int index)
* virtual const char *MplexEntry::Scalar(int index)
* virtual int MplexEntry::ScalarIndex(int index)
* virtual int MplexEntry::CountVal()
* virtual int MplexEntry::Period()

  These methods, re-implemented from the Entry class, return the corresponding
  field parameter.

* int MplexEntry::SetInput(const char *field, int index)
* int MplexEntry::SetWindOp(WindOpType coeff)
* int MplexEntry::SetCountVal(int count_val)
* int MplexEntry::SetCountVal(const char *count_val)
* int MplexEntry::SetPeriod(int period)
* int MplexEntry::SetPeriod(const char *period)

  These functions will change the specified input field, check field, or the
  count value or max to the value or field code supplied.


CarraytEntry Class
----------------

Defined in getdata/constentry.h

* CarraytEntry::CarraytEntry()
  
  This creates a new CONST entry object with default parameters.

* CarraytEntry::CarraytEntry(const char *field_code, DataType type,
    int fragment_index = 0)

* virtual DataType CarraytEntry::ConstType()
* virtual size_t Carray::ArrayLen()

  This method, re-implemented from the Entry class, returns the data type of the
  CONST field.

* int SetArrayLen(size_t array_len)
* int SetType(DataType field)

  These method will change the data type or length of the CARRAY field.  They
  return non-zero on error.


ConstEntry Class
----------------

Defined in getdata/constentry.h

* ConstEntry::ConstEntry()
  
  This creates a new CONST entry object with default parameters.

* ConstEntry::ConstEntry(const char *field_code, DataType type,
    int fragment_index = 0)

* virtual DataType ConstEntry::ConstType()

  This method, re-implemented from the Entry class, returns the data type of the
  CONST field.

* int SetType(DataType field)

  This method will change the data type of the CONST field.  It returns non-zero
  on error.


StringEntry Class
-----------------

Defined in getdata/stringentry.h

* StringEntry::StringEntry()
  
  This creates a new STRING entry object with default parameters.

* StringEntry::StringEntry(const char *field_code, int fragment_index = 0)


IndexEntry Class
----------------

Defined in getdata/indexentry.h

The IndexEntry has no public constructor, nor any methods other than the ones
provided by the Entry base class.
