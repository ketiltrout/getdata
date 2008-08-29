C++ BINDINGS FOR GETDATA
========================

This README describes the C++ bindings for the GetData library.  These bindings
consist of a single C++ class `Dirfile' defined in the `libdirifle' library.
The Dirfile class implements a thin wrapper around the C library.  C++ programs
that want access to this class should include dirfile.h.

* Dirfile::Dirfile(const char* dirfilename, unsigned int flags = GD_RDWR);

  The constructor takes the name of the dirfile and the dirfile flags and will
  call dirfile_open(3) on the provided path name.  If flags is omitted, the
  default GD_RDWR will be used.

* ~Dirfile::Dirfile()

  The destructor will take care of calling dirfile_close(3).

* int Dirfile::Error()

  The Error method provides access to the error member of the underlying
  DIRFILE* object.

* const char* Dirfile::ErrorString(size_t len = 4096)

  The ErrorString method will return a buffer containing a description of the
  last GetData library error as obtained from getdata_error_string(3).  This
  buffer is local to the object, and subsequent calls to ErrorString() will
  overwrite the buffer.  The string written to the buffer will be at most
  len characters long, up to a maximum of 4096 characters.

* unsigned int Dirfile::SamplesPerFrame(const char* field_code)
* const char** Dirfile::FieldList();
* unsigned int Dirfile::NFields();
* off_t Dirfile::NFrames();
* size_t Dirfile::GetData(const char* field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    Dirfile::DataType type, void* data_out);
* size_t Dirfile::PutData(const char* field_code, off_t first_frame,
    off_t first_sample, size_t num_frames, size_t num_samples,
    Dirfile::DataType type, const void* data_in);

  These methods call the corresponding function from the C API on the C DIRFILE
  object associated with the C++ object.  Arguments of type Dirfile::DataType
  should be one of:

    Null, Unknown, UInt8, Int8, UInt16, Int16, UInt32, Int32, UInt64, Int64,
    Float32, Float64

  which are aliases for the gd_type_t values GD_NULL, GD_UNKNOWN, GD_UINT8, &c.
