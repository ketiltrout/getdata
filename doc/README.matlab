MATLAB BINDINGS FOR GETDATA
===========================

The MATLAB bindings consist of a compatibility library, libgetdata-matlab, a
collection of MEX functions, and a few pure MATLAB functions implementing the
bindings.

The compatibility library is installed in ${libdir}.  By default, the MATLAB
files are installed in ${libdir}/getdata/matlab/getdata, although this path
can be changed with the --with-matlab-dir option to ./configure.  To use the
bindings, the installation directory must be added to the MATLAB path.

Full documentation of the bindings are provided from within the MATLAB help
system.  After installing the bindings, running

  >> help getdata

will provide an overview of the bindings and provide a list of the functions
availabe.
