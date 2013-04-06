PERL BINDINGS FOR GETDATA
=========================

The Perl bindings consist of a perl module, `GetData.pm'.  They should work with
Perl 5.6 or newer.  Complex data are represented within the module as
Math::Complex objects.

The GetData Perl bindings are documented in POD markup within GetData.pm.
After installation, this documentation should be available in section 3 of
the UNIX manual:

  $ man 3 GetData

Before installation, the manual can be read using a command along the lines of

 $ pod2man bindings/perl/GetData.pm.in | nroff -mandoc | less
