use GetData;
use strict;
use Test::More tests => 8;

# callback
sub callback {
  my ($pdata, $extra) = @_;

  is ($extra, "extra stuff", "\$extra parameter good");
  is ($$pdata{'suberror'}, $GetData::E_FORMAT_BAD_LINE, "suberror good");
  is ($$pdata{'linenum'}, 2, "linenum good");
  is ($$pdata{'filename'}, "dirfile/format", "filename good");

  $GetData::SYNTAX_IGNORE
}

# create the dirfile
system "rm -rf dirfile" if (-e "dirfile" and not -d "dirfile");
ok(-e "dirfile" or mkdir "dirfile");

open GLOB, ">dirfile/format" or die;
print GLOB "data RAW UINT16 8\nbad line\n" or die;
close GLOB or die;

my $d = &GetData::open("dirfile", $GetData::RDONLY, \&callback, "extra stuff");
ok (defined $d, "open created an object");
my $e = &GetData::error($d);
is ($e, $GetData::E_OK, "no error from open");
is ($d->close(), 0, "close succeeded");

system "rm -rf dirfile";
