use GetData;
use strict;
use Test::More tests => 7;

# callback
sub callback {
  my ($pdata, $extra) = @_;

  is ($extra, "extra stuff");
  is ($$pdata{'suberror'}, $GetData::E_FORMAT_BAD_LINE);
  is ($$pdata{'linenum'}, 2);
  is ($$pdata{'filename'}, "dirfile/format");

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
$d.close();

system "rm -rf dirfile";

is($e, $GetData::E_OK);
