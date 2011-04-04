use GetData;
use strict;

# callback
sub callback {
  my ($pdata, $extra) = @_;

  die "extra = $extra" if ($extra ne "extra stuff");
  die "suberror = " . $$pdata{'suberror'} if ($$pdata{'suberror'} != 8);
  die "line = " . $$pdata{'line'} if ($$pdata{'line'} ne "bad line\n");
  die "linenum = " . $$pdata{'linenum'} if ($$pdata{'linenum'} != 2);
  die "filename = " . $$pdata{'filename'} if ($$pdata{'filename'} ne
    "dirfile/format");

  $GetData::SYNTAX_IGNORE
}

# create the dirfile
system "rm -rf dirfile" if (-e "dirfile" and not -d "dirfile");
(mkdir "dirfile" or die) unless -e "dirfile";

open GLOB, ">dirfile/format" or die;
print GLOB "data RAW UINT16 8\nbad line\n" or die;
close GLOB or die;

my $d = &GetData::open("dirfile", $GetData::RDONLY, \&callback, "extra stuff");
my $e = &GetData::error($d);
$d.close();

system "rm -rf dirfile";

die "e = $e" unless ($e == $GetData::E_OK);
