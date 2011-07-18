use strict;
use warnings;

my $preamble = 1;

# Generate the list
my @funclist;
for (<>) {
  $preamble = 0 if (/^MODULE/);
  next if $preamble;
  next unless /^(\w*)\(/;
  push @funclist, $1;
}
my $funclist = join(' ', sort @funclist);

for (<STDIN>) {
  s/\@FUNCLIST@/$funclist/g;
  print $_;
}
