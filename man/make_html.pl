#!/usr/bin/perl -w

use strict;

my @dlstack = ();

die unless $ARGV[0];
my $f = $ARGV[0];
my $a = $ARGV[1];
my $html = "";

sub Splitify {
  my @t;
  my $q = 0;
  my $t = undef;

  my @c = split //, shift;
  for (@c) {
    if (/"/) {
      $q = 1 - $q;
    } elsif (/ /) {
      if ($q) {
        $t .= " ";
      } elsif (defined $t) {
        push @t, $t;
        $t = undef;
      }
    } elsif (/[\S]/) {
      $t .= $_;
    } else {
      die $_;
    }
  }

  push @t, $t if defined $t;

  @t;
}

sub FromTo {
  my ($from, $to) = @_;
  my $codes;

  return "" if ($from eq $to);

  $codes .= "</$from>" unless ($from eq "R");
  $codes .= "<$to>" unless ($to eq "R");

  $codes;
}

sub FixFont {
  my ($cur, $string) = @_;
  my $out;

  $out .= FromTo("R", $cur);

  my @c = split //, $string;

  for (my $i = 0; $i <= $#c; ++$i) {
    if ($c[$i] eq '\\' and $c[$i + 1] eq 'f') {
      my $new = $c[$i + 2];
      die $string unless $new;
      $out .= FromTo($cur, $new);
      $cur = $new;
      $i += 2;
    } else {
      $out .= $c[$i];
    }
  }

  $out .= FromTo($cur, "R");

  $out;
}

sub Alternate {
  my @types;
  my $string;
  ($types[0], $types[1], $string) = @_;
  my $s = 1;
  my @t = Splitify($string);
  my $h = shift @t;

  for (@t) {
    $h .= "\\f" . $types[$s] . $_;
    $s = 1 - $s;
  }

  FixFont($types[0], $h);
}

my $dl = 0;
my $rs = 0;
my $pd = 0;
my $ul = 0;
my $nf = 0;
my $header = 0;
my $table = 0;
my $dl_end = 0;
my $pd_data;
my $vers="Version 0.0.0";
my $date="Date unknown.";
open F, $f or die $!;
while(<F>) {
  next if (/^\.\\"/);
  chomp;
  if ($_ eq "" and not $nf) {
    $_ = ($dl == 1) ? ".DP" : ".PP";
  }
  $_ = ".IP" if ($_ =~ /\.in /);
  $_ = ".PP" if ($_ eq ".in");

  #strip email addresses?
  if (/^<[^ ]*@[^ ]*>./) {
    $html .= ". ";
    next;
  }

  s/\\-/-/g;
  s/&/&amp;/g;
  s/</&lt;/g;
  s/>/&gt;/g;
  s/\\t/&nbsp;&nbsp;&nbsp;/g;
  s/\\\|/&thinsp;/g;
  s/""""/&quot;&quot;/g;
  s/\\e"/\\&quot;/g;
  s/ *\\\(em */&mdash;/g;
  s/\\\(co/&copy;/g;
  s/ -([0-9])/ &minus;$1/g;

  s/ /&nbsp;/g if ($nf and not /^\./);

  # sugar
  s/\*\*([0-9]*)/<sup>$1<\/sup>/;

  if ($table) {
    if (/center tab/) {
      $html .= '<DIV CLASS="inset"><TABLE>';
      $header = 1;
    } elsif (/^\.TE/) {
      $html .= "</TABLE></DIV>";
      $table = 0;
    } elsif (/cbscbs/ or /rlrl/) {
      ;
    } else {
      if ($header) {
        s/\|/<\/TH><TH colspan=\"2\">/g;
        $html .= "<TR><TH colspan=\"2\">$_</TH></TR>";
        $header = 0;
      } else {
        s/\|/<\/TD><TD>/g;
        $html .= "<TR><TD>$_</TD></TR>";
      }
    }
    next;
  }

  #$html .= "[$dl+$#dlstack;$rs]";
  if (/^\.TH (gd_\w*) 3 "(.*?)" "(.*?)"/) {
    $vers = $3;
    $date = $2;
    my $func = $a || $1;
    $html .= '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" ' .
    '"http://www.w3.org/TR/html4/strict.dtd"><HTML><HEAD>' .
    '<META http-equiv="Content-Type" content="text/html;charset=utf-8" >' .
    '<LINK REL="stylesheet" HREF="/getdata.css" TYPE="text/css"><TITLE>' .
    $func . '(3) Manual Page</TITLE></HEAD><BODY><H1>' . $func . "(3)</H1>";
  } elsif (/^\.TH ([^ ]*) ([15]) "(.*?)" "(.*?)"/) {
    $vers = $4;
    $date = $3;
    my $sect = $2;
    my $name = $a || $1;
    $html .= '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" ' .
    '"http://www.w3.org/TR/html4/strict.dtd"><HTML><HEAD>' .
    '<META http-equiv="Content-Type" content="text/html;charset=utf-8" >' .
    '<LINK REL="stylesheet" HREF="/getdata.css" TYPE="text/css"><TITLE>' .
    $name . "($sect) Manual Page</TITLE></HEAD><BODY><H1>" . $name .
    "($sect)</H1>";
  } elsif (/^[^.]/) {
    $html .= FixFont("R", $_) . " ";
  } elsif (/^\.B (.*)/) {
    if ($pd) {
      $pd_data = $1;
    } elsif ($dl == 2) {
      $html .= "<DT>";
      if ($pd_data) {
        $html .= FixFont("B", $pd_data) . "<BR>";
        $pd_data = undef;
      }
      $html .= FixFont("B", $1) . "</DT><DD>";
      $dl = 1;
    } else {
      $html .= FixFont("B", $1) . " ";
    }
  } elsif (/^\.BI (.*)/) {
    if ($dl == 2) {
      $html .= "<DT>";
      $html .= Alternate("B", "I", $1) . " ";
      $html .= "</DT><DD>";
      $dl = 1;
    } else {
      $html .= Alternate("B", "I", $1) . " ";
    }
  } elsif (/^\.BR (.*)/) {
    if ($dl == 2) {
      $html .= "<DT>";
      $html .= Alternate("B", "R", $1) . " ";
      $html .= "</DT><DD>";
      $dl = 1;
    } else {
      $html .= Alternate("B", "R", $1) . " ";
    }
  } elsif (/^\.DP$/) {
    $html .= "<P>";
  } elsif (/^\.HP$/) {
    if ($dl == 1) { $html .= "</DD></DL>"; $dl = pop(@dlstack) || 0; };
    if ($ul == 1) { $html .= "</UL>"; $ul = 0; };

    $html .= '<P class="hang">';
  } elsif (/^\.I (.*)/) {
    if ($dl == 2) {
      $html .= "<DT>" . FixFont("I", $1) . "</DT><DD>";
      $dl = 1;
    } else {
      $html .= FixFont("I", $1) . " ";
    }
  } elsif (/^\.IB (.*)/) {
    if ($dl == 2) {
      $html .= "<DT>";
      $html .= Alternate("I", "B", $1) . " ";
      $html .= "</DT><DD>";
      $dl = 1;
    } else {
      $html .= Alternate("I", "B", $1) . " ";
    }
  } elsif (/^\.IP \\\(bu/) {
    if ($dl == 1) { $html .= "</DD></DL>"; $dl = pop(@dlstack) || 0; };
    if ($ul == 1) {
      $html .= "<LI>";
    } else {
      $html .= "<UL><LI>";
      $ul = 1;
    }
  } elsif (/^\.IP/) {
    if ($ul == 1) { $html .= "</UL>"; $ul = 0; };

    if ($dl and not $rs) {
      $html .= '<P>';
    } else {
      $html .= '<P class="inset">';
    }
  } elsif (/^\.IR (.*)/) {
    if ($dl == 2) {
      $html .= "<DT>";
      $html .= Alternate("I", "R", $1) . " ";
      $html .= "</DT><DD>";
      $dl = 1;
    } else {
      $html .= Alternate("I", "R", $1) . " ";
    }
  } elsif (/^\.PP?$/) {
    $dl_end = 0;
    if ($dl == 1) {
      $dl_end = 3;
      $html .= "</DD></DL>";
      if ($rs) {
        $dl = 0;
      } else {
        $dl = pop(@dlstack) || 0;
      }
    }
    if ($rs == 2) { $html .= '</DIV>'; $rs = 1; }
    if ($ul == 1) { $html .= "</UL>"; $ul = 0; };

    $html .= "<P>";
  } elsif (/^\.PD 0/) {
    if ($dl_end) {
      $dl_end = 0;
      $dl = 1;
      # chop off "</DD></DL><P>"
      for (my $i = 0; $i < length("</DD></DL><P>"); ++$i) {
        chop $html;
      }
    }
    $pd = 1;
  } elsif (/^\.PD/) {
    $pd = 0;
  } elsif (/^\.RB (.*)/) {
    if ($dl == 2) {
      $html .= "<DT>";
      $html .= Alternate("R", "B", $1) . " ";
      $html .= "</DT><DD>";
      $dl = 1;
    } else {
      $html .= Alternate("R", "B", $1) . " ";
    }
  } elsif (/^\.RE/) {
    if ($dl == 1) { $html .= "</DD></DL>" }
    if ($rs == 2) { $html .= '</DIV>' }
    $dl = pop @dlstack;
    $rs = 0;
  } elsif (/^\.RI (.*)/) {
    if ($dl == 2) {
      $html .= "<DT>";
      $html .= Alternate("R", "I", $1) . " ";
      $html .= "</DT><DD>";
      $dl = 1;
    } else {
      $html .= Alternate("R", "I", $1) . " ";
    }
  } elsif (/^\.RS/) {
    $html .= '<DIV CLASS="inset">';
    push @dlstack, $dl;
    $rs = 2;
    $dl = 0;
  } elsif (/^\.SH (.*)/) {
    if ($dl == 1) { $html .= "</DD></DL>"; $dl = pop(@dlstack) || 0; };
    if ($ul == 1) { $html .= "</UL>"; $ul = 0; };

    $html .= "<H2>$1</H2><P>";
  } elsif (/^\.SS (.*)/) {
    if ($dl == 1) { $html .= "</DD></DL>"; $dl = pop(@dlstack) || 0; };
    if ($ul == 1) { $html .= "</UL>"; $ul = 0; };

    $html .= "<H3>$1</H3><P>";
  } elsif (/^\.TP/) {
    if ($ul == 1) { $html .= "</UL>"; $ul = 0; };

    if ($dl == 1) {
      $html .= "</DD>";
    } else {
      push @dlstack, $dl if ($dl);
      $html .= "<DL>";
    }
    $dl = 2;
    next;
  } elsif (/\.TS/) {
    $table = 1;
  } elsif (/^\.br/) {
    $html .= "<BR>";
  } elsif (/\.fam C/) {
    $html .= "<tt>";
    next;
  } elsif (/\.fam/) {
    $html .= "</tt>";
    next;
  } elsif (/\.fi/) {
    $nf = 0;
    next;
  } elsif (/\.nf/) {
    $nf = 1;
    next;
  } elsif (/^\.(?:nh|ad|hy)/) {
    ; # ignored
  } else {
    die $_ if ($dl == 2);
    print STDERR $_, "\n" if /^\./;
    $html .= $_;
  }
  $html .= "<BR>" if $nf;
  $dl_end-- if ($dl_end);
}
$html .= "</BODY></HTML>";

$html =~  s/ \* / &times; /g;
$html =~ s/\\~/ /g;
$html =~ s/(<B>(gd_\w*?)<\/B>\(3\))/<A HREF="$2.3.html">$1<\/A>/g;
$html =~ s/((?:<B>)?(dirfile[-a-z]*)(?:<\/B>)?\(5\))/<A HREF="$2.5.html">$1<\/A>/g;

$html =~ s/\\e/\\/g;

$html .= "<HR><P><I>Last updated on $date for GetData $vers.</I>";

print $html;

