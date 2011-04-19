#!/usr/bin/perl -w

use strict;

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

sub Alternate {
  my $s = 0;
  my $h = "";
  my @t = Splitify($_[4]);

  for (@t) {
    $h .= $_[$s] . $_ . $_[$s + 1];
    $s = 2 - $s;
  }

  $h;
}

my $dl = 0;
my $ul = 0;
my $nf = 0;
my $header = 0;
my $table = 0;
open F, $f or die $!;
while(<F>) {
  next if (/^\.\\"/);
  chomp;
  $_ = ".PP" if ($_ eq "" and not $nf);
  $_ = ".IP" if ($_ =~ /\.in /);
  $_ = ".PP" if ($_ eq ".in");
  s/&/&amp;/g;
  s/</&lt;/g;
  s/>/&gt;/g;
  s/\\\(em/&mdash;/g;

  s/ /&nbsp;/g if ($nf and not /^\./);

  # sugar
  s/\*\*([0-9]*)/<sup>$1<\/sup>/;

  if ($table) {
    if (/center tab/) {
      $html .= '<DIV CLASS="inset"><TABLE>';
      $header = 1;
    } elsif (/^\.TE/) {
      $html .= "</TABLE></DIV";
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

  if (/^\.TH (gd_\w*) 3 "(.*?)" "(.*?)"/) {
    my $vers = $3;
    my $date = $2;
    my $func = $a || $1;
    $html .= '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" ' .
    '"http://www.w3.org/TR/html4/strict.dtd"><HTML><HEAD>' .
    '<META http-equiv="Content-Type" content="text/html;charset=utf-8" >' .
    '<LINK REL="stylesheet" HREF="/getdata.css" TYPE="text/css"><TITLE>' .
    $func . '(3) Manual Page</TITLE></HEAD><BODY><H1>' . $func . "(3)</H1>";
  } elsif (/^[^.]/) {
    $html .= $_ . " ";
  } elsif (/^\.B (.*)/) {
    if ($dl == 2) {
      $html .= "<DT><B>$1</B></DT><DD>";
      $dl = 1;
    } else {
      $html .= "<B>$1</B> ";
    }
  } elsif (/^\.BI (.*)/) {
    if ($dl == 2) {
      $html .= "<DT>";
      $html .= Alternate("<B>", "</B>", "<I>", "</I>", $1) . " ";
      $html .= "</DT><DD>";
      $dl = 1;
    } else {
      $html .= Alternate("<B>", "</B>", "<I>", "</I>", $1) . " ";
    }
  } elsif (/^\.BR (.*)/) {
    if ($dl == 2) {
      $html .= "<DT>";
      $html .= Alternate("<B>", "</B>", "", "", $1) . " ";
      $html .= "</DT><DD>";
      $dl = 1;
    } else {
      $html .= Alternate("<B>", "</B>", "", "", $1) . " ";
    }
  } elsif (/^\.HP$/) {
    if ($dl == 1) { $html .= "</DD></DL>"; $dl = 0; };
    if ($ul == 1) { $html .= "</UL>"; $ul = 0; };

    $html .= '<P class="hang">';
  } elsif (/^\.I (.*)/) {
    $html .= "<I>$1</I> ";
  } elsif (/^\.IB (.*)/) {
    if ($dl == 2) {
      $html .= "<DT>";
      $html .= Alternate("<I>", "</I>", "<B>", "</B>", $1) . " ";
      $html .= "</DT><DD>";
      $dl = 1;
    } else {
      $html .= Alternate("<I>", "</I>", "<B>", "</B>", $1) . " ";
    }
  } elsif (/^\.IP \\\(bu/) {
    if ($dl == 1) { $html .= "</DD></DL>"; $dl = 0; };
    if ($ul == 1) {
      $html .= "<LI>";
    } else {
      $html .= "<UL><LI>";
      $ul = 1;
    }
  } elsif (/^\.IP/) {
    if ($dl == 1) { $html .= "</DD></DL>"; $dl = 0; };
    if ($ul == 1) { $html .= "</UL>"; $ul = 0; };

    $html .= '<P class="inset">';
  } elsif (/^\.IR (.*)/) {
    $html .= Alternate("<I>", "</I>", "", "", $1) . " ";
  } elsif (/^\.PP$/) {
    if ($dl == 1) { $html .= "</DD></DL>"; $dl = 0; };
    if ($ul == 1) { $html .= "</UL>"; $ul = 0; };

    $html .= "<P>";
  } elsif (/^\.PD/) {
    ;
  } elsif (/^\.RB (.*)/) {
    if ($dl == 2) {
      $html .= "<DT>";
      $html .= Alternate("", "", "<B>", "</B>", $1) . " ";
      $html .= "</DT><DD>";
      $dl = 1;
    } else {
      $html .= Alternate("", "", "<B>", "</B>", $1) . " ";
    }
  } elsif (/^\.RE/) {
    $html .= '</DIV>';
  } elsif (/^\.RI (.*)/) {
    if ($dl == 2) {
      $html .= "<DT>";
      $html .= Alternate("", "", "<I>", "</I>", $1) . " ";
      $html .= "</DT><DD>";
      $dl = 1;
    } else {
      $html .= Alternate("", "", "<I>", "</I>", $1) . " ";
    }
  } elsif (/^\.RS/) {
    $html .= '<DIV CLASS="inset">'
  } elsif (/^\.SH (.*)/) {
    if ($dl == 1) { $html .= "</DD></DL>"; $dl = 0; };
    if ($ul == 1) { $html .= "</UL>"; $ul = 0; };

    $html .= "<H2>$1</H2>";
  } elsif (/^\.SS (.*)/) {
    if ($dl == 1) { $html .= "</DD></DL>"; $dl = 0; };
    if ($ul == 1) { $html .= "</UL>"; $ul = 0; };

    $html .= "<H3>$1</H3>";
  } elsif (/^\.TP/) {
    if ($ul == 1) { $html .= "</UL>"; $ul = 0; };

    if ($dl == 1) {
      $html .= "</DD>"; $dl = 0;
    } else {
      $html .= "<DL>";
    }
    $dl = 2;
    next;
  } elsif (/\.TS/) {
    $table = 1;
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
}
$html .= "</BODY></HTML>";

$html =~ s/\\~/ /g;
$html =~ s/(<B>(gd_\w*?)<\/B>\(3\))/<A HREF="$2.3.html">$1<\/A>/g;

print $html;

