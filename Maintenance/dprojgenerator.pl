#!/usr/bin/perl

use warnings;
use strict;
use FindBin;
use v5.10;
use File::Temp qw/ tempfile tempdir /;

my $debug = 0;

sub click_on_save_changes
{
  use Win32::GuiTest ':FUNC';
  use Time::HiRes 'usleep';
  $| = 1;
  while(1)
  {
    my @windows = WaitWindowLike undef, '^&Ja$', 'Button', undef, undef, 1;
    for (@windows)
    {
      my $bestaetigen = GetWindowText(GetParent(GetParent(GetParent($_))));
      next unless $bestaetigen eq 'Bestätigen';
      say "Pushing button $_..." if $debug;
      PushChildButton GetParent($_), '&Ja';
      last;
    }
    print ".";
    usleep 1e5;
  }
}

my $pid = fork;
click_on_save_changes if $pid==0 or $debug==3;

my $tempdir = tempdir( CLEANUP => 1 );
my @dprs = split /\n/, `find "$FindBin::Bin" -name '*.dpr'`;

foreach my $dpr (@dprs)
{
  say "dpr: $dpr" if $debug>1;
  my $dproj = $dpr;
  $dproj =~ s/\.dpr$/.dproj/;
  next if -r $dproj;
  say "Missing: $dproj" if $debug;

  my ($fh, $filename) = tempfile(DIR => $tempdir, SUFFIX => '.bat');
  say $fh 'call "C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\bin\rsvars.bat"';
  my $wdpr = Cygwin::posix_to_win_path($dpr);
#  say $fh "pause";
  say $fh "\"%BDS%/bin/bds\" -ns -m \"$wdpr\"";
  close $fh;
  say `cat $filename` if $debug;
  my $w = Cygwin::posix_to_win_path($filename);
  say `cmd.exe /c "$w"`;
  unlink $filename;
  #exit;
}
kill 'TERM', $pid;
