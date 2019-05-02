#!/usr/bin/perl

use warnings;
use strict;
use File::Slurp qw(edit_file);

sub process
{
  my $ext = shift;
  my $find = "find -name $ext -print0";
  open(my $dprojs, '-|', $find) || die "$0: can't open pipe to $find: $!";
  my @dprojs = split /\0/, <$dprojs>;

  print scalar(@dprojs), " examples to process for extension $ext.\n";

  my $count = 0;
  for my $dproj (@dprojs)
  {
    print " Processing $dproj...";

    edit_file {

      # Delphi/dproj
      s/(<DCC_ExeOutput>).+(<\/DCC_ExeOutput>)/$1bin$2/g;
      s/(<DCC_UnitSearchPath>).+(<\/DCC_UnitSearchPath>)/$1..\\..\\..\\Source;\$\(DCC_UnitSearchPath\)$2/g;
      s/(<DCC_DcuOutput>).+(<\/DCC_DcuOutput>)/$1lib$2/g;

      # C++ Builder/cbproj
      s/(<IntermediateOutputDir>).+(<\/IntermediateOutputDir>)/$1lib$2/g;
      s/(<FinalOutputDir>).+(<\/FinalOutputDir>)/$1bin$2/g;
    } $dproj;

    print " done.\n";
    $count++;
  }
  print "Done with $count examples.\n";
}

process('*.dproj');
process('*.cbproj');
process('*.lpi');

