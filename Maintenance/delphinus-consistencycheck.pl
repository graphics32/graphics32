#!/usr/bin/perl

use warnings; 
use strict;
use v5.10;
use File::Slurp;
use JSON;
use Term::ANSIColor;
use Data::Dumper;
use XML::LibXML;
use XML::LibXSLT;
use File::Basename;

sub sayred
{
	say color('red'), @_, color('reset');
}

my $dij = decode_json read_file('Delphinus.Install.json', binmode => ':utf8'); 
my $debug = 0;

my %dijdirs;
for my $project (@{$dij->{'projects'}})
{
	my $p = $project->{'project'};
	say "Checking $p..." if $debug;
	say -f $p ? 'exists' : 'missing' if $debug;
	unless(-f $p)
	{
		sayred "$p is missing!";
		next;
	}
	$p =~ s/\\/\//g;
	my $d = dirname($p);
	$dijdirs{$d} = 1;
	sayred "Dir $d is missing!" unless(-d $d);
	say `xmllint -noout $p` if $debug;
	my $groupproj = XML::LibXML->load_xml(location => $p);
	say $groupproj if $debug>1;
	my $xpc = XML::LibXML::XPathContext->new($groupproj->documentElement());
	$xpc->registerNs('msbuild', 'http://schemas.microsoft.com/developer/msbuild/2003');
	$xpc->findnodes('/msbuild:Project/msbuild:ItemGroup/msbuild:Projects/@Include')->foreach(sub {
		my $subprj = $_->textContent();
		say 'subproject: ', $subprj if $debug;
		my $s = $d .'/'. $subprj;
		say "s: $s: ", -f $s ? 'exists' : 'missing' if $debug;
		sayred "File $s is missing!" unless(-f $s);
       	});
}

my @dirs = split /\n/, `find Source/Packages -mindepth 1 -maxdepth 1 -type d`;
say "Existing subdirs in Source/Packages: ", @dirs if $debug;
foreach my $d (@dirs)
{
       	next if exists $dijdirs{$d};
	sayred "$d missing in Delphinus.Install.json!";
}
