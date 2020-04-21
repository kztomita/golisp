#!/usr/bin/perl

use strict;
use warnings;

use Text::Diff;
use File::Basename;

sub build {
    `cd ../src;go build golisp.go`;
    my $ret = $?;
    if ($ret) {
	die "build error";
    }
}

sub eval_test_script {
    my ($file) = @_;

    my $expected_file = $file;
    $expected_file =~ s/\.lsp/.expected/;

    print "Test $file ";

    my @results = `../src/golisp ${file}`;
    my $ret = $?;
    if ($ret) {
	die "test error";
    }

    open my $fh, '<', $expected_file
      or die "Can't open $expected_file .";
    my @expected = <$fh>;
    close $fh;

    my $diff = diff \@results, \@expected;
    if ($diff eq "") {
	print "OK\n";
	return 1;
    } else {
	print "ERROR\n";
	print $diff, "\n";
	return;
    }
}

sub main {
    build;

    my ($ok, $err) = (0, 0);

    my @files;
    if (@ARGV == 0) {
	@files = glob "*\.lsp";
    } else {
	@files = (@ARGV[@ARGV - 1]);
    }

    foreach my $file (@files) {
	my $ret = eval_test_script $file;
	if ($ret) {
	    $ok++;
	} else {
	    $err++;
	}
    }
    print "OK: $ok, Error: $err\n";
}

main;
