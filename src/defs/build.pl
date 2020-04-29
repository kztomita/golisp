#!/usr/bin/perl

use strict;
use warnings;

sub build_go_code {
    my ($string) = @_;

    $string =~ s/`/` + "`" + `/g;

    my $go =<<END_OF_CODE;
package interpreter

var systemFuncDefinitions string

func init() {
	systemFuncDefinitions = `
$string
	`
}
END_OF_CODE

    return $go;
}

sub main {
    my $code = '';

    my @files = glob "*\.lsp";
    foreach my $file (@files) {
	open my $fh, '<', $file
	  or die "Can't open $file .";
	my @lines = <$fh>;
	close $fh;

	$code .= join "", @lines;
    }

    my $go_code = build_go_code $code;

    open my $fh, '>', '../interpreter/defs.go'
      or die "Can't open defs.go";
    print $fh $go_code;
    close $fh;

    print "done\n";
}

main;

