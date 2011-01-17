#!/usr/bin/env perl

use strict;
use vars qw { $FILE };

$FILE = shift;
if ($FILE eq "") {
    die "Syntax: $0 [filename]\n";
}

&parse($FILE);

sub parse {
    my ($FILE) = @_;
    open(FILE, "<$FILE") || die "$FILE: $!";
    mkdir("./maptiles-output", 0755);
    while (<FILE>) {
        chomp;
        my $j = 0;
        my $contents = $_;
        next unless($contents =~ /^INSERT /);
        my ($junk, $sql, $junk) = split(/\(|\)/, $contents);
        my ($zoom, $x, $y, $flags, $length, $data) = split(/\,/, $sql);
        $data =~ s/^X'//;
        $data =~ s/'$//;
        my $filename = "./maptiles-output/$x,$y\@$zoom.png";
        next if int(length($data)) < 128;
        print $filename . "\n";
        open(OUT, ">$filename") . "\n" || die "$filename: $!";
        print int(length($data)) . "\n";
        while($j < length($data)) {
            my $hex = "0x" . substr($data, $j, 2);
            print OUT chr(hex($hex));
            $j += 2;
        }
        close(OUT);
    }
    close(FILE);
}

