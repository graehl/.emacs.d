#!/usr/bin/env perl
package Foo;
1;
package main;
use strict;
use warnings;

use Perl::Lint;

my $file = shift or die "Usage: $0 file";

my $linter = Perl::Lint->new;
my $violations = $linter->lint($file);

if (my $count = scalar(@{$violations})) {
    my @sorted = sort {
        $a->{filename} cmp $b->{filename}
    } sort { $a->{line} <=> $b->{line} } @{$violations};

    for my $violation (@sorted) {
        my $description = $violation->{description} || 'a something violation found';
        (my $policy = $violation->{policy}) =~ s{\APerl::Lint::Policy::}{};
        my $line = $violation->{line} == 0 ? 1 : $violation->{line};
        printf "%s:%d: %s\n",
            $violation->{filename}, $line, $description;
    }
    return 1;
} else {
    printf "No violations!!\n";
    return 0;
}
