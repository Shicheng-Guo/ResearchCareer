#!/usr/bin/perl -w
use strict;
my $SearchResultFile = shift @ARGV;
open FH, $SearchResultFile or die "Cannot open the $SearchResultFile:$!\n";
open OUT, ">$SearchResultFile.Email.Sum.txt" or die "Cannot create the output file:$!\n";
print OUT "Email\tTitle\tFirst Name\tMiddle Name\tLast Name\tContribution Title\r\n";
my %AuthorInfo;
my %email;
my ($tmpLastName, $tmpForeName, $tmpEmail);
while (<FH>) {
    if (m{\s(\S+?@\w.*)\.</Aff}){
      $email{$1}++;
    }
}

foreach my $emailtemp(sort keys %email){
print OUT "$emailtemp\t$email{$emailtemp}\n";
}
