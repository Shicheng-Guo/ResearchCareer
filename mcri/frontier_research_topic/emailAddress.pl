#!/usr/bin/perl -w
use strict;
my $SearchResultFile = 'methylation-biomarker.txt';
open FH, $SearchResultFile or die "Cannot open the $SearchResultFile:$!\n";
open OUT, ">result.xls" or die "Cannot create the output file:$!\n";
my %AuthorInfo;
my ($tmpLastName, $tmpForeName, $tmpEmail);
while (<FH>) {
  if (m{LastName>(\w.*)</LastName>}){
    $tmpLastName = $1;
  }elsif (m{ForeName>(\w.*)</ForeName}){
    $tmpForeName = $1;
  }elsif (m{<Affiliation>}){
    if (m{\s(\S+?@\w.*)\.</Aff}){
      $tmpEmail = $1;
    }else{
      $tmpEmail = "None";
    }
    if ($tmpEmail =~ m/@/){
      print OUT "$tmpLastName\t$tmpForeName\t$tmpEmail\r\n";
      $tmpEmail = "None";
    }
  }
}
