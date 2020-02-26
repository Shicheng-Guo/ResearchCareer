open F,"xml.xml";

while(<F>){

        if(/<AuthorList/){
		my $lastname;
		my $firstname;	
        chomp(my $f1=<F>);
                chomp(my $f2=<F>);
                chomp(my $f3=<F>);
                chomp(my $f4=<F>);
                chomp(my $f5=<F>);
                chomp(my $f6=<F>);
				
        if($f3=~/\>(.+)\</){
               $firstname=$1;
        }
        if($f2=~/>(.+)</){
               $lastname=$1;
        }
        if($f6=~/(\w+\@\w+.\w+)/){
		my $email=$1;
		if($email){
		print "$email\tDr.\t$firstname\t\t$lastname\tPersonalized and Precison Medicine\n";
        
		}
        }
        }
}

