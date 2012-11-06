use Acme::EyeDrops qw(sightly);
print sightly( { Shape       => 'camel',
        SourceFile  => 'nice_guy.pl',
        Regex       => 1 } );
