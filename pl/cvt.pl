use Acme::EyeDrops qw(sightly);

print sightly( { Shape      => 'camel',
                 SourceFile => 'hello.pl',
                 Regex      => 1 } );
