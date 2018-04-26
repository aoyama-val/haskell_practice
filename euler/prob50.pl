use List::Util 'sum';
use Math::Pari qw/primes isprime/;

my $p = primes(4920); # Last 21 terms sum > 1_000_000
my $max = 545;        # First 546 terms sum > 1_000_000
my $min = 21;         # Known that 21 consecutive terms exist

TERM:
for my $terms (reverse $min .. $max) {
    for my $i (0 .. @$p - $terms) {
        my $sum = sum(@$p[$i .. $i + $terms]);
        last if $sum > 1_000_000;
        if (isprime($sum)) {
            print "$terms $sum\n";
            exit;
        }
    }
}
