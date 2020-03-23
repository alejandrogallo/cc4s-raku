use Test;
use Cc4s;





ok * eq '(file "test.dat")', "Cc4s Input of strings"
  given to-string (<file>, <test.dat>);
ok * eq '(someFlag 2)', "Cc4s Input of Integers"
  given to-string (<someFlag>, 2);
ok * eq '(someFlag 2.2)', "Cc4s Input of Real"
  given to-string (<someFlag>, 2.2);
ok * eq '(some-flag 0)', "Cc4s Input of Booleans"
  given to-string (<some-flag>, 0);
ok * eq '(Data SomeTensor)', "Cc4s Input of Data of TensorReal"
  given to-string (<Data>, <SomeTensor>, <RealTensor>);
ok * eq '', "Cc4s No Input"
  given to-string ();

subtest {
  my Cc4s::CoulombIntegral @all-ints = ([X~] $_ xx 4) X~ <CoulombIntegral>
                                        given < H P >;
  ok +@all-ints eq 16, "Possible number of Coulomb Integrals";
  ok @all-ints[0] eq <HHHHCoulombIntegral>, <HHHHCoulombIntegral>;
  ok @all-ints[15] eq <PPPPCoulombIntegral>, <PPPPCoulombIntegral>;

  given [X~] (for 1..4 { < H P > }) {
    ok < HPPH HHHH > (<=) $_ , 'Basic matching for hphp integrals with subset';
  }

}, 'Integrals';
