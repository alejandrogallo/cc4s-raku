use Test;
use Cc4s;





ok $_ eq '(file "test.dat")', "Cc4s Input of strings: $_"
  given to-string (<file>, <test.dat>);
ok $_ eq '(IntFlag 2)', "Cc4s Input of Integers: $_"
  given to-string (<IntFlag>, Int.new: 2);
ok $_ eq '(RFlag 2.20000000000000000000)', "Cc4s Input of Real: $_"
  given to-string (<RFlag>, 2.2);
ok $_ eq '(some-flag 0)', "Cc4s Input of Booleans: $_"
  given to-string (<some-flag>, 0);
ok $_ eq '(Data SomeTensor)', "Cc4s Input of Data of TensorReal: $_"
  given to-string (<Data>, <SomeTensor>, <RealTensor>);
ok $_ eq '', "Cc4s No Input"
  given to-string ();

subtest {
  my Cc4s::CoulombIntegral @all-ints = ([X~] $_ xx 4) X~ <CoulombIntegrals>
                                        given < H P >;
  ok +@all-ints eq 16, "Possible number of Coulomb Integrals";
  ok @all-ints[0] eq <HHHHCoulombIntegrals>, <HHHHCoulombIntegrals>;
  ok @all-ints[15] eq <PPPPCoulombIntegrals>, <PPPPCoulombIntegrals>;

  given [X~] (for 1..4 { < H P > }) {
    ok < HPPH HHHH > (<=) $_ , 'Basic matching for hphp integrals with subset';
  }

}, 'Integrals';

ok $_ eq [], <real-tensor-if-given no input>
  given real-tensors-if-given :!HH :!PP :!ZZP;

ok do {
  (sort $_) eq sort [(<HH>, <HH>, <RealTensor>), (<PP>, <PP>, <RealTensor>)]
}, <real-tensor-if-given some input>
   given real-tensors-if-given :HH :PP :!ZZP;
