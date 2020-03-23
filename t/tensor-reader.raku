use Test;
use Cc4s;

subtest {

  my Algorithm $r = TensorReader :Data<SomeTensor>
                                 :file<the-file.dat>
                                 ;

  ok $r.Str eq qq!
TensorReader [
  (Data SomeTensor)
  (file "the-file.dat")
] [

].
!
, 'TensorReader without bin';

  my Algorithm $e = TensorReader :Data<SomeTensor>
                                 :file<the-file.dat>
                                 :bin
                                 ;

  ok $e.Str eq qq!
TensorReader [
  (Data SomeTensor)
  (file "the-file.dat")
  (mode "binary")
] [

].
!
, 'TensorReader with bin';

  ok $r ~ $e, "String concatenate two reader algorithms";

}, "Tensor Reader";
