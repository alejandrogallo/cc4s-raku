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

  ok ~$_ eq qq!
TensorReader [
  (Data SomeTensor)
  (file "the-file.dat")
  (mode "binary")
  (slice 23)
] [
  (slicing 2.23000000000000000000)
].
!, "TensorReader with extra inputs and outputs"
given (TensorReader :Data<SomeTensor>
                    :file<the-file.dat>
                    :bin
                    ).add-inputs([(<slice>, 23),])
                     .add-outputs([(<slicing>, 2.23),])
                    ;

}, "Tensor Reader";
