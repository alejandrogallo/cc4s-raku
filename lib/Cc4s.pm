# vim:ft=raku
use v6;

unit module Cc4s;

enum PlaceHolder ( <_TENSOR_NAME_>
                 , <_PATH_TO_VERTEX_FILE_>
                 )
                 ;

subset Boolean   of Int  where * ∈ (0, 1);
subset DataType  of Str  where * (elem) < RealTensor ComplexTensor >;
subset StrInput  of List where (Str, Str);
# write DataType to differentiate DataInput from StrInput
subset DataInput of List where (Str, Str, DataType);
subset IntInput  of List where (Str, Int);
subset BoolInput of List where (Str, Boolean);
subset RealInput of List where (Str, Real);
subset NoInput   of List where ();
subset Input     of List where StrInput  | DataInput | IntInput
                             | RealInput | BoolInput | NoInput
                             ;

multi sub to-string(NoInput $a) of Str is export {""}
multi sub to-string(StrInput $a) of Str is export {qq!({$a.head} "{$a.tail}")!}
multi sub to-string(DataInput $a) of Str is export {
  ($a.head ~~ $a[1]) ?? $a.head !! qq!({$a.head} {$a[1]})!
}
multi sub to-string(Input $a) of Str is export {qq!($a)!}

class Algorithm is export {
  has Str $.name;
  has Input @.inputs;
  has Input @.outputs;
  method Str {qq!
$.name [
  { (@.inputs  ==> map &to-string ==> grep /^^ . + $$/ ).join: "\n  " }
] [
  { (@.outputs ==> map &to-string ==> grep /^^ . + $$/ ).join: "\n  " }
].
!.subst: /^^ \s+ $$/, ""}

  method add-inputs (@i) of Algorithm { @.inputs.append:
                                          Array[Input].new(@i); self }
  method add-outputs (@o) of Algorithm { @.outputs.append:
                                          Array[Input].new(@o); self }

  method help {qq!
We just want to help you. Here I want an overview of possible input/output
variables. Optional and maybe also mandatory.
!}
}

subset CoulombIntegral of Str
  where * ∈ (([X~] $_ xx 4) X~ <CoulombIntegral> given < H P >);

sub TensorIO
  ( Str :name($name)
  , Str :Data($data) = ~_TENSOR_NAME_
  , Str :file($file) = ""
  , Bool :bin($bin) = False
  , DataType :dtype($dtype) = <RealTensor>
  ) of Algorithm
  {
  Algorithm.new:
    :name($name)
    :inputs( (<Data>, $data, $dtype)
           , $file ?? (<file>, $file) !! ()
           , $bin ?? (<mode>, <binary>) !! ()
           )
}

our &TensorReader is export = &TensorIO.assuming(:name<TensorReader>);
our &TensorWriter is export = &TensorIO.assuming(:name<TensorWriter>);
our &ComplexTensorReader is export
    = &TensorIO.assuming(:name<ComplexTensorReader>,
                         :dtype<ComplexTensor>);
our &ComplexTensorWriter is export
    = &TensorIO.assuming(:name<ComplexTensorWriter>,
                         :dtype<ComplexTensor>);

our sub CoulombVertexReader
  ( Str :file($file) = ~_PATH_TO_VERTEX_FILE_
  , Str :CoulombVertex($vertex) = <CoulombVertex>
  , Str :HoleEigenEnergies($h) = <HoleEigenEnergies>
  , Str :ParticleEigenEnergies($p) = <ParticleEigenEnergies>
  ) is export
  {

  Algorithm.new:
    :name<CoulombVertexReader>
    :inputs( (<file>, $file), )
    :outputs( (<CoulombVertex>, $vertex, <RealTensor>)
            , (<HoleEigenEnergies>, $h, <RealTensor>)
            , (<ParticleEigenEnergies>, $p, <RealTensor>)
            )

}

our sub CoulombIntegralsFromVertex
  ( Str :CoulombVertex($v) = <CoulombVertex>
  , Str :HoleEigenEnergies($h) = <HoleEigenEnergies>
  , Str :ParticleEigenEnergies($p) = <ParticleEigenEnergies>
  , *%ints
  ) is export
  {
  Algorithm.new:
    :name<CoulombIntegralsFromVertex>
    :inputs( (<CoulombVertex>, $v, <RealTensor>)
           , (<HoleEigenEnergies>, $h, <RealTensor>)
           , (<ParticleEigenEnergies>, $p, <RealTensor>)
           )
    :outputs(for
                 # get only things with keys being a CoulombIntegral
                 (%ints ==> grep {$_.keys[0] ~~ CoulombIntegral})
                 # name of the integral
                 {( $_.keys[0]
                 # name of the user integral, if non given, the name of
                 # the integral is given
                 , ($_.values[0] eq True) ?? $_.keys[0] !! $_.values[0]
                 # datat type
                 , <RealTensor>
                 )})
}

our sub Mp2EnergyFromCoulombIntegrals
  ( Str :PPHHCoulombIntegral($pphh) = <PPHHCoulombIntegral>
  , Str :HoleEigenEnergies($h) = <HoleEigenEnergies>
  , Str :ParticleEigenEnergies($p) = <ParticleEigenEnergies>
  , Str :Mp2Energy($energy) = <Mp2Energy>
  , Str :Mp2DoublesAmplitudes($amplitudes) = <Mp2DoublesAmplitudes>
  ) is export {

  Algorithm.new:
    :name<Mp2EnergyFromCoulombIntegrals>
    :inputs( (<HoleEigenEnergies>, $h, <RealTensor>)
           , (<ParticleEigenEnergies>, $p, <RealTensor>)
           , (<PPHHCoulombIntegral>, $pphh, <RealTensor>)
           )
    :outputs( (<Mp2Energy>, $energy, <RealTensor>)
            , (<Mp2DoublesAmplitudes>, $amplitudes, <RealTensor>)
            )

}

our sub CcsdEnergyFromCoulombIntegrals
  ( Str :PPHHCoulombIntegral($pphh) = <PPHHCoulombIntegral>
  , Str :PHPHCoulombIntegral($phph) = <PHPHCoulombIntegral>
  , Str :HHHHCoulombIntegral($hhhh) = <HHHHCoulombIntegral>
  , Str :HHHPCoulombIntegral($hhhp) = <HHHPCoulombIntegral>
  , Str :CoulombVertex($vertex) = <CoulombVertex>
  , Str :HoleEigenEnergies($h) = <HoleEigenEnergies>
  , Str :ParticleEigenEnergies($p) = <ParticleEigenEnergies>
  , Str :initialDoublesAmplitudes($i-tabij) = ""
  , Str :initialSinglesAmplitudes($i-tai) = ""
  , Boolean :distinguishable($dist) = 0
  , Boolean :PPL($ppl) = 0
  , Int :integralsSliceSize($sliceSize) = 0
  , Str :CcsdEnergy($energy) = <CcsdEnergy>
  , Str :CcsdSinglesAmplitudes($tai) = ""
  , Str :CcsdDoublesAmplitudes($tabij) = ""
  ) is export {

  Algorithm.new:
    :name<CcsdEnergyFromCoulombIntegrals>
    :inputs( (<CoulombVertex>, $vertex, <RealTensor>)
           , (<HoleEigenEnergies>, $h, <RealTensor>)
           , (<ParticleEigenEnergies>, $p, <RealTensor>)
           , (<PPHHCoulombIntegral>, $pphh, <RealTensor>)
           , (<PHPHCoulombIntegral>, $phph, <RealTensor>)
           , (<HHHHCoulombIntegral>, $hhhh, <RealTensor>)
           , (<HHHPCoulombIntegral>, $hhhp, <RealTensor>)
           , $i-tabij ?? (<initialDoublesAmplitudes>, $i-tabij, <RealTensor>) !! ()
           , $i-tai ?? (<initialSinglesAmplitudes>, $i-tai, <RealTensor>) !! ()
           , $dist ?? (<distinguishable>, $dist)  !! ()
           , $ppl ?? (<PPL>, $ppl) !! ()
           , $sliceSize ?? (<integralsSliceSize>, $sliceSize) !! ()
           )
    :outputs( (<CcsdEnergy>, $energy, <RealTensor>)
            , $tai ?? (<CcsdSinglesAmplitudes>, $tai, <RealTensor>) !! ()
            , $tabij ?? (<CcsdDoublesAmplitudes>, $tabij, <RealTensor>) !! ()
            )

}
