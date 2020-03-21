# vim:ft=raku
use v6;

unit module Cc4s;

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
}

subset CoulombIntegral of Str
  where * ∈ (([X~] $_ xx 4) X~ <CoulombIntegral> given < H P >);

sub TensorIO ( Str :name($name)
             , Str :data($data)
             , Str :file($file) = ""
             , Bool :bin($bin) = False
             , DataType :dtype($dtype) = <RealTensor>
             ) of Algorithm {
  Algorithm.new:
    :name($name)
    :inputs( (<data>, $data, $dtype)
           , (<file>, $file)
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

our
sub CoulombVertexReader( Str :file($file)
                       , Str :vertex($vertex) = <CoulombVertex>
                       , Str :e-holes($h) = <HoleEigenEnergies>
                       , Str :e-particles($p) = <ParticleEigenEnergies>
                       ) is export {

  Algorithm.new:
    :name<CoulombVertexReader>
    :inputs( (<file>, $file)
           , (<CoulombVertex>, $vertex, <RealTensor>)
           , (<HoleEigenEnergies>, $h, <RealTensor>)
           , (<ParticleEigenEnergies>, $p, <RealTensor>)
           )

}

our
sub CoulombIntegralsFromVertex ( Str :vertex($v) = <CoulombVertex>
                               , Str :e-holes($h) = <HoleEigenEnergies>
                               , Str :e-particles($p) = <ParticleEigenEnergies>
                               , *%ints
                               ) is export {
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

our sub mp2-integrals { :PPHHCoulombIntegral }

our sub ccsd-integrals {
  my CoulombIntegral @o = < PHPH PPHH HHHH HHHP > X~
                          <CoulombIntegral>;
  %(for @o { $_ => True })
}

our sub ccsd-ref-integrals {
  my CoulombIntegral @o = < PHPH PPHH HHHH HHHP PPPP PPPH > X~
                          <CoulombIntegral>;
  %(for @o { $_ => True })
}

our
sub Mp2EnergyFromCoulombIntegrals
  ( Str :PPHHCoulombIntegral($pphh) = <PPHHCoulombIntegral>
  , Str :e-holes($h) = <HoleEigenEnergies>
  , Str :e-particles($p) = <ParticleEigenEnergies>
  , Str :energy($energy) = <Mp2Energy>
  , Str :amplitudes($amplitudes) = <Mp2DoublesAmplitudes>
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

our
sub CcsdEnergyFromCoulombIntegrals
  ( Str :PPHHCoulombIntegral($pphh) = <PPHHCoulombIntegral>
  , Str :e-holes($h) = <HoleEigenEnergies>
  , Str :e-particles($p) = <ParticleEigenEnergies>
  , Str :energy($energy) = <CcsdEnergy>
  , Str :singles-amplitudes($tai) = <CcsdSinglesAmplitudes>
  , Str :doubles-amplitudes($tabij) = <CcsdDoublesAmplitudes>
  ) is export {

  Algorithm.new:
    :name<CcsdEnergyFromCoulombIntegrals>
    :inputs( (<HoleEigenEnergies>, $h, <RealTensor>)
           , (<ParticleEigenEnergies>, $p, <RealTensor>)
           , (<PPHHCoulombIntegral>, $pphh, <RealTensor>)
           )
    :outputs( (<CcsdEnergy>, $energy, <RealTensor>)
            , (<CcsdSinglesAmplitudes>, $tai, <RealTensor>)
            , (<CcsdDoublesAmplitudes>, $tabij, <RealTensor>)
            )

}
