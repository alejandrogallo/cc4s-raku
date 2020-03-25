# vim:ft=raku
use v6;

unit module Cc4s;

enum PlaceHolder ( <_TENSOR_NAME_>
                 , <_PATH_TO_VERTEX_FILE_>
                 , <_PATH_TO_XYZ_FILE_>
                 , <_BASIS_SET_>
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

# Convert a hash of { :PPHHCoulombIntegrals, :WHATEVER }
# into a list of DataInput of CoulombIntegrals ready to be fed into a cc4s algo
sub cints-hash-to-input-list (%ints) of Array[DataInput] {
  Array[DataInput].new: (
     ( .keys[0]
     , (.values[0] eq True) ?? .keys[0] !! .values[0]
     , <RealTensor>
     ) for (%ints ==> grep {.keys[0] ~~ CoulombIntegral})
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
    :outputs(cints-hash-to-input-list %ints)
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

our sub CcsdEnergyFromCoulombIntegralsReference
  ( Str :PPHHCoulombIntegral($pphh) = <PPHHCoulombIntegral>
  , Str :PHPHCoulombIntegral($phph) = <PHPHCoulombIntegral>
  , Str :HHHHCoulombIntegral($hhhh) = <HHHHCoulombIntegral>
  , Str :HHHPCoulombIntegral($hhhp) = <HHHPCoulombIntegral>
  , Str :PPPHCoulombIntegral($ppph) = <PPPHCoulombIntegral>
  , Str :PPPPCoulombIntegral($pppp) = <PPPPCoulombIntegral>
  , Str :HoleEigenEnergies($h) = <HoleEigenEnergies>
  , Str :ParticleEigenEnergies($p) = <ParticleEigenEnergies>
  , Str :initialDoublesAmplitudes($i-tabij) = ""
  , Str :initialSinglesAmplitudes($i-tai) = ""
  , Boolean :OnlyPPL($ppl) = 0
  , Str :CcsdEnergy($energy) = <CcsdEnergy>
  , Str :CcsdSinglesAmplitudes($tai) = ""
  , Str :CcsdDoublesAmplitudes($tabij) = ""
  ) is export {

  Algorithm.new:
    :name<CcsdEnergyFromCoulombIntegrals>
    :inputs( (<HoleEigenEnergies>, $h, <RealTensor>)
           , (<ParticleEigenEnergies>, $p, <RealTensor>)
           , (<PPHHCoulombIntegral>, $pphh, <RealTensor>)
           , (<PHPHCoulombIntegral>, $phph, <RealTensor>)
           , (<HHHHCoulombIntegral>, $hhhh, <RealTensor>)
           , (<HHHPCoulombIntegral>, $hhhp, <RealTensor>)
           , (<PPPHCoulombIntegral>, $ppph, <RealTensor>)
           , (<PPPPCoulombIntegral>, $pppp, <RealTensor>)
           , $i-tabij ?? (<initialDoublesAmplitudes>, $i-tabij, <RealTensor>) !! ()
           , $i-tai ?? (<initialSinglesAmplitudes>, $i-tai, <RealTensor>) !! ()
           , $ppl ?? (<OnlyPPL>, $ppl) !! ()
           )
    :outputs( (<CcsdEnergy>, $energy, <RealTensor>)
            , $tai ?? (<CcsdSinglesAmplitudes>, $tai, <RealTensor>) !! ()
            , $tabij ?? (<CcsdDoublesAmplitudes>, $tabij, <RealTensor>) !! ()
            )

}

our sub CoulombIntegralsFromGaussian
  ( Str :xyzStructureFile($structure) = ~_PATH_TO_XYZ_FILE_
  , Str :basisSet($basis) = ~_BASIS_SET_
  , Str :CoulombIntegrals($cs) = <CoulombIntegrals>
  , Str :kernel($kernel) where * ∈ < coulomb delta > = <coulomb>
  , Boolean :chemistNotation($c-n) = 1
  ) of Algorithm is export
  {

  Algorithm.new:
    :name<CoulombIntegralsFromGaussian>
    :inputs( (<xyzStructureFile>, $structure)
           , (<basisSet>, $basis)
           , (<kernel>, $kernel)
           , (<chemistNotation>, $c-n)
           )
    :outputs( (<CoulombIntegrals>, $cs, <RealTensor>), )

}

sub HartreeFockFromGaussian
  ( Str :xyzStructureFile($structure) = ~_PATH_TO_XYZ_FILE_
  , Str :basisSet($basis) = ~_BASIS_SET_
  , Real :energyDifference($ediff) = 1e-6
  , Int :numberOfElectrons($nelec) where * > -2 = -1
  , Int :maxIterations($max-iter) = 50
  # outputs
  , Str :CoreHamiltonian($core) = ""
  , Str :HartreeFockEnergy($energy) = <HartreeFockEnergy>
  , Str :HoleEigenEnergies($h) = <HoleEigenEnergies>
  , Str :OrbitalCoefficients($coeffs) = <OrbitalCoefficients>
  , Str :OverlapMatrix($overlap) = ""
  , Str :ParticleEigenEnergies($p) = <ParticleEigenEnergies>

  ) of Algorithm is export
  {

  Algorithm.new:
    :name<HartreeFockFromGaussian>
    :inputs( (<xyzStructureFile>, $structure)
           , (<basisSet>, $basis)
           , (<maxIterations>, $max-iter)
           , (<energyDifference>, $ediff)
           , $nelec eq -1 ?? () !! (<numberOfElectrons>, $nelec)
           )
    :output( $core ?? (<CoreHamiltonian>, $core, <RealTensor>) !! ()
           , $coeffs ?? (<OrbitalCoefficients>, $core, <RealTensor>) !! ()
           , $overlap ?? (<OverlapMatrix>, $core, <RealTensor>) !! ()
           , $energy ?? (<HartreeFockEnergy>, $core, <RealTensor>) !! ()
           , $h ?? (<HoleEigenEnergies>, $core, <RealTensor>) !! ()
           , $p ?? (<ParticleEigenEnergies>, $core, <RealTensor>) !! ()
           )

}

sub MeanCorrelationHoleDepth
  ( Str :$DoublesAmplitudes = <DoublesAmplitudes>
  , Str :$PPHHDelta = <PPHHDelta>
  , Str :$G = <G>
  ) of Algorithm is export
  {

  Algorithm.new:
    :name<MeanCorrelationHoleDepth>
    :inputs( (<DoublesAmplitudes>, $DoublesAmplitudes, <RealTensor>)
           , (<PPHHDelta>, $PPHHDelta, <RealTensor>)
           )
    :outputs( (<G>, $G, <RealTensor>), )

}


sub CoulombIntegralsFromRotatedCoulombIntegrals
  ( Str :$OrbitalCoefficients = <OrbitalCoefficients>
  , Str :$CoulombIntegrals = <CoulombIntegrals>
  , Int :$nelec where * > -2 = -1
  , Int :$No = $nelec div 2
  , Boolean :chemistNotation($cn) = 1
  , *%ints
  ) of Algorithm is export
  {
  Algorithm.new:
    :name<CoulombIntegralsFromRotatedCoulombIntegrals>
    :inputs( (<OrbitalCoefficients>, $OrbitalCoefficients, <RealTensor>)
           , (<CoulombIntegrals>, $CoulombIntegrals, <RealTensor>)
           , $nelec eq -1 ?? () !! (<nelec>, $nelec)
           , $nelec eq -1 ?? (<No>, $No) !! ()
           , $cn eq 1 ?? () !! (<chemistNotation>, $cn)
           )
    :outputs(cints-hash-to-input-list %ints)
}
