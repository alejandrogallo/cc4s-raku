# vim:ft=raku
use v6;

unit module Cc4s;

enum PlaceHolder ( <_TENSOR_NAME_>
                 , <_PATH_TO_VERTEX_FILE_>
                 , <_PATH_TO_XYZ_FILE_>
                 , <_BASIS_SET_>
                 )
                 ;

#| A cc4s boolean type, either 0 or 1
subset Boolean   of Int  where * ∈ (0, 1);
subset DataType  of Str  where * (elem) < RealTensor ComplexTensor >;
subset StrInput  of List where (Str, Str);
# write DataType to differentiate DataInput from StrInput
subset DataInput of List where (Str, Str, DataType);
subset IntInput  of List where (Str, Int);
subset BoolInput of List where (Str, Boolean);
subset RealInput of List where (Str, Num) | (Str, Rat);
subset NoInput   of List where ();
subset Input     of List where StrInput  | DataInput | IntInput
                             | RealInput | BoolInput | NoInput
                             ;

multi sub to-string(NoInput $a) of Str is export {""}
multi sub to-string(StrInput $a) of Str is export {qq!({$a.head} "{$a.tail}")!}
multi sub to-string(RealInput $a) of Str is export {
  "({$a.head} {$a.tail.fmt: "%.20f"})"
}
multi sub to-string(DataInput $a) of Str is export {
  ($a.head ~~ $a[1]) ?? $a.head !! "({$a.head} {$a[1]})"
}
multi sub to-string(Input $a) of Str is export {"($a)"}

class Algorithm is export {
  has Str $.name;
  has Input @.inputs;
  has Input @.outputs;
  has Signature $.signature;
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

  method help {
    qq!
    The signature of the $!name is

    { $!signature.gist.subst: /\,/, "\n", :g }

    !
  }
}

subset CoulombIntegral of Str
  where * ∈ (([X~] $_ xx 4) X~ <CoulombIntegrals> given < H P >);

sub real-tensors-if-given (*%tsrs) of Array[DataInput] is export {
  Array[DataInput].new: (
    (.keys[0], (.values[0] eq True) ?? .keys[0] !! .values[0], <RealTensor>)
    for %tsrs ==> grep {.values[0]}
  )
}

#| Template function for most tensor input/output functions in cc4s
sub TensorIO
  ( Str :$name
  , Str :$Data = ~_TENSOR_NAME_
  , Str :$file = ""
  , Bool :$bin = False
  , DataType :dtype($dtype) = <RealTensor>
  ) of Algorithm
  {
  Algorithm.new:
    :name($name)
    :signature(&?ROUTINE.signature)
    :inputs( (<Data>, $Data, $dtype)
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

#| CoulombVertexReader
sub CoulombVertexReader
  ( Str :file($file) = ~_PATH_TO_VERTEX_FILE_
  , Str :CoulombVertex($vertex) = <CoulombVertex>
  , Str :HoleEigenEnergies($h) = <HoleEigenEnergies>
  , Str :ParticleEigenEnergies($p) = <ParticleEigenEnergies>
  ) is export
  {

  Algorithm.new:
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( (<file>, $file), )
    :outputs( (<CoulombVertex>, $vertex, <RealTensor>)
            , (<HoleEigenEnergies>, $h, <RealTensor>)
            , (<ParticleEigenEnergies>, $p, <RealTensor>)
            )

}

# Convert a hash of { :PPHHCoulombIntegrals, :WHATEVER }
# into a list of DataInput of CoulombIntegrals ready to be fed into a cc4s algo
sub cints-hash-to-input-list (%ints) of Array[DataInput] is export {
   real-tensors-if-given |$_
     given hash %ints.grep: {.keys[0] ~~ CoulombIntegral}
}

#| CoulombIntegralsFromVertex
sub CoulombIntegralsFromVertex
  ( Str :CoulombVertex($v) = <CoulombVertex>
  , Str :HoleEigenEnergies($h) = <HoleEigenEnergies>
  , Str :ParticleEigenEnergies($p) = <ParticleEigenEnergies>
  #| This should be any combination of :HHHHCoulombIntegrals, ...
  , *%ints
  ) is export
  {
  Algorithm.new:
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( (<CoulombVertex>, $v, <RealTensor>)
           , (<HoleEigenEnergies>, $h, <RealTensor>)
           , (<ParticleEigenEnergies>, $p, <RealTensor>)
           )
    :outputs(cints-hash-to-input-list %ints)
}

#| Mp2EnergyFromCoulombIntegrals
sub Mp2EnergyFromCoulombIntegrals
  ( Str :PPHHCoulombIntegrals($pphh) = <PPHHCoulombIntegrals>
  , Str :HoleEigenEnergies($h) = <HoleEigenEnergies>
  , Str :ParticleEigenEnergies($p) = <ParticleEigenEnergies>
  , Str :Mp2Energy($energy) = <Mp2Energy>
  , Str :Mp2DoublesAmplitudes($amplitudes) = <Mp2DoublesAmplitudes>
  ) is export {

  Algorithm.new:
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( (<HoleEigenEnergies>, $h, <RealTensor>)
           , (<ParticleEigenEnergies>, $p, <RealTensor>)
           , (<PPHHCoulombIntegrals>, $pphh, <RealTensor>)
           )
    :outputs( (<Mp2Energy>, $energy, <RealTensor>)
            , (<Mp2DoublesAmplitudes>, $amplitudes, <RealTensor>)
            )

}

#| CcsdEnergyFromCoulombIntegrals
sub CcsdEnergyFromCoulombIntegrals
  ( Str :PPHHCoulombIntegrals($pphh) = <PPHHCoulombIntegrals>
  , Str :PHPHCoulombIntegrals($phph) = <PHPHCoulombIntegrals>
  , Str :HHHHCoulombIntegrals($hhhh) = <HHHHCoulombIntegrals>
  , Str :HHHPCoulombIntegrals($hhhp) = <HHHPCoulombIntegrals>
  , Str :CoulombVertex($vertex) = <CoulombVertex>
  , Str :HoleEigenEnergies($h) = <HoleEigenEnergies>
  , Str :ParticleEigenEnergies($p) = <ParticleEigenEnergies>
  , Str :initialDoublesAmplitudes($i-tabij) = ""
  , Str :initialSinglesAmplitudes($i-tai) = ""
  , Boolean :distinguishable($dist) = 0
  , Boolean :PPL($ppl) = 0
  , Int :integralsSliceSize($sliceSize) = 0
  , Str :mixer($mixer) = ""
  , Real :mixingRatio($mixRat) = -1
  , Int :MaxResidua($maxRes) = 0
  , Int :maxIterations($maxIter) = 0
  , Real :amplitudesConvergence($ampConv) = -1
  , Real :energyConvergence($enConv) = -1
  , Str :CcsdEnergy($energy) = <CcsdEnergy>
  , Str :CcsdSinglesAmplitudes($tai) = ""
  , Str :CcsdDoublesAmplitudes($tabij) = ""
  ) is export {

  Algorithm.new:
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( (<CoulombVertex>, $vertex, <RealTensor>)
           , (<HoleEigenEnergies>, $h, <RealTensor>)
           , (<ParticleEigenEnergies>, $p, <RealTensor>)
           , (<PPHHCoulombIntegrals>, $pphh, <RealTensor>)
           , (<PHPHCoulombIntegrals>, $phph, <RealTensor>)
           , (<HHHHCoulombIntegrals>, $hhhh, <RealTensor>)
           , (<HHHPCoulombIntegrals>, $hhhp, <RealTensor>)
           , $i-tabij ?? (<initialDoublesAmplitudes>, $i-tabij, <RealTensor>) !! ()
           , $i-tai ?? (<initialSinglesAmplitudes>, $i-tai, <RealTensor>) !! ()
           , $dist ?? (<distinguishable>, $dist)  !! ()
           , $ppl ?? (<PPL>, $ppl) !! ()
           , $sliceSize ?? (<integralsSliceSize>, $sliceSize) !! ()
           , $mixer ?? (<Mixer>, $mixer) !! ()
           , ($mixRat > 0) ?? (<mixingRatio>, $mixRat) !! ()
           , $maxRes ?? (<MaxResidua>, $maxRes) !! ()
           , $maxIter ?? (<maxIterations>, $maxIter) !! ()
           , ($ampConv > 0) ?? (<amplitudesConvergence> , $ampConv) !! ()
           , ($enConv > 0) ?? (<energyConvergence> , $enConv) !! ()
           )
    :outputs( (<CcsdEnergy>, $energy, <RealTensor>)
            , $tai ?? (<CcsdSinglesAmplitudes>, $tai, <RealTensor>) !! ()
            , $tabij ?? (<CcsdDoublesAmplitudes>, $tabij, <RealTensor>) !! ()
            )

}

#| CcsdEnergyFromCoulombIntegralsReference
sub CcsdEnergyFromCoulombIntegralsReference
  ( Str :PPHHCoulombIntegrals($pphh) = <PPHHCoulombIntegrals>
  , Str :PHPHCoulombIntegrals($phph) = <PHPHCoulombIntegrals>
  , Str :HHHHCoulombIntegrals($hhhh) = <HHHHCoulombIntegrals>
  , Str :HHHPCoulombIntegrals($hhhp) = <HHHPCoulombIntegrals>
  , Str :PPPHCoulombIntegrals($ppph) = <PPPHCoulombIntegrals>
  , Str :PPPPCoulombIntegrals($pppp) = <PPPPCoulombIntegrals>
  , Str :HoleEigenEnergies($h) = <HoleEigenEnergies>
  , Str :ParticleEigenEnergies($p) = <ParticleEigenEnergies>
  , Str :initialDoublesAmplitudes($i-tabij) = ""
  , Str :initialSinglesAmplitudes($i-tai) = ""
  , Boolean :OnlyPPL($ppl) = 0
  , Str :mixer($mixer) = ""
  , Real :mixingRatio($mixRat) = -1
  , Int :MaxResidua($maxRes) = 0
  , Int :maxIterations($maxIter) = 0
  , Real :amplitudesConvergence($ampConv) = -1
  , Real :energyConvergence($enConv) = -1
  , Str :CcsdEnergy($energy) = <CcsdEnergy>
  , Str :CcsdSinglesAmplitudes($tai) = ""
  , Str :CcsdDoublesAmplitudes($tabij) = ""
  ) is export {

  Algorithm.new:
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( (<HoleEigenEnergies>, $h, <RealTensor>)
           , (<ParticleEigenEnergies>, $p, <RealTensor>)
           , (<PPHHCoulombIntegrals>, $pphh, <RealTensor>)
           , (<PHPHCoulombIntegrals>, $phph, <RealTensor>)
           , (<HHHHCoulombIntegrals>, $hhhh, <RealTensor>)
           , (<HHHPCoulombIntegrals>, $hhhp, <RealTensor>)
           , (<PPPHCoulombIntegrals>, $ppph, <RealTensor>)
           , (<PPPPCoulombIntegrals>, $pppp, <RealTensor>)
           , $i-tabij ?? (<initialDoublesAmplitudes>, $i-tabij, <RealTensor>) !! ()
           , $i-tai ?? (<initialSinglesAmplitudes>, $i-tai, <RealTensor>) !! ()
           , $ppl ?? (<OnlyPPL>, $ppl) !! ()
           , $mixer ?? (<Mixer>, $mixer) !! ()
           , ($mixRat > 0) ?? (<mixingRatio>, $mixRat) !! ()
           , $maxRes ?? (<MaxResidua>, $maxRes) !! ()
           , $maxIter ?? (<maxIterations>, $maxIter) !! ()
           , ($ampConv > 0) ?? (<amplitudesConvergence> , $ampConv) !! ()
           , ($enConv > 0) ?? (<energyConvergence> , $enConv) !! ()

           )
    :outputs( (<CcsdEnergy>, $energy, <RealTensor>)
            , $tai ?? (<CcsdSinglesAmplitudes>, $tai, <RealTensor>) !! ()
            , $tabij ?? (<CcsdDoublesAmplitudes>, $tabij, <RealTensor>) !! ()
            )

}

#| CoulombIntegralsFromGaussian
sub CoulombIntegralsFromGaussian
  ( Str :xyzStructureFile($structure) = ~_PATH_TO_XYZ_FILE_
  , Str :basisSet($basis) = ~_BASIS_SET_
  , Str :CoulombIntegrals($cs) = <CoulombIntegrals>
  , Str :kernel($kernel) where * ∈ < coulomb delta > = <coulomb>
  , Boolean :chemistNotation($c-n) = 1
  ) of Algorithm is export
  {

  Algorithm.new:
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( (<xyzStructureFile>, $structure)
           , (<basisSet>, $basis)
           , (<kernel>, $kernel)
           , (<chemistNotation>, $c-n)
           )
    :outputs( (<CoulombIntegrals>, $cs, <RealTensor>), )

}

#| HartreeFockFromGaussian
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
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( (<xyzStructureFile>, $structure)
           , (<basisSet>, $basis)
           , (<maxIterations>, $max-iter)
           , (<energyDifference>, $ediff)
           , $nelec eq -1 ?? () !! (<numberOfElectrons>, $nelec)
           )
    :outputs( $core ?? (<CoreHamiltonian>, $core, <RealTensor>) !! ()
            , $coeffs ?? (<OrbitalCoefficients>, $coeffs, <RealTensor>) !! ()
            , $overlap ?? (<OverlapMatrix>, $overlap, <RealTensor>) !! ()
            , $energy ?? (<HartreeFockEnergy>, $energy, <RealTensor>) !! ()
            , $h ?? (<HoleEigenEnergies>, $h, <RealTensor>) !! ()
            , $p ?? (<ParticleEigenEnergies>, $p, <RealTensor>) !! ()
            )

}

#| MeanCorrelationHoleDepth
sub MeanCorrelationHoleDepth
  ( Str :$DoublesAmplitudes = <DoublesAmplitudes>
  , Str :$PPHHDelta = <PPHHDelta>
  , Str :$G = <G>
  ) of Algorithm is export
  {

  Algorithm.new:
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( (<DoublesAmplitudes>, $DoublesAmplitudes, <RealTensor>)
           , (<PPHHDelta>, $PPHHDelta, <RealTensor>)
           )
    :outputs( (<G>, $G, <RealTensor>), )

}


#| CoulombIntegralsFromRotatedCoulombIntegrals
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
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( (<OrbitalCoefficients>, $OrbitalCoefficients, <RealTensor>)
           , (<CoulombIntegrals>, $CoulombIntegrals, <RealTensor>)
           , $nelec eq -1 ?? () !! (<nelec>, $nelec)
           , $nelec eq -1 ?? (<No>, $No) !! ()
           , $cn eq 1 ?? () !! (<chemistNotation>, $cn)
           )
    :outputs(cints-hash-to-input-list %ints)
}

#| FiniteSizeCorrection
sub FiniteSizeCorrection
  ( Str :CoulombVertex($vertex) = <CoulombVertex>
  , Str :CoulombKernel($kernel) = <CoulombKernel>
  , Str :HoleEigenEnergies($h) = <HoleEigenEnergies>
  , Str :ParticleEigenEnergies($p) = <ParticleEigenEnergies>
  , Str :SinglesAmplitudes($tai) = ~_TENSOR_NAME_
  , Str :DoublesAmplitudes($tabij) = ~_TENSOR_NAME_
  , Str :StructureFactor($sofg) = <StructureFactor>
  ) of Algorithm is export
  {
  Algorithm.new:
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( (<HoleEigenEnergies>, $h, <RealTensor>)
           , (<ParticleEigenEnergies>, $p, <RealTensor>)
           , (<CoulombVertex>, $vertex, <RealTensor>)
           , (<CoulombKernel>, $kernel, <RealTensor>)
           , (<SinglesAmplitudes>, $tai, <RealTensor>)
           , (<DoublesAmplitudes>, $tabij, <RealTensor>)
           )
    :outputs( (<StructureFactor>, $sofg, <RealTensor>), )
}

sub TensorAntisymmetrizer(*%ints) of Algorithm is export {
  Algorithm.new: :name(&?ROUTINE.name)
                 :signature(&?ROUTINE.signature)
                 :inputs(cints-hash-to-input-list %ints)
}

sub TensorUnrestricter
  ( Str :$Data is required
  , Str :$Out = $Data
  ) of Algorithm is export
  {
  Algorithm.new:
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( real-tensors-if-given :$Data )
    :outputs( real-tensors-if-given :$Out )
}

sub UccsdAmplitudesFromCoulombIntegrals
  ( Boolean :$intermediates = 1
  , Boolean :$antisymmetrize = 1
  , Boolean :$unrestricted = 1
  , Str :mixer($mixer) = ""
  , Real :mixingRatio($mixRat) = -1
  , Int :MaxResidua($maxRes) = 0
  , Int :maxIterations($maxIter) = 0
  , Real :amplitudesConvergence($ampConv) = -1
  , Real :energyConvergence($enConv) = -1
  , Str :$HoleEigenEnergies
  , Str :$ParticleEigenEnergies
  , Str :$initialDoublesAmplitudes
  , Str :$initialSinglesAmplitudes
  , Str :$HPFockMatrix = ""
  , Str :$PPFockMatrix = ""
  , Str :$HHFockMatrix = ""
  , Str :$UccsdDoublesAmplitudes = ""
  , Str :$UccsdSinglesAmplitudes = ""
  , Str :$UccsdEnergy = ""
  , Str :$HHHHCoulombIntegrals = <HHHHCoulombIntegrals>
  , Str :$HHHPCoulombIntegrals = <HHHPCoulombIntegrals>
  , Str :$HHPHCoulombIntegrals = <HHPHCoulombIntegrals>
  , Str :$HHPPCoulombIntegrals = <HHPPCoulombIntegrals>
  , Str :$HPHHCoulombIntegrals = <HPHHCoulombIntegrals>
  , Str :$HPHPCoulombIntegrals = <HPHPCoulombIntegrals>
  , Str :$HPPHCoulombIntegrals = <HPPHCoulombIntegrals>
  , Str :$HPPPCoulombIntegrals = <HPPPCoulombIntegrals>
  , Str :$PHHPCoulombIntegrals = <PHHPCoulombIntegrals>
  , Str :$PHPHCoulombIntegrals = <PHPHCoulombIntegrals>
  , Str :$PHPPCoulombIntegrals = <PHPPCoulombIntegrals>
  , Str :$PPHHCoulombIntegrals = <PPHHCoulombIntegrals>
  , Str :$PPHPCoulombIntegrals = <PPHPCoulombIntegrals>
  , Str :$PPPHCoulombIntegrals = <PPPHCoulombIntegrals>
  , Str :$PPPPCoulombIntegrals = <PPPPCoulombIntegrals>
  ) of Algorithm is export
  {

  Algorithm.new:
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( :$intermediates.kv.list
           , :$antisymmetrize.kv.list
           , :$unrestricted.kv.list
           , $mixer ?? (<Mixer>, $mixer) !! ()
           , ($mixRat > 0) ?? (<mixingRatio>, $mixRat) !! ()
           , $maxRes ?? (<MaxResidua>, $maxRes) !! ()
           , $maxIter ?? (<maxIterations>, $maxIter) !! ()
           , ($ampConv > 0) ?? (<amplitudesConvergence> , $ampConv) !! ()
           , ($enConv > 0) ?? (<energyConvergence> , $enConv) !! ()
           , |$_ given real-tensors-if-given
                 :$HHHHCoulombIntegrals :$HHHPCoulombIntegrals
                 :$HHPHCoulombIntegrals :$HHPPCoulombIntegrals
                 :$HPHHCoulombIntegrals :$HPHPCoulombIntegrals
                 :$HPPHCoulombIntegrals :$HPPPCoulombIntegrals
                 :$PHHPCoulombIntegrals :$PHPHCoulombIntegrals
                 :$PHPPCoulombIntegrals :$PPHHCoulombIntegrals
                 :$PPHPCoulombIntegrals :$PPPHCoulombIntegrals
                 :$PPPPCoulombIntegrals
                 :$HPFockMatrix :$PPFockMatrix :$HHFockMatrix
                 :$HoleEigenEnergies
                 :$ParticleEigenEnergies
                 :$initialDoublesAmplitudes
                 :$initialSinglesAmplitudes
           )
    :outputs( |$_ given real-tensors-if-given :$UccsdEnergy
                                              :$UccsdDoublesAmplitudes
                                              :$UccsdSinglesAmplitudes
            )

}

grammar cc4s-range {
  token TOP { <single-range>* %% \, }
  token single-range { \s*\d+\s*[\-\s*\d+]?\s* }
}

sub CcsdEquationOfMotionDavidson
  ( Boolean :$complexVersion = 1
  , Str :$oneBodyRdmRange where {cc4s-range.parse($_)} = ""
  , Str :$printEigenvectorsDoubles where {cc4s-range.parse($_)} = ""
  , Str :$printEigenvectorsRange where {cc4s-range.parse($_)} = ""
  # Davidson solver
  , Real :$ediff = 0.00000001
  , Int :$maxBasisSize = 0
  , Boolean :$intermediates = 1
  , Int :$eigenstates is required = 2
  , Boolean :$refreshIterations =  0
  , Boolean :$refreshOnMaxBasisSize = 0
  , Int :$maxIterations = 32
  , Int :$minIterations = 1
  # preconditioner
  , Boolean :$preconditionerRandom = 0
  , Real :$preconditionerRandomSigma = 0.01
  , Boolean :$preconditionerSpinFlip = 0
  # T amplitudes
  , Str :$SinglesAmplitudes is required
  , Str :$DoublesAmplitudes is required
  # Fock Matrix
  , Str :$ParticleEigenEnergies = <ParticleEigenEnergies>
  , Str :$HoleEigenEnergies = <HoleEigenEnergies>
  , Str :$HPFockMatrix = <HPFockMatrix>
  , Str :$HHFockMatrix = <HHFockMatrix>
  , Str :$PPFockMatrix = <PPFockMatrix>
  , Str :$HHHHCoulombIntegrals = <HHHHCoulombIntegrals>
  , Str :$PPPPCoulombIntegrals = <PPPPCoulombIntegrals>
  , Str :$HHHPCoulombIntegrals = <HHHPCoulombIntegrals>
  , Str :$HHPPCoulombIntegrals = <HHPPCoulombIntegrals>
  , Str :$HPHHCoulombIntegrals = <HPHHCoulombIntegrals>
  , Str :$HPHPCoulombIntegrals = <HPHPCoulombIntegrals>
  , Str :$HPPPCoulombIntegrals = <HPPPCoulombIntegrals>
  , Str :$PPHPCoulombIntegrals = <PPHPCoulombIntegrals>
  , Str :$PPPHCoulombIntegrals = <PPPHCoulombIntegrals>
  , Str :$PHPPCoulombIntegrals = <PHPPCoulombIntegrals>
  , Str :$PHPHCoulombIntegrals = <PHPHCoulombIntegrals>
  , Str :$HPPHCoulombIntegrals = <HPPHCoulombIntegrals>
  , Str :$HHPHCoulombIntegrals = <HHPHCoulombIntegrals>
  , Str :$PHHPCoulombIntegrals = <PHHPCoulombIntegrals>
  ) of Algorithm is export
  {
  Algorithm.new:
    :name(&?ROUTINE.name)
    :signature(&?ROUTINE.signature)
    :inputs( :$complexVersion.kv.list
           , :$oneBodyRdmRange.kv.list
           , :$printEigenvectorsDoubles.kv.list
           , :$printEigenvectorsRange.kv.list
           , :$ediff.kv.list
           , :$intermediates.kv.list
           , $maxBasisSize ?? :$maxBasisSize.kv.list !! ()
           , :$eigenstates.kv.list
           , :$refreshIterations.kv.list
           , :$refreshOnMaxBasisSize.kv.list
           , :$maxIterations.kv.list
           , :$minIterations.kv.list
           , :$preconditionerRandom.kv.list
           , :$preconditionerRandomSigma.kv.list
           , :$preconditionerSpinFlip.kv.list
           , |$_ given real-tensors-if-given
                 :$HHHHCoulombIntegrals :$PPPPCoulombIntegrals
                 :$HHHPCoulombIntegrals :$HHPPCoulombIntegrals
                 :$HPHHCoulombIntegrals :$HPHPCoulombIntegrals
                 :$HPPPCoulombIntegrals :$PPHPCoulombIntegrals
                 :$PPPHCoulombIntegrals :$PHPPCoulombIntegrals
                 :$PHPHCoulombIntegrals :$HPPHCoulombIntegrals
                 :$HHPHCoulombIntegrals :$PHHPCoulombIntegrals
                 :$HPFockMatrix :$PPFockMatrix :$HHFockMatrix
                 :$HoleEigenEnergies :$ParticleEigenEnergies
                 :$DoublesAmplitudes :$SinglesAmplitudes
           )
}
