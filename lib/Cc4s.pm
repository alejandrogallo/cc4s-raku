use v6;

unit module Cc4s;

#########LIST OF SUPPORTED ALGORITHM
####### DONE #####
# TensorReader TensorWriter
# ComplexTensorWriter ComplexTensorReader CoulombIntegralsFromVertex
##### MISSING ####
# CcsdEnergyFromCoulombIntegrals CcsdEnergyFromCoulombIntegralsReference
# ParenthesisTriples


sub TensorIO ( Str :name($name)
             , Str :data($data)
             , Str :file($file) = ""
             , Str :mode($mode) where * (elem) < binary text > = "text"
             ) {

  my Str $output = $name;
  $output ~= "[";
  $output ~= "(data " ~ $data ~ ")";
  if $file { $output ~= " (file \""~$file~"\")" };
  if $mode ~~ "binary" { $output ~= "  (mode \"binary\")" };
  $output ~= "][].\n";

}
## TODO: change Str :mode because we should be consistent with cc4s.
##       There is only "binary" or nothing. When I try "bin" it crashes....not necessary
sub TensorReader ( Str :data($data)
                , Str :file($file) = ""
                , Str :mode($mode) where * (elem) < binary text > = "text"
                ) is export {

  TensorIO( name => "TensorReader", data => $data, file => $file, mode => $mode);

}

sub TensorWriter ( Str :data($data)
                , Str :file($file) = ""
                , Str :mode($mode) where * (elem) < binary text > = "text"
                ) is export {

  TensorIO( name => "TensorWriter", data => $data, file => $file, mode => $mode);

}

sub ComplexTensorReader ( Str :data($data)
                , Str :file($file) = ""
                , Str :mode($mode) where * (elem) < binary text > = "text"
                ) is export {

  TensorIO( name => "ComplexTensorReader", data => $data, file => $file, mode => $mode);

}

sub ComplexTensorWriter ( Str :data($data)
                , Str :file($file) = ""
                , Str :mode($mode) where * (elem) < binary text > = "text"
                ) is export {

  TensorIO( name => "ComplexTensorWriter", data => $data, file => $file, mode => $mode);

}

sub CoulombVertexReader( Str :file($file) ) is export {

  my Str $output = "CoulombVertexReader [\n";
  $output ~= "(file \""~$file~"\")\n] [\n";
  $output ~= "   CoulombVertex  HoleEigenEnergies ParticleEigenEnergies \n].\n";


}

sub CoulombIntegralsFromVertex ( Str :mode($mode) = "None"
                               , Str :integrals(@integrals)
                               ) is export {
  my Str @XXXX = [ "PHPH", "PPHH", "HHHH", "HHHP", "PPPP", "PPPH",
                   "PHHH", "HHPP", "PHHP", "HPHH", "HPHP", "HPPP",
                   "PPHP", "HPPH", "HHPH", "PHPP" ];
  my Str $ci = "CoulombIntegrals";
  my Str @integralList;
  if $mode ~~ "mp2"     { @integralList.push: @XXXX[1]~$ci };
  if $mode ~~ "ccsd"    { for 0..4 -> $i { @integralList.push: @XXXX[$i]~$ci } };
  if $mode ~~ "ccsdref" { for 0..6 -> $i { @integralList.push: @XXXX[$i]~$ci } };
  if $mode ~~ "kccsd"   { say "Kein Plan alter" };
  if $mode ~~ "None" {
    for @XXXX -> $i {
      my Str $xint = $i~"CoulombIntegrals";
      if @integrals.grep( * ~~ $xint) { @integralList.push: $xint };
    }
  }

  if !@integralList { return "%WARNING: CoulombIntegralsFromVertex w/o valid arguments ";}
  my Str $output = "CoulombIntegralsFromVertex [\n";
  $output ~= "  CoulombVertex HoleEigenEnergies ParticleEigenEnergies\n] [\n";
  for @integralList -> $i { $output ~= "  "~$i~"\n" };
  $output ~= "].\n";

}

sub CcsdEnergyFromCoulombIntegrals () is export {


}
