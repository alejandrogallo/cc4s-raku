use Cc4s;
use Test;

subtest {

  my @a = ( &TensorReader
          , &TensorWriter
          , &ComplexTensorWriter
          , &ComplexTensorReader
          , &CoulombVertexReader
        # Basic algorithms
          , &CcsdEnergyFromCoulombIntegrals
          , &CoulombIntegralsFromVertex
          , &Mp2EnergyFromCoulombIntegrals
          , &CoulombIntegralsFromRotatedCoulombIntegrals
        # Hartree fock related
          , &HartreeFockFromGaussian
          , &CoulombIntegralsFromGaussian
          , &MeanCorrelationHoleDepth
          );

  for @a {
    given $_.() { ok ~$_, "creating {$_.name}"; }
  }

}, <Creation of algorithms>;
