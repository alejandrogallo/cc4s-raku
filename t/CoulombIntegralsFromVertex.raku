use Test;
use Cc4s;

ok ~$_ ~~ q!
CoulombIntegralsFromVertex [
  CoulombVertex
  HoleEigenEnergies
  ParticleEigenEnergies
] [

].
!, 'Without integrals' given CoulombIntegralsFromVertex;

ok ~$_ ~~ q!
CoulombIntegralsFromVertex [
  CoulombVertex
  HoleEigenEnergies
  ParticleEigenEnergies
] [
  HHHHCoulombIntegral
].
!, 'With hhhh' given CoulombIntegralsFromVertex :HHHHCoulombIntegral;

ok ~$_ ~~ q!
CoulombIntegralsFromVertex [
  (CoulombVertex SomeOther)
  HoleEigenEnergies
  ParticleEigenEnergies
] [
  PPPPCoulombIntegral
].
!, 'With pppp and CoulombVertex'
given CoulombIntegralsFromVertex :CoulombVertex<SomeOther>
                                 :PPPPCoulombIntegral
                                 ;
