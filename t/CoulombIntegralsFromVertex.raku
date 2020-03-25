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
  HHHHCoulombIntegrals
].
!, 'With hhhh' given CoulombIntegralsFromVertex :HHHHCoulombIntegrals;

ok ~$_ ~~ q!
CoulombIntegralsFromVertex [
  (CoulombVertex SomeOther)
  HoleEigenEnergies
  ParticleEigenEnergies
] [
  PPPPCoulombIntegrals
].
!, 'With pppp and CoulombVertex'
given CoulombIntegralsFromVertex :CoulombVertex<SomeOther>
                                 :PPPPCoulombIntegrals
                                 ;
