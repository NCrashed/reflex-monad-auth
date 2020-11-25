# Pin version of nixpkgs to ensure build reproduceability
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "reflex-frp";
  repo = "reflex-platform";
  rev = "8d421e9e06b0477cbc065346aaf596c9db6cc387";
  sha256  = "06fy5b0mk5k2ps1h78yihf4j76cb855r86y9p4jv5d91nfyl2dck";
})
