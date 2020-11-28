with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "common-tutorial";
  buildInputs = [stack
                 gnumake
                 pandoc         # For markdown rendering
                ];
}
