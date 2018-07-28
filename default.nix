{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({pkgs,  ... }: {
        overrides = self: super: let
                  semantic-reflex = pkgs.fetchFromGitHub {
                  owner = "tomsmalley";
                  repo = "semantic-reflex";
                  rev = "38fce7e4d08d46b8664768f1b7fe38846dbac1e2";
                  sha256 = "1s2p12r682wd8j2z63pjvbi4s9v02crh6nz8kjilwdsfs02yp5p2";
                  };
                  semantic-reflex = self.callCabal2nix "semantic-reflex" "${semantic-reflex}/semantic-reflex" {};
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
