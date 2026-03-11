{
	description = ''Flake for the derivation of the R package starvz 
	and the tool starvz, compiled as the package starvz-tool'';

	inputs = {
		nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
		flake-utils.url = "github:numtide/flake-utils";
		starpu.url = "github:Sacolle/nix-starpu";
		poti.url = "github:schnorr/poti";
		pajeng.url = "github:schnorr/pajeng";
	};

	outputs = { self, nixpkgs, flake-utils, starpu, poti, pajeng }: 
	flake-utils.lib.eachDefaultSystem (system:
	let
		StarPU = (starpu.packages.${system}.default.override {
			enableCUDA = false;
			enableTrace = true;
            extraOptions = [ "--enable-maxcpus=256" ];
		});
		pkgs = import nixpkgs { inherit system; };
		starvz = pkgs.callPackage ./starvz.nix {};
		starvzTools = pkgs.callPackage ./starvzTools.nix { 
			poti = poti.packages.${system}.poti;
			pajeng = pajeng.packages.${system}.pajeng;
            inherit StarPU starvz; 
        };
	in
	{
        # defaultPackage = self.packages.${system}.starvzTools;
		packages = rec {
            default = starvzTools;
            inherit starvz starvzTools;
		};
	});
}
