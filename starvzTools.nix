{ 
    stdenv,
    lib,
    fetchFromGitHub,
    StarPU,
    pkg-config,
    makeWrapper,
    rWrapper,
    zlib,
    recutils,

    starvz,
    poti,
    pajeng
}:
stdenv.mkDerivation (f: 
let
    myR = (rWrapper.override { packages = [ starvz ]; });
    starvzPath = lib.makeBinPath [ 
        myR 
        poti 
        pajeng 
        StarPU 
        recutils 
    ];
in
{
    name = "starvz-tools";
    version = "0.7.1";

    src = fetchFromGitHub {
        owner = "schnorr";
        repo = "starvz";
        rev = "CRAN_${f.version}";
        hash = "sha256-zTwr08/0hFdzk/vsxQ2N0MRmEFDm2twYsmVBgXPxpow=";
    };
    nativeBuildInputs = [ 
         makeWrapper
         myR
    ];
    patchPhase = ''
        patchShebangs inst/tools/*.R
        patchShebangs inst/tools/*.sh
    '';

    buildInputs = [ zlib ];

    buildPhase = ''
        $CC -O2 inst/tools/starvz_csv.c -o starvz_fast_csv_split -lz -fopenmp
    '';

    installPhase = ''
        # tools/ (STARVZ_HOME/tools/)
        mkdir -p $out/tools
        cp inst/tools/*.sh inst/tools/*.R inst/tools/starvz inst/tools/starvz_csv.c $out/tools/
        cp starvz_fast_csv_split $out/tools/
        chmod +x $out/tools/*.sh $out/tools/starvz
        patch

        # etc/ (STARVZ_HOME/etc/)
        mkdir -p $out/etc
        cp -r inst/etc/* $out/etc/

        # bin/starvz — placed here so that STARVZ_HOME=$(dirname $0)/..=$out
        mkdir -p $out/bin
        cp $out/tools/starvz $out/bin/starvz
        
        # add a symlink to the script, so when runing nix run it finds it
        ln -sf $out/bin/starvz $out/bin/starvz-tools

        wrapProgram $out/bin/starvz \
            --prefix PATH : ${starvzPath} \
            --set R_LIBS_USER /nonexistent
    '';
})
