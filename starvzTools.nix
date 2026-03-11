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

    src = ./inst;
    nativeBuildInputs = [ 
         makeWrapper
         myR
    ];
    patchPhase = ''
        patchShebangs tools/*.R
        patchShebangs tools/*.sh
    '';

    buildInputs = [ zlib ];

    buildPhase = ''
        $CC -O2 tools/starvz_csv.c -o starvz_fast_csv_split -lz -fopenmp
    '';

    installPhase = ''
        # tools/ (STARVZ_HOME/tools/)
        mkdir -p $out/tools
        cp tools/*.sh tools/*.R tools/starvz tools/starvz_csv.c $out/tools/
        cp starvz_fast_csv_split $out/tools/
        chmod +x $out/tools/*.sh $out/tools/starvz
        patch

        # etc/ (STARVZ_HOME/etc/)
        mkdir -p $out/etc
        cp -r etc/* $out/etc/

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
