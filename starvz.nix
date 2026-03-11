{ 
    lib,
    fetchFromGitHub,
    rPackages,
    # StarPU,
    arrow-cpp,
    pkg-config,
    R
}:
rPackages.buildRPackage {
    name = "starvz";

    src = fetchFromGitHub {
        owner = "schnorr";
        repo = "starvz";
        rev = "CRAN_0.7.1";
        hash = "sha256-zTwr08/0hFdzk/vsxQ2N0MRmEFDm2twYsmVBgXPxpow=";
    };
    nativeBuildInputs = [ pkg-config R  rPackages.arrow];

    buildInputs = [ arrow-cpp 
    # StarPU 
    ];

    propagatedBuildInputs = with rPackages; [
        rPackages.arrow
        magrittr
        dplyr
        ggplot2
        tibble
        rlang
        tidyr
        patchwork
        purrr
        readr
        stringr
        yaml
        lpSolve
        gtools
        data_tree
        RColorBrewer
        zoo
        Rcpp
        BH
    ];

    doCheck = true;
}
