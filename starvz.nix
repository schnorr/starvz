{ 
    lib,
    rPackages,
    arrow-cpp,
    pkg-config,
    R
}:
rPackages.buildRPackage {
    name = "starvz";

    src = ./.;
    nativeBuildInputs = [ pkg-config R  rPackages.arrow];

    buildInputs = [ arrow-cpp ];

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
