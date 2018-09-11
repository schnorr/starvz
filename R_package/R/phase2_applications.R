
starpu_states <- function()
{
    c("Callback", "FetchingInput", "Idle", "Initializing", "Overhead", "PushingOutput", "Scheduling", "Submitting task", "Progressing", "Sleeping", "Submiting task", "Waiting all tasks", "Building task", "Deinitializing");
}

cholesky_states <- function()
{
    cholesky_colors() %>% .$Kernel;
}

scalfmm_states <- function()
{
    scalfmm_colors() %>% .$Kernel;
}
cholesky_colors <- function()
{
    tibble(
        Kernel = c("potrf", "trsm", "syrk", "gemm"),
        Color = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"));
}

scalfmm_colors <- function()
{
    tibble(
# For the trace I've been given
        Kernel = c("L2L-level", "L2P",     "M2L-level", "M2L-out-level", "M2M",     "P2M",     "P2P",     "P2P-out"),
        Color =  c("#e41a1c",   "#377eb8", "#4daf4a",   "#984ea3",       "#ff7f00", "#ffff33", "#a65628", "#f781bf"));

# For paper https://hal.inria.fr/hal-01474556/document
#        Kernel = c("L2L",     "L2P",     "M2L_in",  "M2L_out", "M2M",     "P2M",     "P2P_in",  "P2P_out"),
#        Color =  c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf"));
}

starpu_colors <- function()
{
  pre_colors <- brewer.pal(12, "Set3");
  pre_colors[13] = "#000000"
  pre_colors[14] = "#000000"
  tibble(Value = starpu_states()) %>%
      # Get colors from Set3
      mutate(Color = pre_colors) %>%
      # Adopt Luka suggestion: Idle = orange; Sleeping = rose
      mutate(Color = case_when(Value == "Idle" ~ "#FDB462",
                               Value == "PushingOutput" ~ "#BEBADA",
                               TRUE ~ Color)) -> t;
    # Transform to a nice named list for ggplot
    ret <- t %>% pull(Color)
    names(ret) <- t %>% pull(Value);
    return(ret);
}

cholesky_pastix_colors <- function()
{
    tibble(
        Kernel = c("blok_dpotrfsp1d_panel",
                   "cblk_dpotrfsp1d_panel",
                   "blok_dtrsmsp",
                   "blok_dgemmsp",
                   "cblk_dgemmsp"),
        Color = c("#e41a1c",
                  "#000000",
                  "#377eb8",
                  "#4daf4a",
                  "#c0c0c0"));
}

qrmumps_colors <- function()
{
qrmumps_color_mapping() %>%
    # Rename
    rename(Kernel = StateName, Color=RGB) %>%
    # Remove Idle
    filter(Kernel != "Idle") %>%
    # Change to character
    mutate(Kernel = as.character(Kernel), Color=as.character(Color)) %>%
    # Select only those necessary
    select(Kernel, Color) %>%
    # Change names according to latest modifications
    mutate(Kernel = case_when(
               .$Kernel == "ASM" ~ "assemble_block",
               .$Kernel == "GEMQRT" ~ "lapack_gemqrt",
               .$Kernel == "GEQRT" ~ "lapack_geqrt",
               .$Kernel == "TPMQRT" ~ "lapack_tpmqrt",
               .$Kernel == "TPQRT" ~ "lapack_tpqrt",
               .$Kernel == "Do_subtree" ~ "do_subtree",
               .$Kernel == "CLEAN" ~ "clean_front",
               .$Kernel == "INIT" ~ "init_front",
               TRUE ~ .$Kernel)) %>%
    # Add new kernels
    bind_rows (tibble(Kernel = c("init_block", "clean_block"),
                      Color = c("#FFFF33", "#984EA3")));
}
