#' @include starvz_data.R
NULL

starpu_states <- function() {
  c("Callback", "FetchingInput", "Idle", "Initializing", "Overhead", "PushingOutput", "Scheduling", "Submitting task", "Progressing", "Sleeping", "Submiting task", "Waiting all tasks", "Building task", "Deinitializing", "execute_on_all_wrapper", "HandleRequests")
}

all_starpu_states <- function() {
  c("Callback", "FetchingInput", "Idle", "Initializing", "Overhead", "PushingOutput", "Scheduling", "Submitting task", "Progressing", "Sleeping", "Submiting task", "Waiting all tasks", "Building task", "Deinitializing", "execute_on_all_wrapper", "HandleRequests", "Executing")
}

cholesky_states <- function() {
  cholesky_colors() %>% .$Kernel
}

qr_states <- function() {
  qr_colors() %>% .$Kernel
}

scalfmm_states <- function() {
  scalfmm_colors() %>% .$Kernel
}

#' Colors for lu
#'
#' This will be deprecated
#'
#' @export
lu_colors <- function() {
  tibble(
    Kernel = c("getrf", "trsm", "gemm", "plgsy", "plrnt"),
    Color = c("#e41a1c", "#377eb8", "#4daf4a", "yellow", "yellow"),
    Use = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
}

#' Colors for lu
#'
#' This will be deprecated
#'
#' @export
cholesky_colors <- function() {
  tibble(
    Kernel = c("potrf", "trsm", "syrk", "gemm", "plgsy"),
    Color = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a", "yellow"),
    Use = c(TRUE, TRUE, TRUE, TRUE, FALSE)
  )
}

qr_colors <- function() {
  tibble(
    Kernel = c("dgeqrt", "dlarfb", "dtpqrt", "dtpmqrt", "lapack_dgeqrt", "lapack_dlarfb", "lapack_dtpqrt", "lapack_dtpmqrt"),
    Color = c("#96e3a2", "#f68285", "#d194d0", "#9bb6dd", "#96e3a2", "#f68285", "#d194d0", "#9bb6dd"),
    Use = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
}

scalfmm_colors <- function() {
  tibble(
    # For the trace I've been given
    Kernel = c("L2L-level", "L2P", "M2L-level", "M2L-out-level", "M2M", "P2M", "P2P", "P2P-out"),
    Color = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf")
  )
}

starpu_colors <- function() {
  pre_colors <- brewer.pal(12, "Set3")
  pre_colors[13] <- "#000000"
  pre_colors[14] <- "#000000"
  pre_colors[15] <- "#000000"
  pre_colors[16] <- "#000000"
  tibble(Value = starpu_states()) %>%
    # Get colors from Set3
    mutate(Color = pre_colors) %>%
    # Adopt Luka suggestion: Idle = orange; Sleeping = rose
    mutate(Color = case_when(
      .data$Value == "Idle" ~ "#FDB462",
      .data$Value == "PushingOutput" ~ "#BEBADA",
      TRUE ~ .data$Color
    )) -> t
  # Transform to a nice named list for ggplot
  ret <- t %>% pull(.data$Color)
  names(ret) <- t %>% pull(.data$Value)
  return(ret)
}


cholesky_pastix_colors <- function() {
  tibble(
    Kernel = c(
      "blok_dpotrfsp1d_panel",
      "cblk_dpotrfsp1d_panel",
      "blok_dtrsmsp",
      "blok_dgemmsp",
      "cblk_dgemmsp"
    ),
    Color = c(
      "#e41a1c",
      "#000000",
      "#377eb8",
      "#4daf4a",
      "#c0c0c0"
    )
  )
}

#' Colors for qr mumps
#'
#' This will be deprecated
#'
#' @export
qrmumps_colors <- function() {
  tibble(
    Kernel = c(
      "geqrt", "gemqrt", "tpqrt", "tpmqrt", "block_extadd", # qrm new
      "lapack_geqrt", "lapack_gemqrt", "lapack_tpqrt", "lapack_tpmqrt", "block_copy", # qrm older
      "do_subtree", "init_block", "clean_block", "init_front", "clean_front"
    ),
    Color = c(
      "#FF7F00", "#377EB8", "#F781BF", "#A65628", "#E41A1C",
      "#FF7F00", "#377EB8", "#F781BF", "#A65628", "#E41A1C",
      "#4DAF4A", "#FFFF33", "#984EA3", "#FFFF33", "#984EA3"
    ),
    Use = c(
      TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE
    )
  )
}
