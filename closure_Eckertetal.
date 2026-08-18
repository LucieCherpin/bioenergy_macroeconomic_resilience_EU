# ===============================
# Closure + IO consistency utils from Eckert et al. (forthcoming)
# ===============================
#Adapt accordingly to match parameters from Scenarios code to Model code

# Notation used throughout this helper file:
#   x_t          = gross output vector in year t,
#   F_t          = final-demand vector in year t,
#   Z_t          = intermediate-transactions matrix in year t,
#   A_t          = technical coefficient matrix with a_ij,t = z_ij,t / x_j,t,
#   L_t          = (I - A_t)^(-1),
#   eps_R, eps_N = exogenous renewable and non-renewable quantity growth rates,
#   epsprime_R,
#   epsprime_N   = endogenous intermediate-demand controls on the brown/green rows,
#   g            = aggregate GDP-growth target from the external scenario,
#   g_R, g_N,
#   g_O          = implied final-demand growth rates for the renewable, non-renewable, and other sectors.
#
# The fixed-point problem solved here is not a standard one-shot Leontief update. The model jointly solves:
#   1) a macro closure on final-demand growth,
#   2) a monetary IO-consistency problem,
#   3) an energy-row adjustment problem that steers the monetary system toward the imposed physical
#      brown/green transition path.

# Closure options (used by both WEM and WAM):
# - "residual-others":
#     * Policy quantity growth is enforced via eps_R, eps_N (used in Z row updates elsewhere).
#     * Final demand growth for REN and NREN is aligned with policy rates:
#         g_R = eps_R, g_N = eps_N.
#     * g_O is solved each iteration so that aggregate growth g is satisfied:
#         (1+g)X = (1+g_R)X_R + (1+g_N)X_N + (1+g_O)X_O.
#     * Economic meaning: REN/NREN demand follows transition targets; other sectors absorb
#       macro consistency. This preserves energy transition paths but can make "others"
#       residual, which may be implausible if transition is aggressive.
#
# - "fixed-others":
#     * Policy quantity growth enforced as above.
#     * Non-energy final demand growth is fixed to g_O = g.
#     * REN demand is fixed to the policy path: g_R = eps_R (optimistic green alignment).
#     * g_N is solved from the aggregate identity.
#     * Economic meaning: the rest of the economy keeps the aggregate growth rate; the
#       residual adjustment is loaded onto NREN demand, making this the most "optimistic"
#       for green (given REN is cleaner).
#
# - "uniform-demand":
#     * Policy quantity growth enforced as above.
#     * REN and NREN final demand both grow at the aggregate rate: g_R = g_N = g.
#     * g_O is solved from the aggregate identity.
#     * Economic meaning: demand growth is uniform in REN/NREN; transition effects are
#       driven by intermediate-use adjustments rather than demand reallocation.
#
# - "eps-only":
#     * Policy quantity growth enforced as above.
#     * Final demand growth is uniform for all sectors: g_R = g_N = g_O = g.
#     * Economic meaning: current behavior retained; no sectoral closure for demand.

compute_closure_growth <- function(option, eps_R, eps_N, g, X_prev, idx_ren, idx_nren,
                                   va_coeff = NULL, target = "output") {
  idx_other <- setdiff(seq_along(X_prev), c(idx_ren, idx_nren))

  # Closure can be formulated either in gross-output weights or GDP/value-added weights.
  # If target = "gdp", the closure identity is applied to
  #   w_i,t = v_i x_i,t,
  # where v_i is the fixed base-year value-added coefficient.
  # If target = "output", the same equations are applied directly to x_i,t.
  if (target == "gdp") {
    if (is.null(va_coeff) || length(va_coeff) != length(X_prev)) {
      stop("va_coeff must be provided and match X_prev length for GDP closure.")
    }
    weights <- va_coeff * X_prev
  } else {
    weights <- X_prev
  }

  X_R_prev <- sum(weights[idx_ren])
  X_N_prev <- sum(weights[idx_nren])
  X_O_prev <- sum(weights[idx_other])
  X_total_prev <- sum(weights)

  safe_g <- function(num, den, g_default) {
    if (!is.finite(den) || den == 0) return(g_default)
    num / den - 1
  }


  if (option == "residual-others") {
    # Closure identity:
    #   (1 + g) X_t^w = (1 + g_R) X_R,t^w + (1 + g_N) X_N,t^w + (1 + g_O) X_O,t^w,
    # where X_R,t^w, X_N,t^w, X_O,t^w are the weighted aggregates over the three blocks.
    #
    # In the residual-others case we impose
    #   g_R = eps_R,
    #   g_N = eps_N,
    # and solve the closure identity for g_O.
    g_R <- eps_R
    g_N <- eps_N
    g_O <- safe_g(
      (1 + g) * X_total_prev - (1 + g_R) * X_R_prev - (1 + g_N) * X_N_prev,
      X_O_prev,
      g
    )
  } else if (option == "fixed-others") {
    # Here we pin the non-energy block to aggregate growth:
    #   g_O = g,
    #   g_R = eps_R,
    # and solve for g_N from the same closure identity.
    # This pushes any inconsistency into the non-renewable energy-demand block.
    g_O <- g
    g_R <- eps_R
    g_N <- safe_g(
      (1 + g) * X_total_prev - (1 + g_R) * X_R_prev - (1 + g_O) * X_O_prev,
      X_N_prev,
      g
    )
  } else if (option == "uniform-demand") {
    # Here both energy blocks are forced to follow aggregate growth in final demand:
    #   g_R = g_N = g,
    # and the residual is absorbed by the non-energy block through g_O.
    g_R <- g
    g_N <- g
    g_O <- safe_g(
      (1 + g) * X_total_prev - (1 + g_R) * X_R_prev - (1 + g_N) * X_N_prev,
      X_O_prev,
      g
    )
  } else if (option == "eps-only") {
    # Fully uniform final-demand growth:
    #   g_R = g_N = g_O = g.
    # The transition then operates only through the endogenous energy-row controls in the IO solver.
    g_R <- g
    g_N <- g
    g_O <- g
  } else {
    stop("Unknown closure option: ", option)
  }

  g_vec <- rep(g_O, length(X_prev))
  g_vec[idx_ren] <- g_R
  g_vec[idx_nren] <- g_N

  list(g_R = g_R, g_N = g_N, g_O = g_O, g_vec = g_vec)
}

# Fixed-point solver: closure, epsprime row updates, and IO consistency solved together.
# At each iteration, we:
#  1) compute closure rates from current X (current shares, not base-year),
#  2) update final demand growth accordingly,
#  3) update Z rows for REN/NREN using epsprime (intermediate-use control),
#  4) recompute A, L, and X,
#  5) adjust epsprime so output quantities follow policy epsilons,
#  6) iterate until convergence in X and epsprime.
#
# Written in equations, iteration k performs:
#   F_t^(k)       = F_t-1 ⊙ (1 + g_vec^(k)),
#   z_Rj,t^(k)    = (1 + epsprime_R^(k)) z_Rj,t-1,
#   z_Nj,t^(k)    = (1 + epsprime_N^(k)) z_Nj,t-1,
#   A_t^(k)       = Z_t^(k) diag(x_t^(k-1))^-1,
#   x_t,raw^(k)   = (I - A_t^(k))^-1 F_t^(k),
#   x_t^(k)       = x_t^(k-1) + λ_x (x_t,raw^(k) - x_t^(k-1)).
#
# The energy-row controls are then updated from the mismatch between the target physical output of the
# two analytical energy sectors and the physical output implied by the monetary solution. With fixed
# average base-year prices p_R and p_N,
#   Q_R,target^(k) = (1 + eps_R) x_R,t^(k-1) / p_R,
#   Q_N,target^(k) = (1 + eps_N) x_N,t^(k-1) / p_N,
#   Q_R,curr^(k)   = x_R,t^(k) / p_R,
#   Q_N,curr^(k)   = x_N,t^(k) / p_N,
# and epsprime is moved part-way toward the implied correcting factor.
#
# Tradeoff:
#   this is a pragmatic fixed-point device, not a structural optimization problem. It keeps the IO model
#   close to the imposed physical transition path while retaining a transparent demand-driven closure, but
#   the result depends on damping parameters x_relax and epsprime_step.
solve_io_consistency <- function(Z_base, F_prev, x_init, diag_mat, option, eps_R, eps_N, g,
                                 idx_ren, idx_nren, p_out_ren, p_out_nren,
                                 va_coeff = NULL, target = "output",
                                 max_iter = 50, rel_io_tol = 1e-8,
                                 epsprime_step = 0.3, x_relax = 0.5) {
  x_iter <- as.numeric(x_init)
  F_curr <- as.numeric(F_prev)
  epsprime_R <- eps_R
  epsprime_N <- eps_N

  for (k in 1:max_iter) {
    closure <- compute_closure_growth(
      option = option,
      eps_R = eps_R,
      eps_N = eps_N,
      g = g,
      X_prev = x_iter,
      idx_ren = idx_ren,
      idx_nren = idx_nren,
      va_coeff = va_coeff,
      target = target
    )

    F_curr <- as.numeric(F_prev) * (1 + closure$g_vec)

    # Update the two energy rows only. All other rows are held at Z_base during the current-year iteration:
    #   z_Rj^(k) = (1 + epsprime_R^(k)) z_Rj,base,
    #   z_Nj^(k) = (1 + epsprime_N^(k)) z_Nj,base.
    #
    # This is the mechanism through which the monetary IO system is steered toward the imposed physical
    # renewable/non-renewable path.
    Z_curr <- Z_base
    Z_curr[idx_ren, ] <- pmax(0, Z_base[idx_ren, ] * (1 + epsprime_R))
    Z_curr[idx_nren, ] <- pmax(0, Z_base[idx_nren, ] * (1 + epsprime_N))

    x_safe <- x_iter
    x_safe[!is.finite(x_safe) | x_safe == 0] <- 1e-6

    A_curr <- sweep(Z_curr, 2, x_safe, FUN = "/")
    L_curr <- solve(diag_mat - A_curr)
    x_new_raw <- as.numeric(L_curr %*% F_curr)
    # Inertia on x_t to reduce oscillations in the non-linear fixed-point problem:
    #   x_t^(k) = x_t^(k-1) + λ_x (x_t,raw^(k) - x_t^(k-1)).
    x_new <- x_iter + x_relax * (x_new_raw - x_iter)

    # Translate the monetary output of the two analytical energy sectors into physical quantities using the
    # fixed base-year average output values per GWh. The target path is the scenario-imposed physical growth:
    #   Q_group,target^(k) = (1 + eps_group) Q_group,prev^(k-1).
    # The current path is
    #   Q_group,curr^(k) = x_group^(k) / p_group.
    # Their ratio defines how epsprime must move.
    Q_prev_R <- x_iter[idx_ren] / p_out_ren
    Q_prev_N <- x_iter[idx_nren] / p_out_nren
    Q_target_R <- Q_prev_R * (1 + eps_R)
    Q_target_N <- Q_prev_N * (1 + eps_N)

    Q_curr_R <- x_new[idx_ren] / p_out_ren
    Q_curr_N <- x_new[idx_nren] / p_out_nren

    Q_curr_R[!is.finite(Q_curr_R) | Q_curr_R == 0] <- 1e-6
    Q_curr_N[!is.finite(Q_curr_N) | Q_curr_N == 0] <- 1e-6

    scale_R <- Q_target_R / Q_curr_R
    scale_N <- Q_target_N / Q_curr_N

    epsprime_R_target <- (1 + epsprime_R) * scale_R - 1
    epsprime_N_target <- (1 + epsprime_N) * scale_N - 1

    # Damped update:
    #   epsprime_new = epsprime_old + λ_eps (epsprime_target - epsprime_old).
    #
    # Without this damping the IO-energy feedback can oscillate strongly, especially when the imposed
    # renewable/non-renewable shock is large relative to the base-year monetary structure.
    epsprime_R_new <- epsprime_R + epsprime_step * (epsprime_R_target - epsprime_R)
    epsprime_N_new <- epsprime_N + epsprime_step * (epsprime_N_target - epsprime_N)

    rel_x <- max(abs(x_new - x_iter) / pmax(1, abs(x_iter)), na.rm = TRUE)
    rel_eps <- max(
      abs(epsprime_R_new - epsprime_R) / pmax(1, abs(epsprime_R)),
      abs(epsprime_N_new - epsprime_N) / pmax(1, abs(epsprime_N)),
      na.rm = TRUE
    )
    # Algebraic IO residual of the current iterate:
    #   r^(k) = x_t^(k) - A_t^(k) x_t^(k) - F_t^(k).
    # This is reported only as a diagnostic; the stopping criterion is based jointly on x and epsprime.
    rel_io_resid <- mean(
      abs(x_new - as.numeric(A_curr %*% x_new) - F_curr) / pmax(1, abs(x_new)),
      na.rm = TRUE
    )

    if (max(rel_x, rel_eps, na.rm = TRUE) < rel_io_tol) {
      return(list(
        A = A_curr,
        L = L_curr,
        X = x_new,
        F = F_curr,
        Z = Z_curr,
        closure = closure,
        epsprime_R = epsprime_R_new,
        epsprime_N = epsprime_N_new,
        rel_io_resid = rel_io_resid,
        g_R = closure$g_R,
        g_N = closure$g_N,
        g_O = closure$g_O
      ))
    }

    x_iter <- x_new
    epsprime_R <- epsprime_R_new
    epsprime_N <- epsprime_N_new
  }

  rel_io_resid <- mean(
    abs(x_iter - as.numeric(A_curr %*% x_iter) - F_curr) / pmax(1, abs(x_iter)),
    na.rm = TRUE
  )

  list(
    A = A_curr,
    L = L_curr,
    X = x_iter,
    F = F_curr,
    Z = Z_curr,
    closure = closure,
    epsprime_R = epsprime_R,
    epsprime_N = epsprime_N,
    rel_io_resid = rel_io_resid,
    g_R = closure$g_R,
    g_N = closure$g_N,
    g_O = closure$g_O
  )
}
