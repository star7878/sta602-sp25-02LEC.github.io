p_eqn = function(r, K, dt = 0.01) {
  t_step = seq(0, 200, by = dt)
  N = length(t_step)
  P = numeric(length = N)
  P[1] = 1
  for(i in 1:(N-1)) {
    P[i+1] = P[i] + (r * P[i] * (1 - (P/K))) * dt
  }
  return(data.frame(t_step, P))
}

x = p_eqn(r = log(2)/20, K = 10^9)

x[which(x$t_step %in% c(20, 40, 60, 80)),]

growth = function(K, r, t) {
  K / (1 + K*exp(-r * t))
}

growth(K = 10^9, r = log(2)/20, t = c(20, 40, 60, 80))
