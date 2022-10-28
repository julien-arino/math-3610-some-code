par = list(r = 0.1, K = 100)

rhs_logistic = function(y, par) {
  OUT = par$r * y * (1 - y/par$K)
  return(OUT)
}

h = 1
t_f = 50
times = seq(h, t_f, by = h)

N_0 = 50
N = N_0

for (t in times) {
  tmp = N[length(N)]+h*rhs_logistic(N[length(N)]
                       , par)
  N = c(N, tmp)
}

N = N[1:(length(N)-1)]
plot(times,N)
