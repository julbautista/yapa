// This model is a moving average of poll results.

data {
  int N_ge;                  // Number of weeks
  int n_options;             // Number of candidates
  int n_ge[N_ge];            // Total sample for each week
  int y_ge[N_ge, n_options]; // Total count for each candidate in each week
  int Q;
}
parameters {
  simplex[n_options] mu;     // 
  real<lower = 0> sigma;
  simplex[n_options] theta[N_ge];
  vector[Q] lag;
}
transformed parameters {
  matrix[N_ge, n_options] epsilon;
  for(o in 1:n_options) {
    for(t in 1:N_ge) {
      epsilon[t, o] = theta[t, o] - mu[o];
      for(q in 1:min(t-1, Q)) {
        epsilon[t, o] = epsilon[t, o] - lag[q]*epsilon[t-q, o];
      }
    }
  }
}
model {
  matrix[N_ge, n_options] eta;
  for(i in 1:N_ge) {
    for(o in 1:n_options) {
      y_ge[i, o] ~ binomial(n_ge[i], theta[i, o]);
      eta[i, o] = mu[o];
      for(q in 1:min(i-1, Q)) {
        eta[i, o] = eta[i, o] + lag[q]*epsilon[i-q, o];
      }
      theta[i, o] ~ normal(mu[o], sigma);
    }
  }
  mu[1] ~ normal(0.46, 0.05);
  mu[2] ~ normal(0.48, 0.05);
  mu[3] ~ normal(0.06, 0.02);
  lag ~ cauchy(0, 2);
  sigma ~ cauchy(0, 2);
}
