// This model is a moving average of poll results.

data {
  int N_ge;                  // Number of weeks
  int n_options;             // Number of candidates
  int n_ge[N_ge];            // Total sample for each week
  int y_ge[N_ge, n_options]; // Total count for each candidate in each week
}
parameters {
  simplex[n_options] mu;     // 
  real<lower = 0> sigma;
  simplex[n_options] theta[N_ge];
  vector[2] lag;
}
transformed parameters {
  matrix[N_ge, n_options] epsilon;
  for(o in 1:n_options) {
    epsilon[1, o] = theta[1, o] - mu[o];
    epsilon[2, o] = theta[2, o] - mu[o] - lag[1]*epsilon[1, o];
    for(t in 3:N_ge) {
      epsilon[t, o] = (theta[t, o] - mu[o]
                        - lag[1]*epsilon[t-1, o] 
                        - lag[2]*epsilon[t-2, o]);
    }
  }
}
model {
  for(i in 1:N_ge) {
    for(o in 1:n_options) {
      y_ge[i, o] ~ binomial(n_ge[i], theta[i, o]);
      if(i > 2) {
        theta[i, o] ~ normal(mu[o] + lag[1]*epsilon[i-1, o] + lag[2]*epsilon[i-2, o],
                             sigma);
        }
    }
  }
  mu[1] ~ normal(0.46, 0.05);
  mu[2] ~ normal(0.48, 0.05);
  mu[3] ~ normal(0.06, 0.02);
  lag ~ normal(1, 0.1);
  sigma ~ cauchy(0, 2);
}
