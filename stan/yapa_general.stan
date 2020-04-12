// This model is a moving average of poll results.

functions {
  // Helper to convert weighted counts from real to int for binomial model
  int real_to_int(real x) {
    int out = 0;
    while(out < round(x)) {
      out += 1;
    }
    return out;
  }
}
data {
  int N_ge;                     // Number of polls
  int n_options;                // Number of candidates
  matrix[N_ge, n_options] y_ge; // Total count for each candidate in each poll
  vector[N_ge] days_out_ge;     // Days from election in each poll
  real<lower = 0> decay_param;
}
transformed data {
  int<lower = 0> y_wt[N_ge, n_options]; // wieghted counts in each poll
  int n_wt[N_ge];                       // weighted sample
  for(i in 1:N_ge) {
    for(o in 1:n_options) {
      y_wt[i, o] = real_to_int(exp(-days_out_ge[i]/decay_param)*(y_ge[i, o]));
    }
    n_wt[i] = sum(y_wt[i, ]);
  } 
}
parameters {
  simplex[n_options] mu; 
}
model {
  for(o in 1:n_options) {
    for(i in 1:N_ge) {
      y_wt[i, o] ~ binomial(n_wt[i], mu[o]);  // binomial model
    }
  }
  mu[1] ~ normal(0.46, 0.1);
  mu[2] ~ normal(0.48, 0.1);
  mu[3] ~ normal(0.06, 0.1);
}
generated quantities {
  simplex[n_options] results;
  results = dirichlet_rng(500 * mu); // Simulate an election
}
