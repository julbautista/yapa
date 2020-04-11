// This model combines polls with prior information to produce
// distributions of vote share in US presidential elections

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
  int N;                  // number of polls
  int n_states;           // number of states 
  int n_options;          // number of candidates
  int state_id[N];        // state identifier
  matrix[N, n_options] y; // matrix of counts for each candidate in each poll
  vector[N] days_out;     // days until election (for weighting)
  matrix<lower = 0, upper = 1>[n_states, n_options] priors; // prior rates
  real<lower = 0> decay_param;
}
transformed data {
  int<lower = 0> y_wt[N, n_options]; // wieghted counts in each poll
  int n_wt[N];                       // weighted sample
  for(i in 1:N) {
    for(o in 1:n_options) {
      y_wt[i, o] = real_to_int(exp(-days_out[i]/decay_param)*(y[i, o]));
    }
    n_wt[i] = sum(y_wt[i, ]);
  } 
}
parameters {
  simplex[n_options] theta[n_states]; // simplex of simulated election proportions
  real<lower = 0> tau[n_states];      // variance in proportions
  real<lower = 0> nu;                 // df variable for student-t
}
model {
  for(o in 1:n_options) {
    for(i in 1:N) {
      y_wt[i, o] ~ binomial(n_wt[i], theta[state_id[i]][o]);  // binomial model
    }
  }
  for(o in 1:n_options) {
    for(s in 1:n_states) {
      theta[s][o] ~ student_t(nu, priors[s, o], tau[s]); // prior on proportion
    }
  }
  tau ~ normal(0.1, 0.01);
  nu ~ normal(2, 2);      // Prior on df, from vetari
}
generated quantities {
  simplex[n_options] results[n_states];
  vector[n_options] bias;
  for(o in 1:n_options) {
    bias[o] = normal_rng(100, 3); // Sample a bias
  }
  for(s in 1:n_states) {
    results[s] = dirichlet_rng(bias .* theta[s]); // Simulate an election
  }
}

