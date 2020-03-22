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
  int N;               // number of polls
  int n_states;        // number of states 
  int n_options;       // number of candidates
  int state_id[N];     // state identifier
  matrix[N, n_options] y; // matrix of counts for each candidate in each poll
  real n[N];
  vector[N] days_out;  // days until election (for weighting)
  matrix<lower = 0, upper = 1>[n_states, n_options] priors; // prior rates
  real<lower = 0> decay_param;
}
transformed data {
  int<lower = 0> y_wt[N, n_options]; // wieghted counts in each poll
  matrix[N, n_options] sampling_error;
  int n_wt[N];            // weighted sample
  for(i in 1:N) {
    for(o in 1:n_options) {
      if(n[i] > 0) {
        sampling_error[i, o] = normal_rng(0, n[i]*(.5/sqrt(n[i])));
      } else {
        sampling_error[i, o] = 0;
      }
      y_wt[i, o] = real_to_int(
        exp(-days_out[i]/decay_param)*(y[i, o] + sampling_error[i, o])
        );
    }
    n_wt[i] = sum(y_wt[i, ]);
  } 
}
parameters {
  simplex[n_options] theta[n_states]; // simplex of simulated election proportions
  real<lower = 0> tau;      // variance in proportions
  real<lower = 0> nu;
}
model {
  for(o in 1:n_options) {
    for(i in 1:N) {
      y_wt[i, o] ~ binomial(n_wt[i], theta[state_id[i]][o]);  // binomial model
    }
  }
  for(o in 1:n_options) {
    for(s in 1:n_states) {
      theta[s][o] ~ student_t(nu, priors[s, o], tau); // prior on proportion
    }
  }
  tau ~ normal(0.2, 0.01); // prior on variance
  nu ~ gamma(1, 0.1); // Prior on df, from vetari
}

