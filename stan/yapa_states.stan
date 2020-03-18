// This model combines polls with prior information to produce
// distributions of vote share in US presidential elections

data {
  int N;               // number of polls
  int n_states;        // number of states 
  int n_options;       // number of candidates
  int state_id[N];     // state identifier
  int n[N];            // poll sample 
  int y[N, n_options]; // matrix of counts for each candidate in each poll
  matrix<lower = 0, upper = 1>[n_states, n_options] priors; // prior rates
}
parameters {
  simplex[n_options] theta[n_states]; // Simplex of simulated election proportions
  real<lower = 0> tau[n_states];      // Variance in proportions
}
model {
  for(i in 1:N) {
    for(o in 1:n_options) {
      y[i, o] ~ binomial(n[i], theta[state_id[i]][o]);  // Binomial model
    }
  }
  for(s in 1:n_states) {
    for(o in 1:n_options) {
      theta[s][o] ~ normal(priors[s, o], tau[s]); // prior on proportion
    }
  }
  tau ~ normal(0.2, 0.01); // prior on variance
}
