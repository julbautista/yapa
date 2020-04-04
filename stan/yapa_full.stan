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
  // General election polls
  int N_ge;
  int n_options;          // number of candidates
  int n_ge[N_ge];
  int y_ge[N_ge, n_options];
  // State polls
  int N;                  // number of polls
  int n_states;           // number of states 
  int state_id[N];        // state identifier
  matrix[N, n_options] y; // matrix of counts for each candidate in each poll
  vector[N] days_out;     // days until election (for weighting)
  matrix<lower = 0>[n_states, n_options] priors; // prior rates
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
  // General election moving average parameters
  simplex[n_options] mu_ge;
  real<lower = 0> sigma_ge;
  simplex[n_options] theta_ge[N_ge];
  vector[2] lag_ge;
  // State level model
  simplex[n_options] theta[n_states]; // simplex of simulated election proportions
  real<lower = 0> tau;                // variance in proportions
  real<lower = 0> nu;                 // df variable for student-t
}
transformed parameters {
  // General election MA lags
  matrix[N_ge, n_options] epsilon_ge;
  for(o in 1:n_options) {
    epsilon_ge[1, o] = theta_ge[1, o] - mu_ge[o];
    epsilon_ge[2, o] = theta_ge[2, o] - mu_ge[o] - lag_ge[1]*epsilon_ge[1, o];
    for(t in 3:N_ge) {
      epsilon_ge[t, o] = (theta_ge[t, o] - mu_ge[o] 
                          - lag_ge[1]*epsilon_ge[t-1, o] 
                          - lag_ge[2]*epsilon_ge[t-2, o]);
    }
  }
}
model {
  // General election MA model
  for(i in 1:N_ge) {
    for(o in 1:n_options) {
      y_ge[i, o] ~ binomial(n_ge[i], theta_ge[i, o]);
      if(i > 2) {
        theta_ge[i, o] ~ normal(mu_ge[o] 
                                + lag_ge[1]*epsilon_ge[i-1, o] 
                                + lag_ge[2]*epsilon_ge[i-2, o], sigma_ge);
        }
    }
  }
  mu_ge ~ normal(0.5, 0.25);
  lag_ge ~ normal(0, 1);
  sigma_ge ~ cauchy(0, 2);
  // State model
  for(o in 1:n_options) {
    for(i in 1:N) {
      y_wt[i, o] ~ binomial(n_wt[i], theta[state_id[i]][o]);  // binomial model
    }
  }
  for(o in 1:n_options) {
    for(s in 1:n_states) {
      theta[s][o] ~ student_t(nu, priors[s, o]*mu_ge[o], tau); // prior on proportion
    }
  }
  tau ~ normal(0.1, 0.01);
  nu ~ gamma(2, 0.1);      // Prior on df, from vetari
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
