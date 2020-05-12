data {
  int N;
  int n_states;
  int n_options;
  matrix[N, n_options] bias;
  int state_id[N];
}
parameters {
  matrix[n_states, n_options] mu_bias;
  matrix<lower = 0>[n_states, n_options] tau_bias;
  real mu[n_options];
}
model {
  for(o in 1:n_options) {
    for(i in 1:N) {
      bias[i, o] ~ student_t(4, mu_bias[state_id[i], o], tau_bias[state_id[i], o]);
      mu_bias[state_id[i], o] ~ normal(mu[o], 0.1);
      tau_bias[state_id[i], o] ~ normal(0, 0.5);
    }
  }
  mu ~ normal(0, 0.05);
}
