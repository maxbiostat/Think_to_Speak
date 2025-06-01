data{
  int<lower = 0> K; // number of aggregated languages
  vector[K] P;// participants
  array[K] int<lower=0> x;
  int<lower = 0> N1;
  int<lower = 0> N2;
  array[N1] int<lower=0> y1;
  array[N2] int<lower=0> y2;
}
parameters{
  real alpha;
  vector[K + 2] eta;
  vector[N1] epsilon_1;
  vector[N2] epsilon_2;
}
transformed parameters{
 vector[K + 2] lambda = exp(alpha + eta);
 vector[N1] gamma_1 = exp(alpha + eta[K + 1] + epsilon_1);
 vector[N2] gamma_2 = exp(alpha + eta[K + 2] + epsilon_2);
}
model{
  alpha ~ normal(0, 5);
  eta ~ normal(0, 1);
  epsilon_1 ~ normal(0, 1);
  epsilon_2 ~ normal(0, 1);
  x ~ poisson(P .* lambda[1:K]);
  y1 ~ poisson(gamma_1);
  y2 ~ poisson(gamma_2);
}
generated quantities{
 real lambda_avg = exp(alpha);
}
