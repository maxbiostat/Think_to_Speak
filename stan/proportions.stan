data{
  int<lower = 0> K;
  array[K] int<lower=0> N;
  array[K] int<lower=0> y;
}
parameters{
  real theta;
  vector[K] delta;
}
transformed parameters{
 vector[K] p = inv_logit(theta + delta);
}
model{
  theta ~ normal(0, 5);
  delta ~ normal(0, 1);
  y ~ binomial(N, p);
}
generated quantities{
 real p_avg = inv_logit(theta);
}
