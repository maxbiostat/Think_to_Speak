data{
  int<lower = 0> K;
  vector[K] P;// participantes
  array[K] int<lower=0> x;
}
parameters{
  real alpha;
  vector[K] eta;
}
transformed parameters{
 vector[K] lambda = exp(alpha + eta);
}
model{
  alpha ~ normal(0, 5);
  eta ~ normal(0, 1);
  x ~ poisson(P .* lambda);
}
generated quantities{
 real lambda_avg = exp(alpha);
}
