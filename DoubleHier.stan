data{

    int<lower=1> N;
    int<lower=1> N_Study;
    real Grad_obs[N];
    int Study[N];
    real Grad_sd[N];
}

parameters{

    vector[N] Grad_est;         // Estimated gradients
    vector[N_Study] a;          // Study-specific intercept
    real<lower=0> lambda;       // Rate of decline of exp distribution of subset specific variances
    vector<lower=0>[N_Study] b; //Study-specific variance
    real mu_a;                  //Mean of population of study intercepts
    real<lower=0> sigma_a;      //SD of population of intercepts
}

model{

  vector[N] sigma;
  vector[N] mu;

    //Priors
    mu_a ~ normal( 0 , 0.5 );
    sigma_a ~ exponential( 5 );
    lambda ~ normal( 25 , 15 )T[0, ];

    //Mu and sigma for each study
    for ( i in 1:N ) {
        mu[i] = a[Study[i]];
    }

    for ( i in 1:N ) {
        sigma[i] = b[Study[i]];
    }

    //Study-specific means are distributed normally
    //Study-specific variances are distributed exponentially
    for ( j in 1:N_Study ){
      a[j] ~ normal( mu_a , sigma_a );
      b[j] ~ exponential( lambda );
    }

    // Observed gradients come from distribution of true gradients and error
    Grad_obs ~ normal( Grad_est , Grad_sd );

    //We model estimated gradients with Gaussian distribution with mean mu and sd sigma
    Grad_est ~ normal( mu , sigma );
}
