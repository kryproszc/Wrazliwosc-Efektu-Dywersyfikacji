#include <Rcpp.h>
using namespace Rcpp;


Rcpp::List projectOnTwoDims(const NumericMatrix a,
                            const NumericMatrix b,
                            const NumericVector p,
                            const NumericVector f,
                            const NumericVector kern,
                            const NumericVector dims,
                            const NumericMatrix data) {
  
  // Intialise objects :
  int n_leaves = a.ncol();
  int dim = a.nrow();
  int n_obs = data.nrow();
  double prod;
  int n_edges1,n_edges2,new_n,k,l;
  Rcpp::NumericVector edges1(2*n_leaves);
  Rcpp::NumericVector edges2(2*n_leaves);
  
  // Compute edges :
  edges1[seq(0,n_leaves-1)] = a(dims(0)-1,_);
  edges1[seq(n_leaves,2*n_leaves-1)] = b(dims(0)-1,_);
  edges1 = sort_unique(edges1);
  
  edges2[seq(0,n_leaves-1)] = a(dims(1)-1,_);
  edges2[seq(n_leaves,2*n_leaves-1)] = b(dims(1)-1,_);
  edges2 = sort_unique(edges2);
  
  n_edges1 = edges1.length() - 1;
  n_edges2 = edges2.length() - 1;
  new_n = n_edges1*n_edges2;
  
  // Setup variables :
  Rcpp::NumericMatrix new_a(new_n,2);
  Rcpp::NumericMatrix new_b(new_n,2);
  Rcpp::NumericVector new_f(new_n);
  Rcpp::NumericVector new_p(new_n);
  Rcpp::NumericVector new_vols(new_n);
  new_p.fill(0.0);
  new_f.fill(0.0);
  
  for(int i =0; i<new_n;i++){
    // Construct new_a and new_b :
    k = i / n_edges2;
    l = i % n_edges2;
    new_a(i,0) = edges1[k];
    new_a(i,1) = edges2[l];
    new_b(i,0) = edges1[k+1];
    new_b(i,1) = edges2[l+1];
    
    // Construct vols :
    new_vols(i) = (new_b(i,0)-new_a(i,0))*(new_b(i,1)-new_a(i,1));
    
    // Construct f :
    for(int n=0; n < n_obs; n++){
      if((new_a(i,0) <= data(n,0))&&(data(n,0) < new_b(i,0))&&(new_a(i,1) <= data(n,1))&&(data(n,1) < new_b(i,1))){
        new_f(i) += 1.0/n_obs;
      }
    }
    
    // Construct p :
    for(int m=0; m< n_leaves; m++){
      prod = kern(m);
      if(prod != 0){
        for(int d = 0; d < dim; d++){
          if(d  == (dims(0)-1)){
            prod *= std::max(std::min(b(d,m),new_b(i,0))-std::max(a(d,m),new_a(i,0)),0.0);
          } else if(d == (dims(1)-1)){
            prod *= std::max(std::min(b(d,m),new_b(i,1))-std::max(a(d,m),new_a(i,1)),0.0);
          } else {
            prod *= b(d,m)-a(d,m);
          }
          if(prod == 0){
            break;
          }
        }
        new_p(i) += prod;
      }
    }
  }
  Rcpp::List L = List::create(Named("f") = new_f , _["p"] = new_p, _["a"] = new_a, _["b"] = new_b, _["vols"] = new_vols);
  return (L);
}

// [[Rcpp::export]]
double lossFunc(const NumericVector bp,
                const NumericMatrix bin_repr,
                const NumericMatrix z){
  
  int D = bin_repr.ncol();
  int d = bin_repr.nrow();
  int n = z.ncol();
  double n_pts, vol, loss=0;
  Rcpp::NumericVector min(d);
  Rcpp::NumericVector max(d);
  
  for (int n_box=0; n_box < D; n_box++){
    vol = 1.0;
    n_pts= 0.0;
    // Compute sides and volumes :
    for (int dim=0; dim < d; dim++){
      min(dim) = bp(dim)*bin_repr(dim,n_box);
      max(dim) = std::pow(bp(dim),1.0-bin_repr(dim,n_box));
      vol = vol*(max(dim) - min(dim));
    }
    if(vol > 0){
      // check if observations are inside the box :
      for(int n_obs=0; n_obs<n; n_obs++){
        if(is_true(all(min<=z(_,n_obs))) && is_true(all(z(_,n_obs)<max))){
          n_pts += 1.0;
        }
      }
      loss -=std::pow(n_pts/n,2)/vol;
    }
  }
  return(loss);
}


Rcpp::NumericVector cortMonteCarlo(const NumericMatrix z,
                                   const NumericMatrix min,
                                   const NumericMatrix max,
                                   const int N) {
  int d = z.nrow();
  int n = z.ncol();
  int D = min.ncol(); //  == 2^d
  double f_l;
  NumericVector lambda_k(D);
  NumericVector lambda_l(D);
  NumericVector f_k(D);
  LogicalMatrix core_checks(D,n);
  NumericMatrix z_boot(d,n);
  NumericVector observed_stat(d);
  NumericMatrix bootstraped_stat(N,d);
  NumericVector p_values(d);
  
  observed_stat.fill(0.0);
  bootstraped_stat.fill(0.0);
  //p_values.fill(0.0);
  
  for (int d_rem = 0; d_rem < d; d_rem++){
    
    f_k.fill(0.0);
    lambda_k.fill(1.0);
    lambda_l.fill(1.0);
    core_checks.fill(true);
    
    // Compute the statistic :
    for (int n_leave = 0; n_leave < D; n_leave++){
      f_l = 0.0;
      for (int dim = 0; dim < d; dim++){
        lambda_l(n_leave) *= max(dim,n_leave) - min(dim,n_leave);
      }
      lambda_k(n_leave) = lambda_l(n_leave) / (max(d_rem,n_leave) - min(d_rem,n_leave));
      
      for (int n_obs = 0; n_obs < n; n_obs++){
        for(int dim = 0; dim < d; dim++){
          if(d_rem != dim){
            core_checks(n_leave,n_obs) = core_checks(n_leave,n_obs) && (min(dim,n_leave) <= z(dim,n_obs)) && (z(dim,n_obs) < max(dim,n_leave));
          }
        }
        if(core_checks(n_leave,n_obs)){
          f_k(n_leave) += 1.0/n;
        }
        if(core_checks(n_leave,n_obs) & (min(d_rem,n_leave) <= z(d_rem,n_obs)) && (z(d_rem,n_obs) < max(d_rem,n_leave))){
          f_l += 1.0/n;
        }
      }
      observed_stat(d_rem) += (f_l*f_l)/lambda_l(n_leave) - 2 * (f_l*f_k(n_leave))/lambda_k(n_leave);
    }
    
    // Now bootstrap the same thing :
    z_boot = Rcpp::clone(z);
    for (int n_boot = 0; n_boot < N; n_boot++){
      z_boot(d_rem,_) = runif(n); // The bootstrap is here.
      for (int n_leave = 0; n_leave < D; n_leave++){
        f_l = 0.0;
        for (int n_obs = 0; n_obs < n; n_obs++){
          if(core_checks(n_leave,n_obs) && (min(d_rem,n_leave) <= z_boot(d_rem,n_obs)) && (z_boot(d_rem,n_obs) < max(d_rem,n_leave))){
            f_l += 1.0/n;
          }
        }
        bootstraped_stat(n_boot,d_rem) += (f_l*f_l)/lambda_l(n_leave) - 2 * (f_l*f_k(n_leave))/lambda_k(n_leave);
      }
    }
  }
  for(int d_rem =0; d_rem < d; d_rem++){
    p_values(d_rem) = mean(observed_stat(d_rem) <= bootstraped_stat(_,d_rem));
  }
  return(p_values);
}
















