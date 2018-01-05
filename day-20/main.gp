/* the problem can be simplified to finding when the sum of the divisors (i.e.
 * the sigma function) surpasses one tenth of the desired threshold. */
for(X=1,2900000,if(sigma(X)>2900000,print(X);quit))
