/* no simplification this time, just a linear search. */
for(X=1,29000000,s=vecsum(apply(n->n*11, select(d->X/d<=50, divisors(X))));if(s>29000000,print(X);print(s);quit))
/* $ time gp -q main2.gp
 * 705600
 * 29002446
 * Goodbye!
 * real 0m2.730s
 * user 0m2.656s
 * sys  0m0.026s
 * $
 */
