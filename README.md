# Replication in R of John Rust's 1987 paper "Optimal Replacement of GMC Bus Engines"

[See the notebook here.](https://rpubs.com/colleenobriant/rust1987)

My results come close to Rust's results in his original paper: I estimate maintenance costs with 90 mileage buckets as 2.78 and replacement costs at 10.16.

This replication *does*:

✅ Work fast (1.745) seconds to run the entire replication on my 4 year old laptop.
✅ Replicate the inner loop, switching between contraction iterations and newton-kantorovich iterations.
✅ Explain a little more math than Rust's NFXP Algorithm Manual.
✅ Use better abstractions (IMO) than Rust's gauss code, hopefully making it a little easier to follow.

This replication *does not (yet)*:

❌ Implement the outer BHHH maximum likelihood loop by hand. Instead, I make use of `maxLik::maxBHHH`.
❌ Calculate standard errors.
❌ Calculate implied demand for replacement investment.

Henningsen, Arne and Toomet, Ott (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics 26(3), 443-458. DOI 10.1007/s00180-010-0217-1.

Rust, J. (1987). Optimal Replacement of GMC Bus Engines: An Empirical Model of Harold Zurcher. Econometrica, 55(5), 999–1033. https://doi.org/10.2307/1911259
