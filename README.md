# rust1987
Replication in R of John Rust's 1987 paper "Optimal Replacement of GMC Bus Engines"

[See the notebook here.](https://rpubs.com/colleenobriant/rust1987)

I implement the inner loop myself, switching between contraction iterations and
newton-kantorovich iterations. For the outer BHHH maximum likelihood loop, I use maxLik::maxBHHH.

Henningsen, Arne and Toomet, Ott (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics 26(3), 443-458. DOI 10.1007/s00180-010-0217-1.
