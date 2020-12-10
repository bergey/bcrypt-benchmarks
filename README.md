The main goal is to benchmark `cryptonite` against a Haskell binding to an upstream implementation of `bcrypt`, probably in C.  A secondary goal is to compare the several `bcrypt` bindings, for fidelity and ease of use, rather than performance.

`NaCl` and `libsodium` do not include `bcrypt`, so direct comparison is not possible.  If we can figure out what parameters give similar strength, upgrading to `scrypt` or `argon2` is worth considering.

Libraries included:
    - [cryptonite](http://hackage.haskell.org/package/cryptonite)
    - [bcrypt](http://hackage.haskell.org/package/bcrypt)
    - [haskell-bcrypt](http://hackage.haskell.org/package/haskell-bcrypt)

Libraries considered but excluded:
    - [saltine](https://hackage.haskell.org/package/saltine)
    - [lithium](https://github.com/eth-r/lithium) (says not production-ready)
    - [libsodium](https://hackage.haskell.org/package/libsodium) (maybe later)
    - [NaCl](https://hackage.haskell.org/package/NaCl) (maybe later)
    - [crypto-sodium](https://hackage.haskell.org/package/crypto-sodium) (maybe later)

# Results

system: MacBook Pro, 2.3 GHz Intel i9 (I9-9880H)

```
benchmarking cost=4/cryptonite
time                 1.728 ms   (1.711 ms .. 1.747 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.716 ms   (1.710 ms .. 1.725 ms)
std dev              24.20 μs   (18.73 μs .. 33.45 μs)

benchmarking cost=4/bcrypt
time                 926.6 μs   (922.2 μs .. 933.0 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 950.9 μs   (942.4 μs .. 962.3 μs)
std dev              34.16 μs   (28.14 μs .. 42.90 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking cost=8/cryptonite
time                 25.42 ms   (25.21 ms .. 25.57 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 25.61 ms   (25.49 ms .. 25.80 ms)
std dev              310.6 μs   (172.8 μs .. 507.4 μs)

benchmarking cost=8/bcrypt
time                 12.99 ms   (12.85 ms .. 13.26 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 13.05 ms   (12.93 ms .. 13.24 ms)
std dev              355.9 μs   (238.7 μs .. 469.4 μs)

benchmarking cost=12/cryptonite
time                 447.8 ms   (435.0 ms .. 464.1 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 461.7 ms   (454.6 ms .. 465.9 ms)
std dev              6.869 ms   (3.482 ms .. 8.683 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking cost=12/bcrypt
time                 215.3 ms   (213.7 ms .. 217.5 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 217.2 ms   (215.9 ms .. 220.7 ms)
std dev              2.817 ms   (609.4 μs .. 3.907 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking cost=16/cryptonite
time                 7.886 s    (6.853 s .. 9.081 s)
                     0.997 R²   (0.990 R² .. 1.000 R²)
mean                 7.204 s    (6.847 s .. 7.543 s)
std dev              412.0 ms   (350.9 ms .. 431.6 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking cost=16/bcrypt
time                 3.753 s    (3.159 s .. 4.519 s)
                     0.993 R²   (0.992 R² .. 1.000 R²)
mean                 3.422 s    (3.294 s .. 3.597 s)
std dev              172.8 ms   (52.87 ms .. 233.1 ms)
variance introduced by outliers: 19% (moderately inflated)
```
