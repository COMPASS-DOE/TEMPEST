# longterm_dat

This folder holds data downloaded from the continuous LI-8100 ("Stuart").

The data files can get quite large (for example, `SALT_20181207_LT.81x` downloaded 24 January 2019 was 230 MB!). Because GitHub limits file sizes, we first split such files into smaller subsets, e.g. `SALT_20181207_LT_a.81x`, `SALT_20181207_LT_b.81x`, etc.

Note that chamber 8 was miswired between 2018-08-31 and 2019-03-11
The temperature data is in V1 (see issue #82). Currently we test for this and fix in code:
```
T5 = if_else(T5 > 100 & Port == 8, V1, T5)
```
