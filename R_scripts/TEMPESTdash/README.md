# README

To create a new Dropbox token, do
```
library(rdrop2)
token <- drop_auth(new_user = TRUE) # flush any existing token
saveRDS(token, "TEMPESTdash/droptoken.rds")
```
