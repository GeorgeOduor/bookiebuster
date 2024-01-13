
library(googlesheets4)
df = data.frame(matrix(rnorm(20), nrow=10))
df
(ss <- gs4_create("Sample R", sheets = df,))
gs4_find("Sample R") %>%
  googledrive::drive_trash()
for (i in 1:10) {
  sheet_append(ss,data = df)
  Sys.sleep(10)
}
