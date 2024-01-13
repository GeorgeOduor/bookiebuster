library(googlesheets4)
df = data.frame(matrix(rnorm(20), nrow=10))
options(
  gargle_oauth_cache = "./.secrets/",
  gargle_oauth_email = "george.wamaya@gmail.com"
  )
gs4_auth()
(ss <- gs4_create("Sample R", sheets = df,))

for (i in 1:10) {
  sheet_append(ss,data = df)
  Sys.sleep(10)
}
gs4_find("Sample R") %>%
  googledrive::drive_trash()