#### Downloading herbarium FURB data from JBRJ IPT ####

file_path <- here::here("data", "raw-data")
link <- "https://ipt.jbrj.gov.br/jabot/archive.do?r=furb&v="
version <- "1.58" # Publication date: 2024-12-01
url <- paste0(link, version)

# Name to save
file_name <- "furb_herbaria.zip"
download.file(url = url, 
              destfile = file.path(file_path, file_name), 
              mode = "wb")

# Unzip the DwC
unzip(file.path(file_path, file_name),
      exdir = file_path)


# Herbarium data from JABOT had to be downloaded manually from
# https://furb.jbrj.gov.br -> Consultar (you have to be logged in) -> Exportar -> CSV (Padrão)